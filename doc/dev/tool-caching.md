# Tool Caching

**Related documents:**
- [cache.md](cache.md) - Build artifact caching
- [display.md](display.md) - Console output and progress

This document describes two related caching mechanisms for tools:
1. **Dev tool binary caching** - Caching built tools like ocamlformat, odoc
2. **Content-addressed rule digests** - Better cache hits via tool signatures

## Part 1: Dev Tool Binary Caching

### Problem Statement

Dune currently rebuilds dev tools from source for each workspace, even when:
- The tool version is the same
- The OCaml compiler version matches (for compiler-dependent tools)

**Consequence**: Running `dune fmt` in a new workspace requires building
ocamlformat from scratch, even if an identical binary exists elsewhere.

**User feedback** ([discuss.ocaml.org](https://discuss.ocaml.org/t/installing-developer-tools-with-dune/15612)):
- `dune clean` removes tools, requiring slow rebuild
- Slow builds when no binary available
- Config complexity for editor integration
- Users like: project-local versions, automatic version selection

### Design Principles

1. **No workspace pollution** - no files outside `_build/`
2. **Cross-workspace sharing** - identical tools built once
3. **Fast reinstall** - `dune clean` shouldn't require rebuild
4. **Transparent installation** - like npx, auto-install on demand
5. **Relocatable binaries** - tools run from any location

### Dev Tool Directory Structure

Dev tools use the same build infrastructure as regular packages, with their own
isolated context. The only difference is a promotion step at the end.

```
_build/
├── tools-ocamlformat/              # Build context (isolated from project)
├── pkg/tools-ocamlformat/          # Package builds
│   ├── ocamlformat.0.26.2/
│   │   ├── source/                 # Extracted source
│   │   └── target/                 # Package's install outputs
│   │       └── bin/ocamlformat     # Tool binary
│   └── base.0.16.0/                # Dependencies
│       ├── source/
│       └── target/
├── lock/tools-ocamlformat/         # Lock directory (auto-generated)
└── install/
    ├── tools-ocamlformat/          # Shared install (deps find each other)
    │   ├── bin/
    │   └── lib/
    └── default/
        └── bin/ocamlformat         # Promoted from tool's target/bin/
```

**Package Installation (dual write):**
Packages install their outputs to BOTH locations:
1. `_build/pkg/<ctx>/<pkg>/target/{bin,lib,...}` - per-package outputs
2. `_build/install/<ctx>/` - shared prefix (so dependencies can find each other)

**`dune tools install ocamlformat` flow:**
1. Determine version (from `.ocamlformat` config or CLI flag)
2. Check global cache `~/.cache/dune/tools/ocamlformat.0.26.2/`
3. **Cache hit**: Promote directly from cache to `_build/install/default/bin/`
4. **Cache miss**:
   - Generate lock file in `_build/lock/tools-ocamlformat/`
   - Build all packages in `_build/pkg/tools-ocamlformat/`
   - Promote tool's binaries from `target/bin/` to `_build/install/default/bin/`
   - Copy to global cache for future use

**Key design points:**
- Full isolation: each tool has its own context (different deps, OCaml version)
- Targeted promotion: only the tool package's `target/bin/` is promoted, not deps
- Binary only (for now): only `bin/` is promoted to default context
- Same infrastructure: tools use the same build system as regular packages

### Two-Level Cache Model

```
~/.cache/dune/tools/                              # Global cache (shared)
├── ocamlformat.0.26.2/bin/ocamlformat
├── odoc.2.4.0-5.2.0/bin/odoc                     # compiler-dependent
└── ocamllsp.1.18.0-5.2.0/bin/ocamllsp

_build/install/default/bin/                        # Project install dir
└── ocamlformat -> ~/.cache/dune/tools/ocamlformat.0.26.2/bin/ocamlformat
```

After promotion, binaries can optionally be cached globally and replaced with
symlinks for cross-project sharing.

**Benefits:**
- No workspace pollution (everything in `_build/`)
- `dune clean` removes symlinks but keeps cached binaries (fast reinstall)
- Cross-project sharing via global cache
- Full isolation via per-tool contexts

### Cache Key Design

Simple keys without digest complexity:

**Compiler-independent tools:**
```
{package_name}.{version}
```

**Compiler-dependent tools:**
```
{package_name}.{version}-{ocaml_version}
```

If the opam package definition changes, the version should change. Trust semver.

### Compiler Dependency Classification

| Tool | Compiler-Dependent | Reason |
|------|-------------------|--------|
| ocamlformat | No | Pure text processing |
| ocamlearlybird | No | Debugger protocol |
| opam_publish | No | HTTP/git operations |
| dune_release | No | Release automation |
| odoc | Yes | Parses `.cmt` files |
| ocamllsp | Yes | Analyzes typed AST |
| merlin | Yes | Compiler integration |
| utop | Yes | Links with compiler |

### Auto-Install Flow

When user runs `dune fmt`:

1. Check `_build/install/default/bin/ocamlformat` → use if exists
2. Parse `.ocamlformat` for version (e.g., `version = 0.26.2`)
3. Check global cache `~/.cache/dune/tools/ocamlformat.0.26.2/`
4. **Cache hit**: Create symlink in `_build/install/default/bin/`, run
5. **Cache miss**:
   - Generate lock dir at `_build/lock/tools-ocamlformat/`
   - Build packages in `_build/pkg/tools-ocamlformat/`
   - Promote `_build/pkg/tools-ocamlformat/ocamlformat.0.26.2/target/bin/*`
     to `_build/install/default/bin/`
   - Copy binary to global cache
   - Replace with symlink pointing to cache
6. Run tool from `_build/install/default/bin/`

**No files outside `_build/` in user's repo.**

### Relocatable Binaries

OCaml binaries traditionally embed hardcoded paths. For caching to work,
tools must be relocatable.

[David Allsopp's work](https://discuss.ocaml.org/t/volunteers-to-review-the-relocatable-ocaml-work/16667)
enables relocatable OCaml:

```bash
./configure --with-relative-libdir=../lib/ocaml
```

Tools should be built with:
- `--with-relative-libdir` for OCaml compiler
- Relative paths for findlib/ocamlfind configuration
- No hardcoded `OPAM_SWITCH_PREFIX` in binaries

| Tool | Relocatable | Notes |
|------|-------------|-------|
| ocamlformat | Yes | No runtime dependencies |
| odoc | Yes | Self-contained |
| ocamllsp | Mostly | Needs ocamlformat in PATH |
| merlin | Mostly | Needs compiler libs |

### Commands

```bash
dune tools install <pkg>           # Install to global cache + link
dune tools install ocamlformat.0.26.2  # specific version
dune tools exec <pkg> -- <args>    # Run, auto-install if needed
dune tools env                     # Export PATH for all project tools
dune tools which <pkg>             # Show tool location
```

---

## Part 2: Content-Addressed Rule Digests

### Problem Statement

Dune's cache currently invalidates when tools are at different paths, even if
the tools are identical. This is because rule digests include tool **paths**
rather than tool **content**.

**Consequence**: The same compiler binary at `/home/user1/.opam/default/bin/ocamlopt`
vs `/home/user2/.opam/default/bin/ocamlopt` produces different rule digests,
causing cache misses even though the compilers are identical.

### Proposed Solution

Replace path-based tool identification with content-based "tool signatures" that
capture the semantic identity of a tool.

### Tool Signatures

A tool signature captures the semantic identity of a tool without depending on
its filesystem path:

```ocaml
type t = {
  version: string;                (* Tool version string *)
  config_digest: Digest.t option; (* Normalized config output for OCaml tools *)
}
```

**Key insight**: We hash version + normalized config rather than the full binary.
This is:
- Faster (no need to read multi-MB binaries)
- More stable (rebuild timestamps don't affect digest)
- Sufficient for cache correctness (same version + config = same behavior)

For OCaml tools, the config digest is computed from `ocamlc -config` output with
absolute paths replaced by canonical placeholders:
- `standard_library: /path/to/lib` -> `standard_library: __STDLIB__`
- C compiler paths -> `__CC__`

### On-Disk Caching

Tool signatures are cached on disk at `~/.cache/dune/tool-signatures/` to avoid
recomputing them on every build. The cache is keyed by `(tool_path, mtime)` for
fast invalidation when tools change.

### Action Normalization

A new function `Action.for_digest` parallels the existing `for_shell` but
produces a content-addressed representation:

```ocaml
let for_digest t ~tool_signature_of_path =
  ...
  ~f_program:(fun ~dir x ->
    match x with
    | Ok p -> Tool_signature.to_string (tool_signature_of_path p)
    | Error e -> e.program)
```

### Rule Digest Changes

The rule digest computation changes to use the normalized action:

```ocaml
let compute_rule_digest ... ~tool_signatures =
  let action_for_digest = Action.for_digest action ~tool_signatures in
  let trace = (
    rule_digest_version + 1,  (* Bump from 23 to 24 *)
    sandbox_mode,
    Dep.Facts.digest facts ~env,
    target_paths,
    action_for_digest,  (* Content-addressed *)
    ...
  )
```

---

## Binary Repository

To avoid building from source, dune can download pre-built binaries.

### Repository Structure

```
https://cache.dune.build/tools/
├── ocamlformat/
│   ├── 0.26.2/
│   │   ├── linux-x86_64.tar.gz
│   │   ├── darwin-arm64.tar.gz
│   │   └── checksums.txt
├── odoc/
│   ├── 2.4.0-ocaml-5.2.0/
│   │   ├── linux-x86_64.tar.gz
│   │   └── ...
```

### Download Flow

1. Compute expected cache key
2. Check `https://cache.dune.build/tools/{name}/{key}/{platform}.tar.gz`
3. Verify checksum
4. Extract to `~/.cache/dune/tools/{name}.{key}/`
5. Fall back to source build if binary unavailable

### Security

- All binaries signed with checksums
- HTTPS only
- Optional GPG signatures for paranoid users

---

## Implementation

### Files to Modify

**Dev tool caching:**
| File | Changes |
|------|---------|
| `src/dune_rules/pkg_toolchain.ml` | Generalize to support dev tools |
| `src/dune_rules/pkg_dev_tool.ml` | Cache lookup, symlink creation |
| `src/dune_pkg/dev_tool.ml` | Simplified cache key computation |
| `bin/tools/tools_common.ml` | Auto-install flow, binary download |

**Content-addressed digests:**
| File | Changes |
|------|---------|
| `src/dune_engine/build_system.ml` | Rule digest computation |
| `src/dune_engine/action.ml` | Add `for_digest` function |
| `src/dune_rules/ocaml_toolchain.ml` | Add tool signatures |
| `src/ocaml-config/ocaml_config.ml` | Config normalization |

**New files:**
- `src/dune_engine/tool_signature.ml{,i}`

---

## Open Questions

1. **Non-OCaml tools**: How should we compute signatures for tools like `gcc`
   that don't have a `-config` option? Version string only?

2. **Findlib paths**: Library resolution depends on installation paths. Should
   we include content digests of META files?

3. **Sandboxing interaction**: How does this work with sandboxed builds where
   paths are rewritten?
