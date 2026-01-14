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
├── .pkgs/tools-ocamlformat/          # Package builds (context = tools-ocamlformat)
│   ├── ocamlformat/                  # One directory per package (no version in path)
│   │   ├── source/                   # Fetched & extracted source
│   │   ├── target/
│   │   │   ├── cookie                # Build output tracking
│   │   │   └── ocamlformat.install   # Copied from source
│   │   └── installed                 # Files copied to shared prefix
│   └── base/
│       ├── source/
│       ├── target/
│       │   ├── cookie
│       │   └── base.install
│       └── installed
├── .locks/tools-ocamlformat/         # Lock directory (auto-generated)
└── install/
    ├── tools-ocamlformat/            # Shared install prefix (OPAM_SWITCH_PREFIX)
    │   ├── bin/ocamlformat           # All packages install here
    │   └── lib/...
    └── default/
        └── bin/ocamlformat           # Promoted from tools-ocamlformat
```

The directory uses package name only (no version). The `installed` manifest
persists across version changes, enabling automatic cleanup when upgrading or
downgrading packages - old files are removed before installing new ones.

**Package Installation:**
All packages install to the shared prefix `_build/install/<ctx>/` (like opam's
OPAM_SWITCH_PREFIX). This ensures packages can find their dependencies.

**Tracking Installed Files:**
To know which files each package installed:
1. Package build generates `<pkg>.install` in source directory
2. Copy `<pkg>.install` to `target/<pkg>.install`
3. For promotion, read `target/<pkg>.install` to find installed files

**Requirement:** Dev tools must generate `.install` files. This is standard for
well-behaved OCaml packages (ocamlformat, odoc, ocamllsp all do).

**`dune tools install ocamlformat` flow:**
1. Determine version (from `.ocamlformat` config or CLI flag)
2. Generate lock file in `_build/.locks/tools-ocamlformat/` (needed for cache key)
3. Compute cache key from lock file (e.g., `ocamlformat.0.26.2-a1b2c3d4`)
4. Check global cache `~/.cache/dune/index/ocamlformat.0.26.2-a1b2c3d4/`
5. **Cache hit**: Promote directly from cache to `_build/install/default/bin/`
6. **Cache miss**:
   - Build all packages in `_build/.pkgs/tools-ocamlformat/`
   - Packages install to `_build/install/tools-ocamlformat/`
   - Read `target/ocamlformat.install` to find tool's installed files
   - Promote only the tool's files to `_build/install/default/`
   - Copy to global cache (`~/.cache/dune/index/`) for future use

**Key design points:**
- Full isolation: each tool has its own context (different deps, OCaml version)
- Shared install prefix: packages install to `_build/install/<ctx>/` (opam compatible)
- .install file tracking: know exactly which files belong to each package
- Targeted promotion: only the tool package's files are promoted, not deps
- Binary only (for now): only `bin/` is promoted to default context
- Same infrastructure: tools use the same build system as regular packages

### Two-Level Cache Model

```
~/.cache/dune/
├── db/files/v5/<hash>/...                        # Content-addressed cache
└── index/                                        # Secondary index (symlinks)
    ├── ocamlformat.0.26.2-a1b2c3d4/bin/ocamlformat
    ├── odoc.2.4.0-f7e8d9c0/bin/odoc              # compiler-dependent
    └── ocamllsp.1.18.0-b2c3d4e5/bin/ocamllsp

_build/install/default/bin/                        # Project install dir
└── ocamlformat -> ~/.cache/dune/index/ocamlformat.0.26.2-a1b2c3d4/bin/ocamlformat
```

After promotion, binaries are cached in `~/.cache/dune/index/` and replaced with
symlinks for cross-project sharing. The index entries point to the content-addressed
store (`~/.cache/dune/db/files/v5/`).

**Benefits:**
- No workspace pollution (everything in `_build/`)
- `dune clean` removes symlinks but keeps cached binaries (fast reinstall)
- Cross-project sharing via global cache
- Full isolation via per-tool contexts
- Deduplication via content-addressed storage

### Cache Key Design

Cache keys use an 8-character digest suffix to ensure correctness while enabling sharing:

**Compiler-independent tools:**
```
{package_name}.{version}-{checksum8}
```
Where `checksum8` is the first 8 characters of the source checksum hash from the
opam URL. This enables cross-project sharing regardless of which OCaml version
the project uses.

**Compiler-dependent tools:**
```
{package_name}.{version}-{build_id8}
```
Where `build_id8` is from the dev tool's recursive `build_id` (Merkle hash of
opam content + all deps' build_ids). This automatically includes the compiler
as a dependency, ensuring projects with different compilers get separate cache
entries while projects with the same compiler share.

**Fallback:** If source checksum is unavailable (e.g., git sources), fall back
to using `build_id8` for compiler-independent tools as well. This reduces sharing
but maintains correctness.

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
2. Generate lock dir at `_build/.locks/tools-ocamlformat/` (needed for cache key)
3. Compute cache key from lock file: `ocamlformat.0.26.2-<checksum8>`
4. Check global cache `~/.cache/dune/index/ocamlformat.0.26.2-<checksum8>/`
5. **Cache hit**: Create symlink in `_build/install/default/bin/`, run
6. **Cache miss**:
   - Build packages in `_build/.pkgs/tools-ocamlformat/`
   - Promote `_build/.pkgs/tools-ocamlformat/ocamlformat/target/bin/*`
     to `_build/install/default/bin/`
   - Copy binary to global cache (`~/.cache/dune/index/`)
   - Replace with symlink pointing to cache
7. Run tool from `_build/install/default/bin/`

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

1. Compute expected cache key from lock file
2. Check `https://cache.dune.build/tools/{name}/{key}/{platform}.tar.gz`
3. Verify checksum
4. Extract to `~/.cache/dune/index/{name}.{key}/`
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
| `src/dune_rules/pkg_cache.ml` | Unified cache for all package types |
| `src/dune_rules/pkg_cache.mli` | Cache key computation, index management |
| `bin/tools/tools_common.ml` | Auto-install flow, cache lookup, symlink creation |

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
