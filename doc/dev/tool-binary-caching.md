# Tool Binary Caching for Dune

This document describes a proposed extension to Dune's caching system that stores
and reuses dev tool binaries (ocamlformat, odoc, merlin, etc.) across workspaces.
This addresses the problem of tools being rebuilt per-workspace even when they
are identical.

## Problem Statement

Dune currently rebuilds dev tools from source for each workspace, even when:
- The tool version is the same
- The dependencies are identical
- The OCaml compiler version matches (for compiler-dependent tools)

The current tool installation path is `_build/default/.dev-tool/<package>/target/`
which is workspace-local. There is an existing toolchain cache at
`~/.cache/dune/toolchains/` but it only supports OCaml compilers, not dev tools.

**Consequence**: Running `dune fmt` in a new workspace requires building
ocamlformat from scratch, even if an identical binary exists in another workspace.

## Comparison with Other Ecosystems

| Feature | npm/npx | Cargo | dune (current) | dune (proposed) |
|---------|---------|-------|----------------|-----------------|
| Fixed tool list | No | No | Yes (10 tools) | No |
| Install any pkg | `npm i -D <pkg>` | `cargo install` | No | `dune tools install` |
| Run any pkg | `npx <pkg>` | N/A | No | `dune tools exec` |
| Per-project config | package.json | Cargo.toml | .dev-tools.locks/ | dune-project |
| Global cache | ~/.npm/ | ~/.cargo/ | N/A | ~/.cache/dune/toolchains/ |

npm's approach is particularly relevant: packages are installed to a central cache
and accessed via `node_modules/.bin/` symlinks or `npx`. Similarly, Cargo installs
binaries to `~/.cargo/bin/` with `cargo install`.

## Current Architecture

### Dev Tool Definitions

Dev tools are defined in `src/dune_pkg/dev_tool.ml` as a hardcoded enum:

```ocaml
type t =
  | Ocamlformat | Odoc | Ocamllsp | Utop | Ocamlearlybird
  | Odig | Opam_publish | Dune_release | Ocaml_index | Merlin
```

Each tool has metadata including:
- `package_name`: The opam package (e.g., `Ocamllsp` -> `"ocaml-lsp-server"`)
- `exe_name`: The binary name (e.g., `"ocamllsp"`)
- `needs_to_build_with_same_compiler_as_project`: Whether tool must match OCaml version

### Compiler-Dependent Classification

From `src/dune_pkg/dev_tool.ml`:

```ocaml
let needs_to_build_with_same_compiler_as_project = function
  | Ocamlformat -> false          (* compiler-independent *)
  | Ocamlearlybird -> false
  | Opam_publish -> false
  | Dune_release -> false
  | Utop | Odoc | Ocamllsp | Ocaml_index | Odig | Merlin -> true
```

This classification is key for cache key design: compiler-independent tools can
be shared across OCaml versions, while compiler-dependent tools cannot.

### Existing Toolchain Cache

The toolchain cache in `src/dune_rules/pkg_toolchain.ml` already implements
package-level caching for OCaml compilers:

```ocaml
let base_dir =
  lazy (Path.relative (Lazy.force Dune_util.cache_root_dir) "toolchains")

let pkg_dir (pkg : Dune_pkg.Lock.Pkg.t) =
  let pkg_digest = Dune_digest.Feed.compute_digest
    Lock_dir.Pkg.digest_feed (Lock_dir.Pkg.remove_locs pkg) in
  let dir_name = sprintf "%s.%s-%s"
    (Package.Name.to_string pkg.info.name)
    (Package_version.to_string pkg.info.version)
    (Dune_digest.to_string pkg_digest) in
  Path.Outside_build_dir.relative (base_dir ()) dir_name
```

This pattern of `name.version-digest` can be extended to dev tools.

## Proposed Design

### Extend Toolchains Cache to Dev Tools

Generalize `~/.cache/dune/toolchains/` to store all tools, not just compilers:

```
~/.cache/dune/toolchains/
├── ocaml.5.2.0-<digest>/                  # Compiler (existing)
│   └── target/{bin,lib}/
├── ocamlformat.0.26.2-<pkg_digest>/       # Compiler-independent tool
│   └── target/bin/ocamlformat
├── odoc.2.4.0-5.2.0-<pkg_digest>/         # Compiler-dependent tool
│   └── target/bin/odoc
└── my-tool.1.0-5.2.0-<pkg_digest>/        # Custom tool
    └── target/bin/my-tool
```

### Cache Key Design

Use differentiated cache keys based on compiler dependency:

**Compiler-independent tools:**
```
{package_name}.{version}-{pkg_digest}
```

**Compiler-dependent tools:**
```
{package_name}.{version}-{ocaml_version}-{pkg_digest}
```

Where `pkg_digest` is computed from the lock file contents (dependencies, build
commands, etc.) as currently done for compilers in `pkg_toolchain.ml`.

### Tool Build Flow

**Current flow:**
1. Lock tool deps -> `.dev-tools.locks/<tool>/`
2. Build tool -> `_build/default/.dev-tool/<tool>/target/`
3. Run from `_build/`

**Proposed flow:**
1. Lock tool deps -> `.dev-tools.locks/<tool>/`
2. Compute cache key from lock + (optionally) OCaml version
3. Check `~/.cache/dune/toolchains/<cache_key>/`
4. **Cache hit:** Use directly from cache
5. **Cache miss:** Build directly to cache location
6. Run from cache

### PATH and Library Integration

Tools run directly from cache without copying to `_build/`:

```ocaml
(* Compute paths from cache *)
let cached_tool_prefix tool pkg =
  Pkg_toolchain.installation_prefix pkg

let cached_tool_bin_dir tool pkg =
  Path.Outside_build_dir.relative (cached_tool_prefix tool pkg) "bin"

let cached_tool_lib_dir tool pkg =
  Path.Outside_build_dir.relative (cached_tool_prefix tool pkg) "lib"
```

The `dune tools env` command exports:
- `PATH`: All cached tool bin directories
- `OCAMLPATH`: All cached tool lib directories (for runtime plugins)

### Generalize to Any Package

**Current limitation:** Hardcoded enum of 10 tools in `dev_tool.ml`

**Proposed extension:** Support any opam package as a tool:

```bash
dune tools install <any-opam-pkg>     # Install any package
dune tools exec <pkg> -- <args>       # Run any installed tool
dune tools env                        # Export PATH for all tools
```

### Tool Configuration in dune-project

Add optional `(tools ...)` stanza for per-project tool configuration:

```lisp
(tools
 (ocamlformat (version 0.26.2))           ; specific version
 (odoc)                                    ; latest compatible
 (my-tool (package my-tool-pkg))          ; custom package name
 (my-fork (source (git "https://...")))   ; patched version
 (fast-tool (compiler-independent true))  ; override default
)
```

This enables:
- Version pinning beyond lock file
- Custom tools not in the hardcoded list
- Patched/forked tool versions
- Override compiler dependency classification

## Implementation Considerations

### Relocatability

Most OCaml tools should be relocatable (no hardcoded paths in binaries). However,
some tools may have issues running from arbitrary locations. The implementation
should:

1. Default to running from cache
2. Document any tools with known relocatability issues
3. Provide fallback mechanism if needed

### Concurrent Builds

Multiple processes may attempt to populate the same cache entry. Use the atomic
hardlink pattern from `src/dune_pkg/pkg_cache.ml`:

1. Build to temporary location
2. Atomic rename to final cache location
3. Handle EEXIST gracefully (another process won)

### Cache Cleanup

Options for managing cache size:
1. Reuse existing trimmer logic from `src/dune_cache/trimmer.ml`
2. Add tool-specific cleanup command: `dune cache trim --tools`
3. Track usage timestamps for LRU eviction

### Unknown Tool Classification

For tools not in the hardcoded list, default to compiler-dependent (safer).
Allow override via `(compiler-independent true)` in dune-project.

## Files to Modify

| File | Changes |
|------|---------|
| `src/dune_rules/pkg_toolchain.ml{,i}` | Generalize to support dev tools |
| `src/dune_rules/pkg_dev_tool.ml{,i}` | Add cache lookup/store functions |
| `src/dune_pkg/dev_tool.ml{,i}` | Add cache key computation |
| `src/dune_lang/stanzas/dune_project.ml{,i}` | Parse `(tools ...)` stanza |
| `bin/tools/tools_common.ml{,i}` | Integrate cache, update PATH logic |
| `bin/lock_dev_tool.ml` | Check cache before building |

## Implementation Plan

**Phase 1: Cache Infrastructure**
- Extend `pkg_toolchain.ml` to support dev tools
- Add cache key computation with compiler dependency handling
- Implement cache lookup/store in `pkg_dev_tool.ml`

**Phase 2: Build Flow Integration**
- Modify `tools_common.ml` to check cache before building
- Update `dune tools env/exec/which` to use cache paths
- Build directly to cache location on miss

**Phase 3: Extensibility**
- Add `(tools ...)` stanza parsing
- Support arbitrary opam packages
- Allow compiler-dependency override

**Phase 4: Testing**
- Cache hit/miss scenarios
- Cross-workspace sharing
- Compiler-dependent vs independent tools
- Custom tool installation

## Open Questions

1. **Cache key stability:** Should we include more/less in the digest? Currently
   using lock file contents like `pkg_toolchain.ml`.

2. **Tool discovery:** How should `dune tools env` discover which tools are
   installed? Scan cache directory? Track in a manifest?

3. **Version conflicts:** What happens if different projects request different
   versions of the same tool? Keep both cached?

4. **Binary distribution:** Could this integrate with a binary package repository
   to download pre-built tools instead of building from source?

