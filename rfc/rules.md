# RFC: Unified Package Rules

## Principle

**One build flow, one cache, one secondary index.**

All packages use the same pkg_rules build flow. The only variations are:
1. **Cache key formula** - What identifies a cached build
2. **Context** - Where the package builds
3. **OCAMLPATH** - Set only for opam-mode packages

## Package Types

| Type | Source | Context | Cached | Cache Key |
|------|--------|---------|--------|-----------|
| **Workspace** | Local dune project | `default` | No | — |
| **Vendored (dune)** | `duniverse/` with dune files | `default` | No | — |
| **Vendored (opam)** | `duniverse/` with opam file | `default` | No | — |
| **Locked** | `dune.lock` → `_build/.pkgs/` | `default` | Yes | `<name>.<ver>-<bid8>` |
| **Dev tool (compiler-independent)** | Auto-solved | `tools-<name>` | Yes | `<name>.<ver>-<checksum8>` |
| **Dev tool (compiler-dependent)** | Auto-solved | `tools-<name>` | Yes | `<name>.<ver>-<bid8>` |
| **Toolchain** | `dune.lock` (compiler pkg) | `default` | Yes | `<name>.<ver>-<bid8>` |

Where:
- `<bid8>` = 8-char build_id (Merkle hash of opam content + deps' build_ids)
- `<checksum8>` = 8-char hash of source checksum from opam url field

**Note:** Vendored packages are never cached because users may edit them.

## Cache Architecture

Reuse the existing content-addressed build cache. Add a secondary index for
cross-project sharing:

```
~/.cache/dune/
├── db/files/v5/<hash>/           # Content-addressed cache (existing)
│   └── ...
└── index/                        # Secondary index (new)
    ├── fmt.0.9.0-a1b2c3d4 → ../db/files/v5/deadbeef...
    ├── ocamlformat.0.26.2-b3c4d5e6 → ../db/files/v5/cafebabe...
    └── odoc.2.4.0-f7e8d9c0 → ../db/files/v5/12345678...
```

The index entries use:
- Locked/Toolchain packages: `name.version-build_id8`
- Compiler-independent dev tools: `name.version-checksum8` (source checksum)
- Compiler-dependent dev tools: `name.version-build_id8` (includes compiler deps)

### Why a Secondary Index?

The content-addressed cache keys on rule hash (all inputs). This prevents
sharing in two important cases:

**1. Compiler-independent dev tools**

```
Project A (OCaml 5.2) + ocamlformat 0.26.2 → rule hash X
Project B (OCaml 4.14) + ocamlformat 0.26.2 → rule hash Y
```

Different rule hashes, but the binary is identical. The secondary index uses
the source checksum (`ocamlformat.0.26.2-<checksum8>`) to enable sharing across
projects regardless of their OCaml version.

**2. Compiler-dependent dev tools**

```
Project A (OCaml 5.2) + odoc 2.4.0 → build_id X (includes OCaml 5.2 deps)
Project B (OCaml 5.2) + odoc 2.4.0 → build_id X (same!)
Project C (OCaml 4.14) + odoc 2.4.0 → build_id Y (different compiler)
```

For compiler-dependent tools like odoc, the dev tool's build_id already includes
all dependencies (including the compiler). The index key `odoc.2.4.0-<build_id8>`
ensures projects with the same compiler share the cache while projects with
different compilers correctly get separate binaries.

**3. Relocatable compilers**

The relocatable-compiler package produces binaries that work at any path.
Without a secondary index, each project would rebuild the compiler even
though the output is shareable.

### Why Symlinks?

1. **No duplication** - One copy in content-addressed store, multiple lookup paths
2. **Atomic** - `symlink()` is atomic on POSIX
3. **Simple check** - Symlink exists = installed
4. **Self-validating** - Symlink target hash ensures correctness

### Cache Key Formula

```ocaml
let cache_key ~pkg_type ~name ~version ~build_id ~source_checksum =
  let name_str = Package.Name.to_string name in
  let version_str = Package_version.to_string version in
  let bid8 d = String.sub (Dune_digest.to_string d) ~pos:0 ~len:8 in

  match pkg_type with
  | Workspace | Vendored ->
      (* Never cached - users may edit sources *)
      None

  | Locked | Toolchain ->
      (* build_id = Merkle hash of opam content + deps' build_ids *)
      Some (sprintf "%s.%s-%s" name_str version_str (bid8 build_id))

  | Dev_tool { compiler_dependent = false; _ } ->
      (* Compiler-independent: use source checksum for cross-project sharing.
         The same ocamlformat source produces the same binary regardless of
         which OCaml version the project uses. *)
      (match source_checksum with
       | Some checksum -> Some (sprintf "%s.%s-%s" name_str version_str (bid8 checksum))
       | None -> Some (sprintf "%s.%s-%s" name_str version_str (bid8 build_id)))

  | Dev_tool { compiler_dependent = true; _ } ->
      (* Compiler-dependent: use dev tool's build_id which includes all deps
         (including the compiler). Different compilers = different build_ids. *)
      Some (sprintf "%s.%s-%s" name_str version_str (bid8 build_id))
```

**Key insight:** For compiler-dependent dev tools, the dev tool's `build_id` already
includes the compiler as a dependency. Different compiler versions produce different
`build_id` values, ensuring correct cache separation without needing to explicitly
track the compiler version.

## Build Flow

```
                    ┌─────────────────┐
                    │  Compute        │
                    │  cache_key      │
                    └────────┬────────┘
                             │
              ┌──────────────┴──────────────┐
              │                             │
              ▼                             ▼
     cache_key = None              cache_key = Some key
     (workspace/vendored)          (locked/tool/toolchain)
              │                             │
              │                    ┌────────┴────────┐
              │                    │                 │
              │                    ▼                 ▼
              │              symlink exists?    symlink missing
              │                    │                 │
              │                    ▼                 │
              │              restore from           │
              │              cache (follow          │
              │              symlink)               │
              │                    │                 │
              │                    │                 ▼
              └────────────────────┴──────► build normally
                                                    │
                                                    ▼
                                           install to target_dir
                                                    │
                                           ┌────────┴────────┐
                                           │                 │
                                           ▼                 ▼
                                    cache_key = None   cache_key = Some
                                           │                 │
                                           │                 ▼
                                           │          store in files/v5/
                                           │          create symlink
                                           │                 │
                                           └────────┬────────┘
                                                    ▼
                                           promote to shared prefix
                                           _build/install/<context>/
```

## Environment Setup

### For all packages

```
PATH                ← deps' bin/ directories (prepended)
OPAM_SWITCH_PREFIX  ← target_dir
OPAM_PACKAGE_NAME   ← package name
OPAM_PACKAGE_VERSION ← package version
```

### For opam-mode packages only

```
OCAMLPATH           ← deps' lib/ directories
OCAMLFIND_DESTDIR   ← install lib root
```

This matches vendor.md which says "Dune sets OCAMLPATH so build commands
discover dependencies via ocamlfind" for opam-mode packages.

**Note:** Workspace and vendored dune packages don't need OCAMLPATH because
dune handles library resolution directly.

## Concurrency

Multiple dune processes may try to cache the same package simultaneously.
The flow handles this safely:

1. Build to temporary directory
2. Store in content-addressed cache (atomic via rename)
3. Create symlink (atomic, idempotent - if exists, same target)

If two processes race:
- Both build (wasted work, but correct)
- First to finish creates symlink
- Second sees symlink exists, skips creation

## Cache Invalidation

Cache entries are never automatically invalidated. The content-addressed
nature ensures correctness: same inputs → same hash → same cache entry.

For space management:
```bash
dune cache trim              # Remove least-recently-used entries
dune cache trim --size 10G   # Keep only 10GB
dune cache clear             # Remove everything
```

## What This Simplifies

### Before (current)

```
src/dune_rules/
├── pkg_toolchain.ml      # Separate toolchain cache
├── dev_tool_cache.ml     # Separate dev tool cache (dead code!)
└── pkg_rules.ml          # Special-case handling for toolchains

~/.cache/dune/
├── toolchains/           # Toolchain cache (full copies)
├── tools/                # Dev tool cache (unused)
└── files/                # Build cache
```

### After (unified)

```
src/dune_rules/
├── pkg_cache.ml          # Unified cache (symlinks to files/)
└── pkg_rules.ml          # One flow, no special cases

~/.cache/dune/
├── files/v5/             # Content-addressed cache (unchanged)
└── index/              # Secondary index (symlinks only)
```

## Compatibility

### Migration from old cache directories

The unified `index/` replaces the old separate directories:

| Old | New | Purpose |
|-----|-----|---------|
| `~/.cache/dune/toolchains/` | `~/.cache/dune/index/` | Relocatable compilers |
| `~/.cache/dune/tools/` | `~/.cache/dune/index/` | Dev tools |

Old directories are not migrated. On first use after upgrade:
- Relocatable compilers will be rebuilt and cached in new location
- Dev tools will be rebuilt and cached in new location

Users can delete old directories to reclaim space:
```bash
rm -rf ~/.cache/dune/toolchains ~/.cache/dune/tools
```

### Lock file format

No changes to lock file format. The cache is purely an optimization.

## Open Questions

### What if source_checksum is not available?

Dev tool lock files may not always have a source checksum (e.g., local packages,
git sources without checksums). In this case, we fall back to using `build_id`:

```
ocamlformat.0.26.2-<build_id8>  # Fallback when no checksum
```

This reduces cache sharing (different build_ids for same source across projects)
but maintains correctness.

### Platform-specific binaries

Currently the cache key doesn't include platform information for dev tools.
This assumes:
- Compiler-independent tools produce identical binaries across platforms (true for
  pure OCaml tools like ocamlformat)
- Compiler-dependent tools include platform info in their build_id via deps

If platform-specific binaries become an issue, we could add platform suffix:
```
ocamlformat.0.26.2-<checksum8>-macos-arm64
```

## References

- [RFC: Lock Files](lock.md) - build_id computation
- [RFC: Developer Tools](tools.md) - dev tool caching design
- [RFC: Extended Vendor Stanza](vendor.md) - vendored packages and OCAMLPATH
- [RFC: Opam Build Mode](opam-mode.md) - opam-mode package builds
