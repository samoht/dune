# RFC: Developer Tools

## Mental Model

**Dev tool = isolated build + simple cache index.**

Tools like `ocamlformat`, `odoc`, and `ocaml-lsp-server` exist alongside your
project without polluting its dependency tree.

```
dune fmt
    ↓
check index: ~/.cache/dune/tools/ocamlformat.0.26.2
    ↓ miss
solve + build in isolated context
    ↓
store binary in build cache (content-addressed)
    ↓
create index: tools/ocamlformat.0.26.2 → files/<hash>
    ↓
symlink: _build/install/default/bin/ocamlformat
```

## User Experience

### Just Works

```bash
dune fmt                    # Installs ocamlformat if needed
dune build @doc             # Installs odoc if needed
```

No configuration. Dune detects when a tool is required, solves dependencies,
builds in isolation, and caches the result.

### Version Pinning

For ocamlformat, create `.ocamlformat`:

```
version = 0.26.2
```

### Explicit Commands

```bash
dune tools install ocamlformat       # Install + cache
dune tools exec odoc -- --help       # Run (auto-install if needed)
dune tools which ocamlformat         # Show location
```

## Supported Tools

| Tool | Package | Compiler-Dependent |
|------|---------|-------------------|
| ocamlformat | `ocamlformat` | No |
| odoc | `odoc` | Yes |
| ocaml-lsp-server | `ocaml-lsp-server` | Yes |
| utop | `utop` | Yes |
| merlin | `merlin` | Yes |
| odig | `odig` | Yes |
| ocaml-index | `ocaml-index` | Yes |
| dune-release | `dune-release` | No |
| opam-publish | `opam-publish` | No |
| earlybird | `earlybird` | No |

**Compiler-dependent** tools interact with compiler internals. They must be
built with the same OCaml version as the project.

**Compiler-independent** tools work with any compiler version.

## Directory Structure

```
_build/
├── .locks/tools-ocamlformat/         # Lock directory
├── .pkgs/tools-ocamlformat/          # Package builds (isolated context)
└── install/
    ├── tools-ocamlformat/bin/        # Tool install prefix
    └── default/bin/ocamlformat       # Symlink to cached binary

~/.cache/dune/
├── files/v5/<hash>                   # Build cache (content-addressed)
└── tools/                            # Secondary index (simple keys)
    ├── ocamlformat.0.26.2 → ../files/v5/<hash>
    └── odoc.2.4.0-5.2.0-a1b2c3d4 → ../files/v5/<hash>
```

## Cache Design

### Why a Secondary Index?

The build cache keys on rule hash (all inputs including compiler). For
compiler-independent tools, this causes unnecessary cache misses:

```
Project A (OCaml 5.2) + ocamlformat 0.26.2 → rule hash X
Project B (OCaml 4.14) + ocamlformat 0.26.2 → rule hash Y
```

Different hashes, different cache entries - even though the binary is identical.

The tool index uses **simpler keys** that only include what matters:

| Type | Key | Example |
|------|-----|---------|
| Compiler-independent | `{pkg}.{version}` | `ocamlformat.0.26.2` |
| Compiler-dependent | `{pkg}.{version}-{ocaml}-{build_id}` | `odoc.2.4.0-5.2.0-a1b2c3d4` |

### No Duplication

The index is just symlinks into the content-addressed build cache. One copy of
each binary, multiple ways to look it up.

### Lookup Flow

1. Compute index key from tool + version (+ compiler if needed)
2. Check `~/.cache/dune/tools/<key>`
3. **Hit**: Follow symlink to cached binary
4. **Miss**: Build tool, store in build cache, create index entry

## Dependency Isolation

Each tool gets its own build context (`tools-<name>`). Tool dependencies are
never added to the project's `OCAMLPATH`:

- No version conflicts with project dependencies
- No accidental imports of tool libraries
- No constraint propagation from tools to project

## Repository Resolution

Dev tools use the default repository defined for the workspace.

## References

- [Build Cache Design](../doc/dev/cache.md)
- [Tool Caching Design](../doc/dev/tool-caching.md)
- [Customize Dev Tool Lock Directories](../doc/howto/customize-dev-tools-lock-directories.md)
