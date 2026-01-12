# RFC: Extended Vendor Stanza

## Workflow

Copy source code into a directory, add a vendor stanza, run `dune build`.

```
my-project/
  src/
  vendor/
    fmt.0.9.0/       # copied from somewhere
    yojson.2.0.0/    # copied from somewhere
    dune             # (vendor ...) stanzas
  dune-project
```

Dune doesn't care where the source came from. It builds what's in the directory.

## Syntax

```dune
(vendor <directory>
 (package <name>)             ; optional: inferred from opam file
 (version <version>)          ; optional: inferred from opam file
 (libraries <lib-spec>...)    ; optional: inferred by scanning directory
 (mode <build-mode>)          ; optional: dune | opam (inferred from contents)
 (install <bool>)             ; optional: default true
 (toolchain <name>))          ; optional: marks as compiler/toolchain
```

All fields are optional. Explicit values override inference.

## Examples

**Basic (equivalent to vendored_dirs):**
```dune
(vendor vendor/fmt.0.9.0)
```

**Selective libraries:**
```dune
(vendor vendor/fmt.0.9.0
 (libraries fmt fmt.tty))
```

**Multi-version with aliasing:**
```dune
(vendor vendor/yojson.1.7.0
 (libraries (yojson :as yojson_v1)))

(vendor vendor/yojson.2.0.0
 (libraries yojson))
```

**Non-dune package:**
```dune
(vendor vendor/zarith.1.14)
```
Dune detects the opam file and uses `(mode opam)` automatically.

**Toolchain:**
```dune
(vendor vendor/ocaml.5.2.0
 (toolchain native))
```

## Library Specification

The `<lib-spec>` uses ordered set language:
- `:standard` — all libraries in directory
- `<name>` — expose with original name
- `(<name> :as <alias>)` — expose under different name
- `:standard \ <name>` — all except specified

## Mode Detection

| Directory Contents | Default Mode |
|--------------------|--------------|
| Contains `dune-project` or `dune` files | `dune` |
| Contains only `*.opam` file | `opam` |

In `dune` mode, the directory builds like normal dune code with library filtering
and aliasing applied.

In `opam` mode, dune runs the package's opam build commands.
See [RFC: Opam Build Mode](opam-mode.md) for details.

## Install Promotion

- `(install true)` (default): artifacts go to `_build/install/<context>/`
- `(install false)`: artifacts stay in vendor build directory only
- Using `:as` aliasing implies `(install false)`

Only one package with a given name can be installed per context. If two vendored
packages have the same name and both have `(install true)`, dune errors with a
message suggesting `(install false)` on one.

## Dependency Resolution

Dune resolves at the library level. When code needs library "foo":

1. Vendored dune packages in workspace
2. Vendored opam packages (installed to shared prefix)
3. Locked packages from dune.lock
4. System OCAMLPATH

Dune sets OCAMLPATH so build commands discover dependencies via ocamlfind.

Dune does not require a closure of opam packages. If a vendored opam package
depends on "foo" but the library is available from a dune package, that works.

## Build Ordering

No solver, no version constraints. Dependencies must be in the workspace or system.

When building an opam-mode package:
1. Parse `depends:` field, evaluate filters (`{os = "linux"}`, etc.)
2. For each dependency, look for it in workspace. If found, build it first.
   If not found, assume system provides it.
3. Build with OCAMLPATH = workspace libraries + system
4. If ocamlfind can't find a library: "Library X not found (required by package Y)"

## Toolchains

`(toolchain <name>)` marks the package as providing a compiler:
- `(toolchain native)` — native OCaml compiler
- `(toolchain windows)` — cross-compiler for `(targets native windows)`

Dune sets `OCAMLFIND_TOOLCHAIN=<name>` and builds toolchain packages first.

## Error Handling

| Condition | Error |
|-----------|-------|
| `(libraries foo)` but foo not in directory | library resolution error |
| Two vendors expose same library name | library resolution error |
| `(mode dune)` but no dune files | library not found |
| `(mode opam)` but no opam file | clear error message |
| Alias conflicts with existing library | library resolution error |

## Compatibility

`(vendored_dirs)` continues to work. In `vendor/dune`, these are equivalent:

```dune
(vendored_dirs foo.1.0.0)
(vendor foo.1.0.0)
```

Note: `vendored_dirs` only matches directories at the current level (no paths with `/`).

## Open Questions

**Shared prefix vs isolated derivations?**

Shared prefix (current approach):
- All packages install to `_build/install/<context>/`
- Simple PATH/OCAMLPATH setup
- One version per package per context

Isolated derivations (nix-style):
- Each package in `_build/.pkgs/<context>/<name>-<build-id>/`
- Multiple versions coexist naturally
- More complex PATH/OCAMLPATH setup

## References

- [RFC: Opam Build Mode](opam-mode.md)
- [RFC: Lock Files](lock.md)
- [dune#8652](https://github.com/ocaml/dune/issues/8652)
