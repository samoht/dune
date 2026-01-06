# Cross-Compilation with Dune Pkg

This document describes how cross-compilation works with dune and dune pkg,
identifies gaps, and proposes a design for locking cross-compilers.

## Current Architecture

### Build Contexts

Dune uses **build contexts** to manage cross-compilation:

```dune
; dune-workspace
(context (default (targets native windows)))
```

This creates:
- `_build/default/` - host context (runs build tools)
- `_build/default.windows/` - target context for Windows

Key properties:
- PPX preprocessors always run on host context
- Code generators run on host context
- `Context.for_host` identifies the host context for a target
- Findlib toolchain is discovered via `OCAMLFIND_TOOLCHAIN`

### Duniverse Workflow

```
dune-project → dune pkg lock → dune.lock → dune pkg fetch → duniverse/ → dune build
```

1. **`dune pkg lock`**: Solves dependencies, creates lock file
2. **`dune pkg fetch`**: Downloads ALL packages to `duniverse/`
3. **`dune build`**: Builds using duniverse sources

**Key insight**: ALL packages (dune and non-dune) are fetched to `duniverse/`.
The classification (Duniverse vs Opam_sandbox) only affects BUILD strategy:

| Classification | Build Method | Cross-compilation |
|----------------|--------------|-------------------|
| Duniverse | Normal dune build | Works via contexts |
| Opam_sandbox | Package's build command | Gap: no cross-env |

### What Works Today

For **dune-built packages**: Cross-compilation works automatically because
they're built by dune's context-aware build system.

For **non-dune packages**: The build command runs as-is, without
cross-compilation environment variables.

## Gap: Non-Dune Package Cross-Compilation

When `pkg_rules.ml` builds a non-dune package for a target context, the
build command doesn't receive cross-compilation configuration:

```ocaml
(* Current: runs build command as-is *)
let build_rule pkg ~context =
  run pkg.build_command  (* No CC, PKG_CONFIG_SYSROOT_DIR, etc. *)
```

This affects:
- `conf-*` packages that probe for C libraries
- Packages using autoconf that need `--host=` flags
- Packages that shell out to `cc` directly

**Workaround**: Pure OCaml packages using ocamlfind work because
`OCAMLFIND_TOOLCHAIN` is set. The gap is narrow but real for C bindings.

## Design: Locking Cross-Compilers

### Principle: Solve Simple, Build Smart

The solver doesn't need host/target awareness. Cross-compilers are regular
packages with different names (like MirageOS solo5). Build time handles
the mapping.

### Lock File

Cross-compiler is just another package in the lock:

```dune
; dune.lock (single-file format)
(lang package 0.2)

(repos
 (https://github.com/ocaml/opam-repository.git abc123))

(packages
 ocaml.5.3.0
 ocaml-windows.5.3.0
 conf-gmp.4
 zarith.1.13)

(platforms linux macos windows)
```

Or in directory format (`dune.lock/`):

```
dune.lock/
  lock.dune
  ocaml.5.3.0.pkg
  ocaml-windows.5.3.0.pkg
  conf-gmp.4.pkg
  zarith.1.13.pkg
```

### Workspace Configuration

The workspace specifies which package to use for each target:

```dune
; dune-workspace
(lang dune 3.18)

(context
 (default
  (targets
   (native)
   (windows
    (toolchain-package ocaml-windows)
    (sysroot /usr/x86_64-w64-mingw32)))))
```

Configuration options per target:
- `toolchain-package`: Which OCaml package from the lock to use
- `sysroot`: Path to target system root (for C library discovery)

### Build-Time Behaviour

#### For Native Context (`_build/default/`)

```
conf-gmp build:
  CC=/usr/bin/gcc
  PKG_CONFIG_PATH=/usr/lib/pkgconfig
  OCAMLFIND_TOOLCHAIN=<none>

zarith build:
  → dune build (uses ocaml from lock)
```

#### For Windows Context (`_build/default.windows/`)

```
conf-gmp build:
  CC=/usr/bin/x86_64-w64-mingw32-gcc
  PKG_CONFIG_PATH=/usr/x86_64-w64-mingw32/lib/pkgconfig
  PKG_CONFIG_SYSROOT_DIR=/usr/x86_64-w64-mingw32
  OCAMLFIND_TOOLCHAIN=windows

zarith build:
  → dune build (uses ocaml-windows from lock)
```

### Demand-Driven Build

Dune's build engine is lazy. Only needed packages are built:

```bash
$ dune build src/main.exe
# Builds only: native conf-gmp, native zarith, native ocaml
# Does NOT build: windows versions, ocaml-windows

$ dune build src/main.exe -x windows
# Builds both: native AND windows versions
```

## Implementation

### Changes to pkg_rules.ml

Inject cross-compilation environment for target contexts:

```ocaml
let build_env pkg ~context =
  match Context.for_host context with
  | None ->
    (* Host context: normal environment *)
    base_env
  | Some _host ->
    (* Target context: inject cross-compilation vars *)
    let target = Context.target context in
    let toolchain_pkg = Workspace.toolchain_package target in
    let sysroot = Workspace.sysroot target in
    Env.extend base_env
      ~vars:[ "CC", cc_for_target target
            ; "PKG_CONFIG_SYSROOT_DIR", Path.to_string sysroot
            ; "PKG_CONFIG_PATH", pkg_config_path sysroot
            ; "OCAMLFIND_TOOLCHAIN", toolchain_name toolchain_pkg
            ]
```

### Changes to Workspace

Add `toolchain-package` and `sysroot` to target syntax:

```ocaml
(* workspace.ml *)
type target =
  { name : string
  ; toolchain_package : Package_name.t option
  ; sysroot : Path.External.t option
  }
```

### Changes to Context

Use locked cross-compiler instead of findlib discovery:

```ocaml
(* context.ml - modify target context creation *)
| Named findlib_toolchain ->
  (* Check workspace for toolchain-package first *)
  match Workspace.toolchain_package_for_target target_name with
  | Some pkg_name ->
    (* Use locked package *)
    create_from_locked_package pkg_name
  | None ->
    (* Fall back to findlib discovery *)
    Findlib_config.discover_from_env ~findlib_toolchain
```

## Example: conf-gmp Cross-Compilation

### Project Setup

```
myproject/
  dune-project
  dune-workspace
  dune.lock
  duniverse/
    conf-gmp.4/
    zarith.1.13/
    ocaml.5.3.0/
    ocaml-windows.5.3.0/
```

### dune-project

```dune
(lang dune 3.18)
(package
 (name myproject)
 (depends
  ocaml
  ocaml-windows
  zarith))
```

### dune.lock (directory format)

```
; lock.dune
(lang package 0.2)
(ocaml ocaml)
(platforms linux macos windows)
```

```dune
; conf-gmp.4.pkg
(version 4)

(build
 (all_platforms
  ((action
    (run pkg-config --exists gmp)))))
```

```dune
; zarith.1.13.pkg
(version 1.13)

(build
 (all_platforms dune))

(depends
 (all_platforms (conf-gmp ocaml)))
```

```dune
; ocaml-windows.5.3.0.pkg
(version 5.3.0)

(source
 (fetch
  (url https://github.com/ocaml-cross/opam-cross-windows/...)
  (checksum sha256=...)))

(build
 (all_platforms
  ((action (run ./configure --host=x86_64-w64-mingw32)))))
```

### dune-workspace

```dune
(lang dune 3.18)

(context
 (default
  (targets
   (native)
   (windows
    (toolchain-package ocaml-windows)
    (sysroot /usr/x86_64-w64-mingw32)))))
```

### Build Flow

```bash
$ dune build -x windows

# Step 1: Build ocaml (native) - for host tools
# Step 2: Build ocaml-windows - cross-compiler
# Step 3: Build conf-gmp (native) - uses host pkg-config
# Step 4: Build conf-gmp (windows) - uses sysroot pkg-config
# Step 5: Build zarith (native) - uses ocaml
# Step 6: Build zarith (windows) - uses ocaml-windows
```

## Potential Blockers

1. **Lock structure has only one `ocaml` field**
   - The cross-compiler is a separate package, not a variant of `ocaml`
   - Workspace maps targets to packages, lock just lists packages

2. **Findlib configuration**
   - opam-cross packages register via opam hooks
   - With dune pkg, may need explicit findlib.conf setup
   - Or bypass findlib entirely using locked package paths

3. **Toolchain caching**
   - Compilers cached in `~/.cache/dune/toolchains/`
   - Cross-compiler has different package name, cached separately

4. **C compiler discovery**
   - Currently derived from `ocamlc -config`
   - Need workspace-level override for target sysroot

## Comparison with Other Systems

| System | Host Deps | Target Deps | Toolchain Config |
|--------|-----------|-------------|------------------|
| Nix | `nativeBuildInputs` | `buildInputs` | `pkgsCross` |
| Cargo | `[build-dependencies]` | `[dependencies]` | `.cargo/config.toml` |
| CMake | `FIND_MODE NEVER` | `FIND_MODE ONLY` | Toolchain files |
| Dune (proposed) | via `for_host` | default | `toolchain-package` |

## Summary

The duniverse approach is elegant: by converting dependencies to "just build
dune projects", most packages get cross-compilation for free.

The gap is narrow:
1. Non-dune packages that probe for C libraries need sysroot configuration
2. Cross-compilers need explicit locking and workspace mapping

The proposed design:
- Keeps solving simple (cross-compiler is just another package)
- Adds `toolchain-package` and `sysroot` to workspace target config
- Injects cross-compilation environment for non-dune package builds
