# Cross-Compilation with Dune Pkg

This document describes how cross-compilation works with dune and dune pkg,
identifies gaps, and proposes a design for locking cross-compilers.

## Context Types

Dune supports three context types, each representing a different OCaml source:

```dune
(context default)                                    ; system OCaml (findlib)
(context (opam (switch foo)))                        ; opam switch
(context (workspace (compiler ocaml.5.4)))           ; local workspace
```

| Context Type | OCaml Source |
|--------------|--------------|
| `default` | System findlib / environment |
| `opam` | Opam switch |
| `workspace` | Local workspace (dune.lock / duniverse / vendored) |

All three context types support cross-compilation via `(targets ...)`.

### Toolchain Stanza

Cross-compilation toolchains are declared explicitly with `(toolchain ...)`:

```dune
; dune-workspace
(lang dune 3.18)

; Define toolchains
(toolchain windows
  (repository https://github.com/ocaml-cross/opam-cross-windows.git)
  (package ocaml-windows))

(toolchain android
  (repository https://github.com/ocaml-cross/opam-cross-android.git)
  (package ocaml-android))

(toolchain solo5
  (package ocaml-solo5))  ; no extra repo needed, in main opam

; Use toolchains by name in targets
(context (workspace
  (compiler ocaml.5.4)
  (targets native windows android solo5)))
```

This creates:
- `default` → uses `ocaml.5.4` for host
- `default.windows` → uses `ocaml-windows.5.4` (version from compiler)
- `default.android` → uses `ocaml-android.5.4`
- `default.solo5` → uses `ocaml-solo5.5.4`

### Toolchain Stanza Syntax

```dune
(toolchain <name>
  (repository <url>)    ; optional: repo containing the cross-compiler
  (package <name>))     ; required: package name (version derived from compiler)
```

- `<name>`: Toolchain name, used in `(targets ...)` and as context suffix
- `(repository ...)`: Git URL for the opam repository (optional if package is in main opam)
- `(package ...)`: Cross-compiler package name, version is derived from `(compiler ...)` in context

### How It Works

1. `(toolchain windows (package ocaml-windows) ...)` declares a toolchain
2. `(context (workspace (compiler ocaml.5.4) (targets native windows)))` uses it
3. Dune automatically:
   - Adds `ocaml-windows.5.4` to solver query (version from compiler)
   - Adds the repository to solver if specified
   - Sets `OCAMLFIND_TOOLCHAIN=windows` for `default.windows` context

## Current Architecture

### Build Contexts

Dune uses **build contexts** to manage cross-compilation:

```dune
; dune-workspace - using system/opam toolchain (already installed)
(context (default (targets native windows)))

; dune-workspace - using workspace toolchain (requires toolchain stanza)
(context (workspace
  (compiler ocaml.5.4)
  (targets native windows)))
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

## Opam Build Directory Structure

Non-dune packages are built in `_build/.pkgs/<context>/` using context names:

```
_build/.pkgs/
  default/
    <pkg-name>.<version>-<digest>/
      source/     # extracted source
      target/     # install prefix
  ocaml414/
    <pkg-name>.<version>-<digest>/
      source/
      target/
  default.windows/
    <pkg-name>.<version>-<digest>/
      source/
      target/
```

Each context has its own copy of sources and build artifacts.

### Per-Context Opam Builds

When building non-dune packages (opam), each context builds separately:

1. **Per-context isolation**: Each context has its own `_build/.pkgs/<context>/` directory
2. **Compiler injection**: Each context uses its `(compiler ...)` package
3. **Environment**: `OCAMLFIND_TOOLCHAIN`, `CC`, etc. set per context

```
# Building zarith (non-dune) for multiple contexts:

_build/.pkgs/default/zarith.1.14-<digest>/source/          # built with ocaml.5.2.0
_build/.pkgs/ocaml414/zarith.1.14-<digest>/source/         # built with ocaml.4.14.2
_build/.pkgs/default.windows/zarith.1.14-<digest>/source/  # built with ocaml-windows
```

### Implementation: pkg_rules.ml Changes

Currently `pkg_rules.ml` builds packages to a single location. Changes needed:

1. **Context-aware build paths**: Include context name in build directory
2. **Compiler from context**: Use `workspace_context.compiler` not just lock default
3. **Environment per context**: Set `OCAMLFIND_TOOLCHAIN` based on context targets

Example workspace with multiple compilers and cross-compilation:

```dune
; dune-workspace
(lang dune 3.18)

; Multi-version testing with workspace contexts
(context (workspace (compiler ocaml.5.4)))

(context (workspace
  (compiler ocaml.4.14)
  (name ocaml414)))

; Cross-compilation with workspace context (requires toolchain stanza above)
(context (workspace
  (compiler ocaml.5.4)
  (targets native windows)))
```

Running `dune pkg lock` with this workspace solves for all specified compilers
and platforms, creating a single dune.lock with appropriate package filters.

This creates:
- `_build/default/` and `_build/.pkgs/default/` (OCaml 5.4)
- `_build/ocaml414/` and `_build/.pkgs/ocaml414/` (OCaml 4.14)
- `_build/default.windows/` and `_build/.pkgs/default.windows/` (cross-compiled)

Each context has its own copy of sources and build artifacts.

### How Cross-Compilation Targets Work

When `(targets native windows)` is specified (with a `(toolchain windows ...)` stanza):
1. The `ocaml-windows.5.4` package is built from the local workspace (version from compiler)
2. It registers a findlib toolchain named `windows`
3. Dune sets `OCAMLFIND_TOOLCHAIN=windows` for the `default.windows` context
4. Local ocamlfind (also from workspace) discovers the toolchain

This uses the same mechanism as opam-cross, just with local packages.

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

The workspace specifies which compiler package to use via `workspace` contexts:

```dune
; dune-workspace
(lang dune 3.18)

; Simple case: single compiler from local workspace
(context (workspace (compiler ocaml.5.4)))

; Cross-compilation: targets reference toolchain stanzas
(context (workspace
  (compiler ocaml.5.4)
  (targets native windows)))
```

Configuration options for `workspace` contexts:
- `compiler`: Which OCaml package from the local workspace to use (required)
- `name`: Context name (default: `default`)
- `targets`: Cross-compilation targets (`native` or toolchain names from `(toolchain ...)` stanzas)

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

Add new `Workspace` context type alongside `Default` and `Opam`:

```ocaml
(* workspace.ml *)
module Workspace = struct
  type t =
    { base : Common.t
    ; compiler : Package_name.t  (* required: OCaml package from local workspace *)
    }
end

type t =
  | Default of Default.t
  | Opam of Opam.t
  | Workspace of Workspace.t    (* NEW *)
```

### Changes to Context

Handle `Workspace` context in context creation:

```ocaml
(* context.ml *)
let create_for_workspace workspace_ctx =
  (* Use pkg_rules.ocaml_toolchain with specified compiler package *)
  let compiler_pkg = workspace_ctx.compiler in
  let* toolchain = Pkg_rules.ocaml_toolchain ~compiler_override:(Some compiler_pkg) ctx in
  ...
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

(toolchain windows
  (repository https://github.com/ocaml-cross/opam-cross-windows.git)
  (package ocaml-windows))

(context (workspace
  (compiler ocaml.5.4)
  (targets native windows)))
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
| Dune (proposed) | via `for_host` | default | `(compiler ...)` |

## Summary

The duniverse approach is elegant: by converting dependencies to "just build
dune projects", most packages get cross-compilation for free.

The gap is narrow:
1. Non-dune packages that probe for C libraries need sysroot configuration
2. Cross-compilers need explicit locking and workspace mapping

The proposed design:
- Keeps solving simple (cross-compiler is just another package)
- Adds `compiler` and `sysroot` to workspace context config
- Injects cross-compilation environment for non-dune package builds
