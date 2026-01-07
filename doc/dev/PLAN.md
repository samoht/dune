# Dune Pkg Feature Implementation Roadmap

## Overview

This document outlines the implementation order for dune pkg features across all design docs.

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

### Cross-Compilation with Toolchain Stanza

```dune
; Define toolchains explicitly
(toolchain windows
  (repository https://github.com/ocaml-cross/opam-cross-windows.git)
  (package ocaml-windows))

(toolchain solo5
  (package ocaml-solo5))  ; in main opam repo

; Reference toolchains by name
(context (workspace
  (compiler ocaml.5.4)
  (targets native windows solo5)))
```

The `(toolchain ...)` stanza:
- Defines cross-compilation target by name
- Specifies repository (optional) and package
- Package version derived from `(compiler ...)` in context

## Feature Summary

| Feature | Design Doc | Status |
|---------|------------|--------|
| Console/Display redesign | display.md | Designed |
| Lock file minimal format | lock-file-format.md | Designed |
| New `workspace` context type | cross-compilation.md | **Revised** |
| Per-context opam sandbox | cross-compilation.md | **Already exists** |
| Lock file filters (os/arch/ocaml) | lock-file-format.md | **Partially exists** |
| Post dependencies | lock-file-format.md | **Already implemented** |
| Vendoring (`vendor` stanza) | vendoring.md | Designed |
| Patching workflow | patching.md | Designed |
| Dev tool caching | tool-caching.md | Designed |
| Content-addressed digests | tool-caching.md | Designed |

## Current Codebase State

### What Already Exists

1. **Per-context package builds** (pkg_rules.ml)
   - Paths are context-aware with consistent naming:
     - Build: `_build/{context}/`
     - Pkg: `_build/pkg/{context}/{pkg_digest}/`
     - Lock: `_build/lock/{context}/`
     - Install: `_build/install/{context}/`
   - Each context gets its own build tree

2. **Platform filters in lock file** (lock.ml:4-172)
   - `Solver_env_disjunction.t` for platform matching
   - `Conditional_choice.t` for platform-specific values
   - `enabled_on_platforms` field on packages

3. **Post dependencies** (lock.ml:554)
   - `post_depends : Dependencies.t Conditional_choice.t` field exists
   - Tracked separately to avoid false cycles

4. **OCaml toolchain from lock** (pkg_rules.ml:2727-2760)
   - `ocaml_toolchain` function reads `lock_dir.ocaml`
   - Returns memoized toolchain from locked OCaml package

### What's Missing

1. **Workspace `compiler` field** - `Context.Common.t` has `toolchain` (findlib) but no `compiler` field
2. **Lock file `compiler` naming** - Currently called `ocaml`, needs rename for consistency
3. **Workspace-to-toolchain wiring** - `ocaml_toolchain` doesn't check workspace context's compiler override
4. **OCaml version filters** - Platform filters exist but no `(ocaml >= 5)` style filters

## Implementation Order

### Tier 1: Foundations (Independent, can parallelize)

#### 1.1 Console/Display Redesign
**Why first:** Improves UX for all subsequent features, independent of pkg changes.

Files:
- `src/dune_console/backend_intf.ml` - new interface
- `src/dune_console/quiet.ml` - errors only
- `src/dune_console/progress.ml` - rewrite
- `src/dune_console/short.ml` - new
- `src/dune_console/verbose.ml` - new

#### 1.2 CLI Options for `dune pkg lock`
**Why early:** Simple additions that improve UX immediately.

```bash
dune pkg lock --compiler ocaml.4.14.2
dune pkg lock --platform linux-x86_64,macos-arm64
dune pkg lock --repo opam,relocatable
```

Files:
- `bin/pkg/lock.ml` - add options
- `src/dune_pkg/solver.ml` - accept compiler/platform params

---

### Tier 2: Multi-Compiler Support (Core pkg feature)

#### 2.1 Workspace `(compiler ...)` Field
**Why:** Enables multiple compiler versions without opam switches.

```dune
(context default)
(context (default (name ocaml414) (compiler ocaml.4.14.2)))
```

**Implementation:**

1. **Add field to Context.Common.t** (workspace.ml:355-369)
   ```ocaml
   type t = {
     ...
     toolchain : Context_name.t option      (* existing - findlib *)
     compiler : Package_name.t option       (* NEW - lock file package *)
     ...
   }
   ```

2. **Add decoder** (workspace.ml:418-425)
   ```ocaml
   and+ compiler =
     field_o "compiler" (Dune_lang.Syntax.since syntax (3, 18) >>> Package_name.decode)
   ```

3. **Wire to pkg_rules.ml** - Modify `ocaml_toolchain` (lines 2727-2760):
   ```ocaml
   let ocaml_toolchain context =
     let* workspace_ctx = ... in  (* get workspace context *)
     let* lock_dir = Lock_dir.get_exn context in
     let ocaml_pkg = match workspace_ctx.compiler with
       | Some pkg -> Some pkg  (* workspace override *)
       | None -> lock_dir.ocaml  (* default from lock *)
     in
     ...
   ```

Files:
- `src/source/workspace.ml` - add `compiler` field (lines 355-369, 418-425)
- `src/source/workspace.mli` - update interface (lines 58-79)
- `src/dune_rules/pkg_rules.ml` - modify `ocaml_toolchain` (lines 2727-2760)

#### 2.2 Lock File `compiler` Field
**Why:** Consistency between workspace and lock file naming.

**Implementation:**

1. Rename field in `Lock.t` (lock.ml:1120):
   ```ocaml
   ; compiler : (Loc.t * Package_name.t) option  (* was: ocaml *)
   ```

2. Update encoding (lock.ml:1312-1314):
   ```ocaml
   | Some c -> [ list sexp [ string "compiler"; Package_name.encode (snd c) ] ])
   ```

3. Add backwards-compat decoder that accepts both `ocaml` and `compiler`

Files:
- `src/dune_pkg/lock.ml` - rename field and update encode/decode (lines 1116-1124, 1312-1340)
- `src/dune_pkg/lock.mli` - update interface

#### 2.3 Per-Context Opam Sandbox Builds
**Status:** Already exists! Context-aware paths in pkg_rules.ml.

```
_build/pkg/default/{pkg_digest}/      # default context
_build/pkg/ocaml414/{pkg_digest}/     # ocaml414 context
_build/install/default/               # shared install dir for default
_build/install/ocaml414/              # shared install dir for ocaml414
```

**No implementation needed** - just verify the existing code works correctly with multiple compiler contexts.

---

### Tier 3: Lock File Enhancements

#### 3.1 OCaml Version Filters
**Why:** Single lock file for multiple compilers.
**Status:** Platform filters exist, need to add OCaml version filters.

```lisp
(packages
 (conf-libffi.2.0.0 (os linux))
 (ppxlib.0.32.0 (ocaml < 5))       ; NEW - OCaml version filter
 (ppxlib.0.33.0 (ocaml >= 5)))     ; NEW - OCaml version filter
```

**Implementation:**

1. Extend `Solver_env` to include OCaml version
2. Add `(ocaml <op> <version>)` filter syntax to `Package_entry`
3. Solver produces version-filtered packages when multiple compilers specified

Files:
- `src/dune_pkg/solver_env.ml` - add OCaml version to environment
- `src/dune_pkg/lock.ml` - extend `Package_entry` filter syntax (lines 1509-1590)
- `src/dune_pkg/solver.ml` - merge solutions for multiple compilers

#### 3.2 Post Dependencies
**Status:** Already implemented in lock.ml:554!

```ocaml
; post_depends : Dependencies.t Conditional_choice.t
```

**Remaining work:** Verify build order handling and `:installed` variable semantics.

#### 3.3 Minimal Lock File Format (Optional)
**Why:** Reduces lock file noise, better diffs.

Files:
- `src/dune_pkg/lock.ml` - File module with minimal format
- `src/dune_pkg/opam_repo.ml` - `load_package_at_hash`
- Migration: read both formats, write minimal

---

### Tier 4: Vendoring & Patching

#### 4.1 Vendor Stanza
**Why:** Enables editable dependencies with selective exposure.

```lisp
(vendor fmt.0.9.0
 (libraries fmt fmt.tty))
```

Files:
- `src/dune_rules/vendor_stanza.ml` - new stanza parsing
- `src/dune_rules/lib_db.ml` - filter libraries by vendor config
- `src/dune_rules/pkg_rules.ml` - sandbox mode support

#### 4.2 Generated duniverse/dune
**Why:** Auto-generate vendor stanzas from lock file.

Files:
- `src/dune_pkg/fetch.ml` - generate duniverse/dune
- Sandbox mode detection from build commands

#### 4.3 Patching Workflow
**Why:** Enables local modifications that persist.

```bash
dune pkg patch diff <pkg>
dune pkg patch commit <pkg>
```

Files:
- `bin/pkg/patch.ml` - new subcommands
- `src/dune_pkg/patch.ml` - diff/apply logic
- `src/dune_pkg/lock.ml` - patches field in lock file

---

### Tier 5: Optimizations

#### 5.1 Dev Tool Binary Caching
**Why:** Faster tool installation across projects.

```
~/.cache/dune/tools/ocamlformat.0.26.2/bin/ocamlformat
```

Files:
- `src/dune_rules/pkg_dev_tool.ml` - cache lookup
- `bin/tools/tools_common.ml` - auto-install flow

#### 5.2 Content-Addressed Rule Digests
**Why:** Better cache hits when tool paths differ.

Files:
- `src/dune_engine/tool_signature.ml` - new module
- `src/dune_engine/action.ml` - `for_digest` function
- `src/dune_engine/build_system.ml` - use tool signatures

---

## Recommended Starting Point

**Start with Tier 2.1 (Workspace compiler field)** because:
1. High user value - enables multi-OCaml-version testing
2. Foundation for cross-compilation
3. Infrastructure already exists (per-context paths, toolchain loading)
4. Small, focused change

**First PR scope:**
1. Add `compiler : Package_name.t option` to `Context.Common.t`
2. Add decoder for `(compiler <pkg>)` field
3. Modify `ocaml_toolchain` to check workspace compiler before lock default
4. Add test case with multiple contexts using different compilers

**Estimated changes:** ~50 lines across 3-4 files.

## Dependencies Graph

```
Tier 1 (Display, CLI) ──────────────────────────────────────┐
                                                            │
Tier 2.1 (Compiler field) ◄─────────────────────────────────┤
    │                                                       │
    ├── Tier 2.2 (Lock file rename ocaml→compiler)          │
    │                                                       │
    ▼                                                       │
Tier 3 (OCaml filters, Minimal format) ◄────────────────────┤
         │                                                  │
         ▼                                                  │
Tier 4 (Vendoring, Patching) ◄──────────────────────────────┘
         │
         ▼
Tier 5 (Tool caching, Content-addressed)
```

**Notes:**
- Tier 1 items are independent and can be done anytime
- Tier 2.3 (per-context builds) already exists - no work needed
- Post dependencies (Tier 3.2) already implemented - just needs verification
- Tier 5 can be done after Tier 2

## Key Code Locations

| Feature | Primary File | Key Lines |
|---------|-------------|-----------|
| Workspace context types | `src/source/workspace.ml` | 355-369 |
| Context field decoders | `src/source/workspace.ml` | 418-511 |
| Lock file types | `src/dune_pkg/lock.ml` | 1116-1124 |
| OCaml toolchain | `src/dune_rules/pkg_rules.ml` | 2727-2760 |
| Package build paths | `src/dune_rules/pkg_rules.ml` | 246-256 |
| Platform filters | `src/dune_pkg/lock.ml` | 4-172 |
