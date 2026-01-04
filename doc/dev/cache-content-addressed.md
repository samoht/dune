# Content-Addressed Caching for Dune

This document describes a proposed improvement to Dune's build cache that uses
content-based tool identification instead of path-based identification. This
addresses cache invalidation issues when tools (especially the OCaml compiler)
are located at different paths across machines or opam switches.

## Problem Statement

Dune's cache currently invalidates when tools are at different paths, even if
the tools are identical. This is because rule digests include tool **paths**
rather than tool **content**.

The rule digest computation in `src/dune_engine/build_system.ml` includes:

```ocaml
let trace = (
  rule_digest_version,
  sandbox_mode,
  Dep.Facts.digest facts ~env,
  target_paths,
  Action.for_shell action,  (* Includes tool paths *)
  ...
)
```

The `Action.for_shell` function in `src/dune_engine/action.ml` serializes tool
paths as strings:

```ocaml
~f_program:(fun ~dir x -> match x with
  | Ok p -> Path.reach p ~from:dir  (* Path string, not content *)
  | Error e -> e.program)
```

**Consequence**: The same compiler binary at `/home/user1/.opam/default/bin/ocamlopt`
vs `/home/user2/.opam/default/bin/ocamlopt` produces different rule digests,
causing cache misses even though the compilers are identical.

## Proposed Solution

Replace path-based tool identification with content-based "tool signatures" that
capture the semantic identity of a tool.

### Architecture

```
+-----------------------------------------------------------+
| Rule Digest Computation                                    |
|   Uses normalized action + tool signatures                 |
+-----------------------------------------------------------+
                           |
                           v
+-----------------------------------------------------------+
| Tool Signature Registry                                    |
|   Maps tools to content-based identities                   |
+-----------------------------------------------------------+
                           |
                           v
+-----------------------------------------------------------+
| Content Addressable Store (existing)                       |
|   Files stored by digest                                   |
+-----------------------------------------------------------+
```

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

For OCaml tools specifically, the config digest is computed from `ocamlc -config`
output with absolute paths replaced by canonical placeholders:
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

## OCaml-Specific Considerations

### Relocatable Compiler Assumption

This design primarily targets compilers with opam's relocatable patches, which
are now standard in opam 2.2+. For non-relocatable compilers, the cache will
fall back to path-based matching (current behavior).

### Existing Infrastructure

Several existing components can be leveraged:

1. **`artifact_substitution.ml`**: Already distinguishes `Relocatable` vs
   `Hardcoded` paths for installed artifacts.

2. **`BUILD_PATH_PREFIX_MAP`**: Already supported in `src/dune_util/` for
   normalizing paths in build outputs.

3. **`pkg_toolchain.ml`**: Has workarounds for non-relocatable compilers in
   package management.

### Embedded Paths

OCaml embeds absolute paths in `.cmi`, `.cmo`, and native code (debug info).
The existing `BUILD_PATH_PREFIX_MAP` mechanism can normalize these.

## Implementation Plan

1. Create `src/dune_engine/tool_signature.ml{,i}` with signature computation
2. Add `Action.for_digest` in `src/dune_engine/action.ml`
3. Integrate into `compute_rule_digest` in `build_system.ml` (bump version 23→24)
4. Add on-disk tool signature cache
5. Extend `Ocaml_toolchain.t` with tool signatures
6. Add `normalize_for_digest` to `ocaml_config.ml`

## Improved Cache Debugging

Add `--debug-cache=rule-digest` flag to help diagnose cache issues:

1. **New flag** in `src/dune_engine/cache_debug_flags.ml`:
   ```ocaml
   type t = {
     shared_cache : bool
   ; workspace_local_cache : bool
   ; fs_cache : bool
   ; rule_digest : bool  (* NEW *)
   }
   ```

2. **Log rule digest components** in `build_system.ml` when enabled:
   - Rule target
   - Action shell representation (shows tool paths)
   - Dependency digest
   - Final rule digest

3. **Enhanced miss reporting** in `rule_cache.ml`:
   - What component changed (action, deps, etc.)
   - Old vs new values for debugging

## Files to Modify

| File | Changes |
|------|---------|
| `src/dune_engine/build_system.ml` | Rule digest computation (use `for_digest`) |
| `src/dune_engine/action.ml` | Add `for_digest` function |
| `src/dune_engine/cache_debug_flags.ml` | Add `rule_digest` flag |
| `src/dune_engine/rule_cache.ml` | Enhanced miss reporting |
| `src/dune_rules/ocaml_toolchain.ml` | Add tool signatures |
| `src/ocaml-config/ocaml_config.ml` | Config normalization |

**New files**:
- `src/dune_engine/tool_signature.ml`
- `src/dune_engine/tool_signature.mli`

## Related Work

This approach is inspired by Nix's content-addressed store, where derivations
reference inputs by content hash rather than path. However, our approach is
simpler:
- We don't require relocatable binaries for all tools
- We hash semantic identity (version + config) rather than binary content
- We maintain backward compatibility with existing cache format

## Open Questions

1. **Non-OCaml tools**: How should we compute signatures for tools like `gcc`
   that don't have a `-config` option? Version string only?

2. **Findlib paths**: Library resolution depends on installation paths. Should
   we include content digests of META files?

3. **Sandboxing interaction**: How does this work with sandboxed builds where
   paths are rewritten?
