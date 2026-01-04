# Progress Display UX

This document describes a unified progress display design for Dune's build and
package management operations.

## Problem Statement

Current build display only shows percentage and rule counts:
```
Done: 42% (100/238, 138 left) (jobs: 8)
```

This is not helpful because:
- Percentage is misleading when rule count grows dynamically
- Users want to know **what** is being built, not abstract counts
- No visibility into cache utilization

## Design Principles

1. **Unified format** for builds and package management
2. **Show what's happening**, not abstract counts
3. **No percentages** for dynamic rule discovery
4. **Cache stats at summary only**, not during progress
5. **Precise truncation behavior** for narrow terminals

## Progress Format

### During Operations

Everything is just building targets. Whether it's a library in your repo
(`dune-engine`) or an opam package (`cmdliner`), they're unified in the build
graph.

Format:
```
Building: dune-engine, cmdliner, main (3/15, 8j)
```

- Comma-separated target names (libraries, packages, executables)
- `3/15` = 3 completed out of 15 total libraries/packages
- `8j` = 8 concurrent jobs

The count is at library/package granularity, not rules. This is stable (known
upfront) unlike dynamic rule counts, and matches how users think about builds.

When network I/O is blocking (fetching sources), show it:
```
Fetching: ocaml-base-compiler | Building: dune-engine (3/15, 4j)
```

### Truncation Behavior

When terminal width is insufficient:

1. Measure available width after `<Phase>: ` prefix
2. Add targets left-to-right until width exceeded
3. If truncated, append `, +N more`

Example at 60 columns:
```
Building: dune-engine, dune-rules, +6 more (8j)
```

If even one target doesn't fit:
```
Building: +8 targets (8j)
```

## Notes

- Everything is building targets - no artificial phase distinctions
- `Cached` is a property of how a target was satisfied, shown in summary only
- `Fetching` shown separately only as optimization hint (network-bound vs CPU-bound)

## Summary at End

After completion, show summary with cache statistics:

```
Done in 12.3s (38 cached, 4 built)
```

For package operations:
```
Installed 10 packages in 45.2s (3 cached, 7 built)
```

For failed builds:
```
Failed after 5.2s (2 errors)
```

## Verbosity Levels

| Level | Flag | Shows |
|-------|------|-------|
| Quiet | `-q` | Errors only |
| Normal | (default) | `Building: target1, target2 (3/15, 8j)` |
| Verbose | `-v` | + build commands |

## Implementation

### Types

```ocaml
type target = {
  name : string  (* library/package/executable name *)
; is_fetch : bool  (* true if network I/O *)
}

type t = {
  active : target list
; completed : int
; total : int  (* library/package count, known upfront *)
; cached : int
; failed : int
}
```

### Display Function

```ocaml
val pp : max_width:int -> t -> Pp.t
(** Renders progress respecting terminal width.
    Groups fetching targets separately if any. *)
```

### Integration Points

| File | Changes |
|------|---------|
| `src/dune_engine/build_system.ml` | Track active targets |
| `src/dune_pkg/fetch.ml` | Report fetch progress |
| `src/dune_rules/pkg_rules.ml` | Report build progress |
| `bin/import.ml` | Update status line format |

**New files:**
- `src/dune_engine/progress.ml`
- `src/dune_engine/progress.mli`

## Non-Goals

- Elapsed time during build (only in summary if needed)
- Download speed/ETA
- Colors for phases (phases are self-explanatory)
- Progress bars (discrete operations don't need them)
