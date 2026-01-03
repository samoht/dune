# Dune Package Management UX Design Document

A UX review and improvement roadmap for `dune pkg` CLI, informed by modern package managers (Cargo, Bun, uv).

**Status:** In Progress
**Date:** 2026-01-03

---

## Completed Changes (Quick Wins)

The following improvements have been implemented:

### 1. Replaced `<` with `->` in outdated output
- **File:** `src/dune_pkg/outdated.ml:159`
- **Before:** `bar 0.0.1 < 0.0.2`
- **After:** `bar 0.0.1 -> 0.0.2`

### 2. Simplified "opam sandbox" terminology
- **File:** `bin/pkg/pkg_common.ml:151`
- **Before:** `opam sandbox:` and `duniverse (dune-built):`
- **After:** `opam:` and `dune:`

### 3. Updated fetch messages to use simpler terminology
- **File:** `bin/pkg/fetch.ml`
- **Before:** "all packages use opam sandbox"
- **After:** "all packages use opam"

### 4. Added package count to lock output
- **File:** `bin/pkg/lock.ml:287-310`
- **Before:** `Solution for dune.lock:`
- **After:** `Solution for dune.lock (5 packages):`

### 5. Updated man page example for outdated command
- **File:** `bin/pkg/outdated.ml:98-100`
- Updated to show `->` instead of `<`

**140+ test files were updated** with the new output format.

---

## Executive Summary

Overall, Dune's package management UX is functional but lacks some polish and discoverability features that modern package managers provide. The biggest gaps are around:
1. **Missing common commands** (`add`, `remove`, `update`, `init`)
2. **Lack of visual feedback** (no colors, minimal progress indicators)
3. **Sparse dependency information** (no tree view, no `why` explanations)
4. **Inconsistent output formatting**

---

## Detailed Observations

### 1. Missing Essential Commands

**Modern PM standard:**
```bash
cargo add serde           # Add dependency
cargo remove serde        # Remove dependency
cargo update              # Update lock file
cargo init                # Initialize new project
```

**Dune currently:**
- No `dune pkg add <package>` - users must manually edit `dune-project`
- No `dune pkg remove <package>`
- No `dune pkg update` (have to re-run `lock`)
- No `dune pkg init`

**Recommendation:** Add `dune pkg add` and `dune pkg remove` at minimum.

---

### 2. Lock Command Output

**Current output (`dune pkg lock`):**
```
Solution for dune.lock

Dependencies common to all supported platforms:
opam sandbox:
- foo.0.0.1
- bar.0.0.2
```

**Issues:**
- "opam sandbox" is cryptic - what does it mean to users?
- No indication of what's new vs already locked
- No download progress or timing info
- No distinction between direct and transitive deps

**Cargo equivalent:**
```
Updating crates.io index
  Adding serde v1.0.152
Locking 12 packages to latest compatible versions
```

**Bun equivalent:**
```
bun add v1.0.0

installed serde@1.0.152

3 packages installed [45.00ms]
```

**Recommendations:**
- Show "Added:", "Updated:", "Unchanged:" sections
- Add timing information
- Consider showing direct vs transitive deps separately
- Replace "opam sandbox" with clearer terminology

---

### 3. Search Output

**Current output (`dune pkg search`):**
```
- bar 0.0.1 Mock bar package that depends on foo.
- foo 0.0.1 (no synopsis)
```

**Issues:**
- Plain bullet points, hard to scan
- No download counts, popularity indicators
- `(no synopsis)` looks awkward - could be omitted or styled differently
- No indication of installed status

**Cargo equivalent:**
```
serde = "1.0.152"        # A generic serialization framework
serde_json = "1.0.91"    # JSON serialization
```

**uv equivalent:**
```
requests (2.28.1)        Downloads: 15M/month
  Python HTTP library
```

**Recommendations:**
- Show package name prominently (bold/color)
- Add version constraints or latest version info
- Consider adding popularity/download metrics
- Mark installed packages with a checkmark

---

### 4. Outdated Command

**Current output:**
```
- 1/2 packages in dune.lock are outdated.
  - bar 0.0.1 < 0.0.2
- 1/2 packages in dune.workspace.lock are outdated.
  - bar 0.0.1 < 0.0.2
```

**Good:** Clear count, shows current vs available version.

**Issues:**
- No color coding (red for major, yellow for minor, etc.)
- No changelog links
- `0.0.1 < 0.0.2` format is technical - could be `0.0.1 -> 0.0.2`

**Cargo equivalent:**
```
Name          Current  Latest
----          -------  ------
serde         1.0.150  1.0.152
serde_json    1.0.89   1.0.91
```

**Recommendations:**
- Use tabular format for readability
- Add semver-aware coloring
- Change `<` to `->` or use columns

---

### 5. Build Progress

**Current output:**
```
    Building foo.0.0.1
```

**Issues:**
- No progress bar
- No ETA or timing
- No parallel build indicator (5/12 packages)
- Leading whitespace without context

**Cargo equivalent:**
```
   Compiling serde v1.0.152
   Compiling serde_json v1.0.91
    Finished dev [unoptimized + debuginfo] target(s) in 2.45s
```

**Bun equivalent:**
```
[#################-----------] 58% building...
```

**Recommendations:**
- Add package count: `Building foo.0.0.1 (1/5)`
- Show timing on completion
- Consider progress bar for longer builds

---

### 6. Error Messages

**Current (missing curl):**
```
Error: The program 'curl' does not appear to be installed. Dune uses 'curl'
to download packages...
Hint: Install 'curl' with your system package manager.
```

**Good:** Clear explanation and actionable hint.

**Current (conflict):**
```
Error:
Unable to solve dependencies while generating lock directory: dune.lock

Couldn't solve the package dependency formula.
Selected candidates: bar.0.0.1 x.dev
- foo -> (problem)
    No usable implementations:
      foo.0.0.1: Rejected by conflicts of local package x
```

**Issues:**
- `(problem)` is not helpful
- Technical language ("package dependency formula")
- No suggested resolution

**Cargo equivalent:**
```
error: failed to select a version for `foo`.
    ... required by package `bar v0.0.1`
    ... which satisfies dependency `bar = "^0.0.1"` of package `myproject`

versions that meet the requirements `^1.0` are: 1.0.5, 1.0.4, 1.0.3

the package `myproject` depends on `foo`, with features: `derive`
but `foo` does not have these features.
```

**Recommendations:**
- Show dependency chain (why was this package needed?)
- Suggest specific actions to resolve
- Replace `(problem)` with meaningful status

---

### 7. Missing Dependency Tree View

**Modern PMs have:**
```bash
cargo tree                    # Show dependency tree
cargo tree --invert serde     # Why is serde needed?
npm why lodash                # Why is this installed?
```

**Dune currently:** No equivalent command.

**Recommendation:** Add `dune pkg tree` and `dune pkg why <package>`.

---

### 8. Patch Command UX (DONE)

**Updated behavior:**
```
$ dune pkg patch
dune: required COMMAND name is missing, must be one of 'commit', 'create', 'list' or 'remove'.
Usage: dune pkg patch COMMAND …
Try 'dune pkg patch --help' for more information.
```

Now uses subcommands for consistency:
```
dune pkg patch list           # List all patches and their status
dune pkg patch create <PKG>   # Prepare a package for patching
dune pkg patch commit <PKG>   # Generate patch from local modifications
dune pkg patch remove <PKG>   # Remove a patch
```

---

### 9. Fetch/Duniverse UX

**Current:**
```
Fetching mypkg.1.0.0 to duniverse/mypkg.1.0.0
Fetched 1 duniverse package(s) to duniverse/
```

**Issues:**
- "duniverse" is OCaml-specific jargon
- No download progress or size info

**Recommendation:**
- Use clearer directory name or explain purpose
- Show download sizes and speeds

---

### 10. Colors and Formatting

**Observation:** All output appears to be plain text without ANSI colors.

**Modern PMs use colors extensively:**
- Green for success/added
- Red for errors/removed
- Yellow for warnings/outdated
- Bold for package names
- Dim for transitive deps

**Recommendation:** Add color support with `--color=auto|always|never` flag.

---

## Quick Wins (Low Effort, High Impact)

### To Implement Now

#### 1. Change `<` to `->` in outdated output
**Current:** `bar 0.0.1 < 0.0.2`
**Proposed:** `bar 0.0.1 -> 0.0.2`

More intuitive "upgrade path" visualization.

#### 2. Replace "opam sandbox" with clearer terminology
**Current:** `opam sandbox:\n- foo.0.0.1`
**Proposed:** `- foo.0.0.1` (just list packages) or use "Packages:" header

The "opam sandbox" label is OCaml-ecosystem jargon that's confusing to newcomers.

#### 3. Add package count to lock output
**Current:**
```
Solution for dune.lock

Dependencies common to all supported platforms:
opam sandbox:
- foo.0.0.1
- bar.0.0.2
```
**Proposed:**
```
Solution for dune.lock (2 packages)

- foo.0.0.1
- bar.0.0.2
```

---

## Implementation Plan for Quick Wins

### Change 1: Replace `<` with `->` in outdated output

**File:** `src/dune_pkg/outdated.ml:159`

```ocaml
(* Current *)
; Pp.text " < "

(* Change to *)
; Pp.text " -> "
```

**Tests to update:**
- `test/blackbox-tests/test-cases/pkg/outdated.t`
- `bin/pkg/outdated.ml` (man page example on line 98)

### Change 2: Simplify "opam sandbox" terminology

**File:** `bin/pkg/pkg_common.ml:151`

```ocaml
(* Current *)
[ "duniverse (dune-built)", duniverse_pkgs; "opam sandbox", opam_pkgs ]

(* Option A: Remove category labels entirely - just list packages *)
(* Option B: Use clearer labels *)
[ "dune packages", duniverse_pkgs; "opam packages", opam_pkgs ]
```

**Also update:**
- `bin/pkg/fetch.ml:279` - "all packages use opam sandbox" message

**Tests to update:**
- Multiple tests reference "opam sandbox:" in output

### Change 3: Add package count to lock output

**File:** `bin/pkg/lock.ml:288-301`

```ocaml
(* Current *)
Pp.textf "Solution for %s" ...

(* Change to *)
let pkg_count = List.length (Lock_dir.Packages.to_pkg_list lock_dir.packages) in
Pp.textf "Solution for %s (%d package%s)" ... pkg_count (if pkg_count = 1 then "" else "s")
```

**Tests to update:**
- Many lock-related tests will need output updates

---

## Deferred Quick Wins

4. **Add timing info** ("Completed in 1.2s") - requires threading timing through output
5. **Show build progress** ("Building foo.0.0.1 (3/8)") - requires build system changes

---

## Medium Effort Improvements (Future Work)

1. **Add `dune pkg add <package>`** command
2. **Add `dune pkg tree`** dependency visualization
3. **Add color output with `--color` flag**
4. **Tabular format for outdated command**

## Larger Initiatives (Future Work)

1. **Add `dune pkg why <package>`** for dependency explanation
2. **Improve conflict error messages** with resolution suggestions
3. **Add interactive mode** for resolving version conflicts
4. **Add package info command** (`dune pkg info <package>`)

---

## Files Summary

**To modify now:**
- `src/dune_pkg/outdated.ml` - Change `<` to `->`
- `bin/pkg/outdated.ml` - Update man page example
- `bin/pkg/pkg_common.ml` - Simplify "opam sandbox" label
- `bin/pkg/fetch.ml` - Update related message
- `bin/pkg/lock.ml` - Add package count to solution header

**Tests to update:**
- `test/blackbox-tests/test-cases/pkg/outdated.t`
- Multiple lock-related tests (will need `dune promote` after changes)
