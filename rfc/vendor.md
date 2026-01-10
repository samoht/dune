# [RFC] Extended Vendor Stanza

## Summary

Add a `(vendor ...)` stanza that extends `(vendored_dirs)` with selective library
exposure, library aliasing, and support for non-dune packages.

## Workflow

The workflow is simple: copy source code into a directory and run `dune build`.

```
my-project/
  src/
  vendor/
    fmt.0.9.0/       # copied from somewhere
    yojson.2.0.0/    # copied from somewhere else
  dune-project
```

Dune doesn't care where the source came from or how it got there. It just builds
what's in the directory.

## Background

### Current State

Dune supports vendoring via `(vendored_dirs <dirs>)`. This marks directories as
vendored (disables warnings-as-errors) and includes them in the build.

However, `(vendored_dirs)` exposes all libraries in a directory. There is no way to:
1. Expose only a subset of libraries
2. Vendor multiple versions of the same library
3. Build non-dune packages

### Prior Art: opam-monorepo

[opam-monorepo](https://github.com/tarides/opam-monorepo) established a vendoring
workflow for OCaml, fetching dependencies into a `duniverse/` directory. It has
known limitations:

- [Issue #145](https://github.com/tarides/opam-monorepo/issues/145): Sources from
  the same git repository but with different versions cause invalid solutions.

- All sources must build with dune. Non-dune sources require
  [opam-overlays](https://github.com/dune-universe/opam-overlays).

### The Problem

**Multiple versions conflict:**
```
vendor/
  yojson.1.7.0/    # contains library "yojson"
  yojson.2.0.0/    # contains library "yojson" → conflict
```

**Monorepo version skew:**
```
vendor/
  core.0.16.0/    # contains core, core_kernel, core_unix, etc.
```

Libraries from a monorepo may have different release schedules. You might need
`core` from one commit and `core_unix` from another. With `(vendored_dirs)`,
you cannot pick libraries from different directories.

**Non-dune source:**
```
vendor/
  zarith.1.14/   # uses ./configure && make
```

## Proposed Solution

### Syntax

```dune
(vendor <directory>
 (libraries <lib-spec>...)    ; optional
 (packages <pkg-spec>...)     ; optional
 (mode <build-mode>))         ; optional: dune | opam
```

Where `<lib-spec>` is:
- `<name>` — expose library with its original name
- `(<name> :as <alias>)` — expose library under a different name

All fields are optional. When omitted:
- `(libraries ...)` and `(packages ...)` should be auto-detected by scanning the directory
- `(mode ...)` should be auto-detected: `dune` if dune files exist, `opam` otherwise

### Examples

**Expose all libraries (equivalent to vendored_dirs):**
```dune
(vendor vendor/fmt.0.9.0)
```

**Selective exposure:**
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

**Non-dune source:**
```dune
(vendor vendor/zarith.1.14)
```
Dune detects the opam file and runs its build commands.

**Force opam mode:**
```dune
(vendor vendor/foo.1.0.0
 (mode opam))
```

### Semantics

**Library filtering:**
1. Scan directory for libraries (existing behavior)
2. If `(libraries ...)` specified, expose only those listed
3. Unlisted libraries should not be built and cannot be used as dependencies

**Library aliasing:**
- `(<name> :as <alias>)` should change the public name used in `(libraries ...)` stanzas
- OCaml module names inside the library should remain unchanged

**Error handling:**
- If `(libraries foo)` lists a library not found in the directory → should error at parse time
- If two `(vendor)` stanzas expose the same library name → should error at parse time
- Windows paths and paths with spaces should be supported

**Build mode:**

| Directory Contents | Default Mode |
|--------------------|--------------|
| Contains `dune-project` or `dune` files | `dune` |
| Contains only `*.opam` file | `opam` |

In `opam` mode, dune should provide full compatibility with opam's build semantics:

**Variable expansion:**
All opam variables should be supported, including:
- Global: `%{make}%`, `%{jobs}%`, `%{arch}%`, `%{os}%`, `%{os-family}%`, `%{os-distribution}%`, `%{os-version}%`
- Directories: `%{prefix}%`, `%{lib}%`, `%{bin}%`, `%{share}%`, `%{etc}%`, `%{doc}%`, `%{man}%`, `%{stublibs}%`
- Package: `%{name}%`, `%{version}%`, `%{build}%`, `%{build-id}%`
- Cross-package: `%{pkg:var}%`, `%{pkg:installed}%`, `%{pkg:enable}%`

**Filters:**
Conditional commands should be evaluated: `[make] {os = "linux"}`, `[nmake] {os = "win32"}`

**Substs:**
Files listed in `substs:` should be processed (e.g., `config.ml.in` → `config.ml`)

**Patches:**
Patches listed in `patches:` should be applied, including conditional patches

**Environment:**
- `build-env:` should set environment for build commands
- `setenv:` should export variables to dependent packages

**Build location:**
- Should build in `_build/.pkgs/<context>/<name>.<version>-<build-id>/`
- Should install to `_build/install/<context>/`

## Use Cases

### Gradual Migration

Keep two versions of a library during migration:

```dune
(vendor vendor/yojson.1.7.0
 (libraries (yojson :as yojson_v1)))

(vendor vendor/yojson.2.0.0
 (libraries yojson))
```

### Cherry-Picking from Monorepos

Pick specific libraries from different copies of a monorepo:

```dune
(vendor vendor/core.0.16.1
 (libraries core core_kernel))

(vendor vendor/core_unix.0.16.0
 (libraries core_unix))
```

### Non-Dune Source

```dune
(vendor vendor/zarith.1.14)
```

### Hiding Internal Libraries

Expose only public libraries:

```dune
(vendor vendor/lwt.5.7.0
 (libraries lwt lwt.unix))

(vendor vendor/async.0.16.0
 (libraries async async_kernel))
```

## Compatibility

`(vendored_dirs)` continues to work. These are equivalent:

```dune
(vendored_dirs vendor/foo.1.0.0)
(vendor vendor/foo.1.0.0)
```

No migration required.

## Alternatives Considered

**Extend `(vendored_dirs)` syntax:**
```dune
(vendored_dirs
 (dir vendor/fmt.0.9.0 (libraries fmt)))
```
Rejected: mixes listing with configuration, harder to read.

**Per-directory config file:**
```
vendor/fmt.0.9.0/.dune-vendor
```
Rejected: splits configuration, requires modifying vendored sources.

**Library-level aliasing:**
```dune
(library
 (name yojson)
 (vendor_alias yojson_v1))
```
Rejected: requires modifying vendored dune files.

## Open Questions

1. **Should `(vendor)` imply `(vendored_dirs)`?**
   Proposed: yes.

2. **Aliasing syntax?**
   Current: `(libraries (yojson :as yojson_v1))`
   Alternative: `(libraries (yojson -> yojson_v1))`

## References

- [opam-monorepo](https://github.com/tarides/opam-monorepo)
- [opam-monorepo #145](https://github.com/tarides/opam-monorepo/issues/145)
- [dune-universe/opam-overlays](https://github.com/dune-universe/opam-overlays)
