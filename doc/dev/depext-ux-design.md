# Dune Depext (External Dependencies) UX Design Document

A UX review and improvement roadmap for `dune show depexts` and external dependency management, informed by modern package managers but grounded in principled CLI design.

**Status:** Draft
**Date:** 2026-01-03

---

## Design Principles

1. **Lean defaults** — Show useful information by default. Use flags to *reduce* output, not reveal hidden features.

2. **Composability** — Output should be greppable and pipeable. Simple line format over tables.

3. **Stay in lane** — Dune should not reimplement system package manager functionality. Leave `apt install` to `apt`.

4. **Explicit over magic** — Platform detection is helpful but should be overridable. No hidden behaviour.

5. **Economy of commands** — Extend existing commands rather than adding new ones.

---

## Success Criteria

1. **Actionable output** — User can install depexts with a single copy-paste command
2. **Composable** — Script authors can extract package list without parsing: `dune show depexts --short | xargs apt install`
3. **Transparent** — User understands which packages require which depexts
4. **Cross-platform** — Same workflow works on different platforms with appropriate package names

---

## Executive Summary

Dune's depext (external/system dependencies) management is functional but minimal. The biggest gaps compared to modern package managers are:

1. **No installation assistance** - Users must manually install depexts
2. **Sparse output** - Just a list of package names with no context
3. **No platform-specific guidance** - No install commands for apt/brew/etc.
4. **Limited discoverability** - Only shown during errors or via `dune show depexts`
5. **No verification** - Can't check if depexts are already installed

---

## Current State Analysis

### Existing Commands

| Command | Description |
|---------|-------------|
| `dune show depexts` | List all external dependencies |
| (none) | No install, check, or doctor commands |

### Current Output

**`dune show depexts`:**
```
gnupg
unzip
```

Just a plain list of package names, one per line, alphabetically sorted.

**Build failure with depexts:**
```
Error: Program unknown-program not found in the tree or in PATH
 (context: default)
Hint: You may want to verify the following depexts are installed:
- unknown-package
```

The hint is helpful but appears only after a build failure.

### What Works Well

1. **Platform-aware filtering** - Shows correct package names for current OS
2. **Portable lock dirs** - Same lock file works across platforms with different depext names
3. **Conditional depexts** - Supports complex filters (os, distribution, version)
4. **Build failure hints** - Suggests depexts when builds fail

---

## Comparison with Modern Package Managers

### Cargo (Rust) - system-deps crate

Cargo itself doesn't manage system deps, but the ecosystem pattern is:
- Build scripts detect missing deps and fail with clear messages
- `pkg-config` integration for automatic detection
- Community uses README instructions

### Nix

```bash
# Declarative system dependencies
nix-shell -p openssl pkg-config
# Or in flake.nix:
buildInputs = [ openssl pkg-config ];
```

Nix fully manages system deps declaratively.

### uv (Python)

```bash
uv pip install --help  # Shows system requirements
# Clear error messages pointing to system packages
```

### Homebrew Bundle

```bash
brew bundle          # Install from Brewfile
brew bundle check    # Verify all deps installed
brew bundle list     # Show what would be installed
```

### apt/dnf patterns

```bash
apt install build-essential  # Install by name
apt-cache search pkg-config  # Search for packages
dpkg -l | grep pkg-config    # Check if installed
```

---

## Detailed Observations

### 1. No Install Guidance by Default

**Current:** Users must manually figure out install commands.

**Issue:** Output is just package names with no actionable next step.

**Solution:** Show install command by default (auto-detect platform).

---

### 2. Sparse Output Format

**Current:**
```
gnupg
unzip
```

**Issues:**
- No indication which package requires each depext
- No install instructions
- No indication if already installed

**Solution:** Make rich output the default. Use `--short` for minimal output.

---

### 3. No Verification Command

**Current:** No way to check if depexts are installed.

**Homebrew Bundle pattern:**
```bash
brew bundle check
# The Brewfile dependencies are satisfied.
# or
# Some dependencies are missing:
#   gnupg
```

**Recommendation:** Add `dune pkg check-depexts` or `dune doctor` command.

---

### 4. Platform Detection Could Be More Helpful

**Current:** Uses environment variables for platform:
```bash
DUNE_CONFIG__OS=linux DUNE_CONFIG__OS_FAMILY=debian dune show depexts
```

**Issues:**
- Users must know to set these variables
- No auto-detection of current platform for the command

**Recommendation:** Auto-detect platform; only require env vars for cross-platform queries.

---

### 5. Help Text is Minimal

**Current `dune show depexts --help`:**
```
Print the list of all the available depexts
```

**Issues:**
- No examples
- No explanation of what depexts are
- No mention of platform-specific behavior

**Recommendation:** Add comprehensive help with examples.

---

### 6. Missing Integration with Package Managers

**Current:** No integration with system package managers.

**Potential integrations:**
- Detect which package manager is available (apt, brew, dnf, pacman)
- Generate appropriate install commands
- Optionally verify packages are installed

---

### 7. Error Message Should Include Install Command

**Current:**
```
Hint: You may want to verify the following depexts are installed:
- gnupg
- unzip
```

**Solution:** Auto-detect platform and show single actionable command:
```
Hint: Missing system dependencies: gnupg, unzip
      Required by: foo.0.0.1

      To install (detected: Homebrew):
        brew install gnupg unzip
```

---

## CLI Design Philosophy

**Principle:** Show useful information by default. Use flags to *reduce* output, not to reveal hidden features.

| Approach | Flag | Default |
|----------|------|---------|
| **Lean (good)** | `--short` reduces | Full info shown |
| **Hidden (bad)** | `--verbose` reveals | Minimal shown |
| **Machine** | `--json` | Human-readable |
| **Filter** | `--platform=X` | Current platform |

---

## Proposed Default Output

**`dune show depexts` (new default):**

```
gnupg       (foo.0.0.1)
unzip       (foo.0.0.1)
libssl-dev  (bar.0.0.2)

brew install gnupg unzip libssl-dev
```

**Design rationale:**
- Lean format: 5 lines for 3 packages (not 7+ with headers)
- Still greppable: `dune show depexts | grep ssl`
- Origin shown inline, not verbose multi-line
- Install command at bottom, ready to copy-paste
- No decorative headers ("System dependencies:")

**Key changes from current:**
- Shows which package requires each depext (no `--verbose` needed)
- Auto-detects platform and shows install command
- Actionable output by default

---

## Quick Wins (Low Effort, High Impact)

### 1. Enrich default output (no new flags needed)

**Current:**
```
$ dune show depexts
gnupg
unzip
```

**Proposed (new default):**
```
$ dune show depexts
gnupg  (foo.0.0.1)
unzip  (foo.0.0.1)

apt install gnupg unzip
```

Users get full context immediately. Lean format, still greppable.

### 2. Add `--short` flag for scripting

**For scripts/CI that need just package names:**
```bash
$ dune show depexts --short
gnupg
unzip

# Useful for:
apt install $(dune show depexts --short | tr '\n' ' ')
```

### 3. Add `--json` flag for tooling

```bash
$ dune show depexts --json
{
  "depexts": [
    {"name": "gnupg", "required_by": ["foo.0.0.1"]},
    {"name": "unzip", "required_by": ["foo.0.0.1"]}
  ],
  "install_cmd": "apt install gnupg unzip"
}
```

### 4. Improve help text

```
NAME
    dune-show-depexts - Print external system dependencies

DESCRIPTION
    Print system packages required by the project's dependencies.
    Shows which package requires each dependency and suggests an
    install command for your platform.

OPTIONS
    --short
        Print only package names, one per line (for scripting).

    --json
        Output as JSON (for tooling integration).

    --pm=MANAGER
        Use specified package manager (apt, brew, dnf, pacman, nix).
        Default: auto-detect from environment.

    --context=NAME
        Use the specified build context.

EXAMPLES
    $ dune show depexts
    libssl-dev  (tls.0.17.0)

    apt install libssl-dev

    $ dune show depexts --short | xargs apt install

    $ dune show depexts --pm=brew
    openssl  (tls.0.17.0)

    brew install openssl
```

---

## Medium Effort Improvements

### 1. Add `--pm` flag for explicit package manager

Platform auto-detection is helpful but can be wrong (containers, multiple package managers). Make it overridable:

```bash
$ dune show depexts              # auto-detect: apt
apt install gnupg unzip

$ dune show depexts --pm=brew    # explicit override
brew install gnupg unzip

$ dune show depexts --pm=nix     # for Nix users
nix-shell -p gnupg unzip
```

### 2. Better error messages during build

Include install commands in build failure hints.

### 3. Platform auto-detection

Detect current OS/distribution automatically instead of requiring env vars. But always show what was detected so users can override if wrong.

---

## Larger Initiatives (Needs Justification)

These require user research to justify the added complexity:

### 1. Nix/Docker integration

Generate Nix expressions or Dockerfiles with system dependencies. This could be valuable for CI reproducibility.

```bash
$ dune show depexts --nix
# outputs nix expression
```

## Explicitly Rejected

These don't fit Dune's scope or design principles:

### 1. `dune pkg install-depexts` — Scope creep

Installing system packages requires `sudo`. Dune should not be in the business of running privileged operations. This is what `opam depext` tried to do and it's a maintenance nightmare across distributions.

**Instead:** Provide a copy-paste command. Users run it themselves.

### 2. `dune pkg check-depexts` — Reimplementing system tools

Checking if packages are installed is the job of the system package manager. Dune reimplementing this is:
- Inevitably incomplete (different detection per distro)
- Maintenance burden
- Potentially wrong (package installed but broken)

**Instead:** Document how to check with native tools:
```bash
# Debian/Ubuntu
dpkg -l $(dune show depexts --short) 2>/dev/null | grep -v '^ii'

# macOS
brew list $(dune show depexts --short) 2>/dev/null
```

Unix composability over reinvention.

---

## Files to Modify

**For quick wins:**
- `bin/describe/describe_depexts.ml` — Add `--short`, `--json`, `--pm` flags; enrich default output
- `src/dune_rules/run_with_path.ml` — Improve error hints with install commands
- `src/dune_rules/pkg_rules.ml` — Add depext origin tracking

**For platform detection:**
- `src/dune_pkg/platform_detect.ml` (new) — Detect package manager from environment

---

## Implementation Plan

### Change 1: Enrich default output

**File:** `bin/describe/describe_depexts.ml`

Replace minimal output with rich, actionable default:

```ocaml
type output_format = Default | Short | Json

let term =
  let+ builder = Common.Builder.term
  and+ context_name = Common.context_arg ~doc:(Some "Build context to use.")
  and+ format =
    let short = Arg.(value & flag & info ["short"]
      ~doc:"Print only package names, one per line (for scripting).") in
    let json = Arg.(value & flag & info ["json"]
      ~doc:"Output as JSON (for tooling integration).") in
    Term.(const (fun s j -> if s then Short else if j then Json else Default)
          $ short $ json)
  in
  ...
```

**File:** `src/dune_rules/pkg_rules.ml`

Add function to get depexts with their source packages:

```ocaml
val all_depexts_with_origins : Context_name.t -> (string * Package_name.t list) list Memo.t
```

### Change 2: Add platform detection for install commands

**New file:** `src/dune_pkg/platform_detect.ml`

```ocaml
type package_manager =
  | Apt
  | Brew
  | Dnf
  | Pacman
  | Unknown

val detect : unit -> package_manager
val install_command : package_manager -> string list -> string
```

### Change 3: Improve build error hints

**File:** `src/dune_rules/run_with_path.ml`

Enhance `depexts_hint` to auto-detect platform and show install command:

```ocaml
let depexts_hint depexts =
  match depexts with
  | [] -> None
  | depexts ->
    let pm = Platform_detect.detect () in
    let install_cmd = Platform_detect.install_command pm depexts in
    Some (Pp.concat ~sep:Pp.cut [
      Pp.text "Missing system dependencies:";
      Pp.enumerate ~f:Pp.verbatim depexts;
      Pp.textf "To install (detected: %s):" (Platform_detect.name pm);
      Pp.verbatim ("  " ^ install_cmd)
    ])
```

### Change 4: Update help text

**File:** `bin/describe/describe_depexts.ml`

```ocaml
let info =
  let doc = "Print external system dependencies" in
  let man = [
    `S "DESCRIPTION";
    `P "Print system packages required by the project's dependencies.";
    `P "By default, shows which packages require each dependency and \
        suggests installation commands for your platform.";
    `S "OPTIONS";
    `S "EXAMPLES";
    `Pre "$ dune show depexts";
    `Pre "System dependencies:";
    `Pre "  libssl-dev  required by tls.0.17.0";
    `Pre "";
    `Pre "To install (detected: Ubuntu):";
    `Pre "  apt install libssl-dev";
  ] in
  Cmd.info "depexts" ~doc ~man
```

---

## Example Output After Improvements

### Default output (lean, actionable):
```
$ dune show depexts
gnupg       (foo.0.0.1)
unzip       (foo.0.0.1)
libssl-dev  (bar.0.0.2)

brew install gnupg unzip libssl-dev
```

### Short mode (for scripting):
```
$ dune show depexts --short
gnupg
libssl-dev
unzip

# Pipe to package manager:
$ dune show depexts --short | xargs brew install
```

### With explicit package manager:
```
$ dune show depexts --pm=apt
gnupg       (foo.0.0.1)
unzip       (foo.0.0.1)
libssl-dev  (bar.0.0.2)

apt install gnupg unzip libssl-dev
```

### JSON mode (for tooling):
```
$ dune show depexts --json
{
  "platform": {"os": "macos", "pm": "brew"},
  "depexts": [
    {"name": "gnupg", "required_by": ["foo.0.0.1"]},
    {"name": "unzip", "required_by": ["foo.0.0.1"]},
    {"name": "libssl-dev", "required_by": ["bar.0.0.2"]}
  ],
  "install": {"command": "brew", "args": ["install", "gnupg", "unzip", "libssl-dev"]}
}
```

Note: `install` is structured (command + args), not a string, for proper machine parsing.

### Build failure (improved):
```
Error: Program pkg-config not found in the tree or in PATH
 (context: default)

Hint: Missing system dependency: pkg-config (conf-pkg-config.2)
      apt install pkg-config
```

### No depexts case:
```
$ dune show depexts
No system dependencies required.
```

