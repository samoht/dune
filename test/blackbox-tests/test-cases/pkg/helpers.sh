export XDG_CACHE_HOME="$PWD/.cache"

# Set the default platform for the purposes of solving dependencies so that the
# output of tests is platform-independent.
export DUNE_CONFIG__OS=linux
export DUNE_CONFIG__ARCH=x86_64
export DUNE_CONFIG__OS_FAMILY=debian
export DUNE_CONFIG__OS_DISTRIBUTION=ubuntu
export DUNE_CONFIG__OS_VERSION=24.11
export DUNE_CONFIG__SYS_OCAML_VERSION=5.4.0+fake

dune="dune"

pkg_root="_build/.pkgs/default"

default_lock_dir="dune.lock"
source_lock_dir="${default_lock_dir}"

# Prints the directory containing the package target and source dirs within the
# _build directory.
get_build_pkg_dir() {
  package_name=$1
  digest=$($dune pkg print-digest $package_name)
  status=$?
  if [ "$status" -eq "0" ]; then
    echo "$pkg_root/$digest"
  else
    return 1
  fi
}

build_pkg() {
  prefix=$(get_build_pkg_dir $1)
  status=$?
  if [ "$status" -eq "0" ]; then
    $dune build "$prefix/target"
  else
    return 1
  fi
}

show_pkg() {
  prefix="$(get_build_pkg_dir $1)"
  find "$prefix" | sort | dune_cmd subst "$prefix" ""
}

strip_sandbox() {
  dune_cmd subst '[^ ]*.sandbox/[^/]+' '$SANDBOX'
}

show_pkg_targets() {
  # Packages now install to the shared _build/install/default/ directory
  # Filter to only show files related to this package
  pkg_name=$1
  prefix="_build/install/default"
  find "$prefix" 2>/dev/null | grep -E "/$pkg_name[/.]" | sort | dune_cmd subst "$prefix" ""
}

show_pkg_cookie() {
  $dune internal dump "$(get_build_pkg_dir $1)/target/cookie"
}

mock_packages="mock-opam-repository/packages"

mkrepo() {
  mkdir -p $mock_packages
  # Initialize as git repo for single-file lock format (needs commit hashes)
  (cd mock-opam-repository && git init -q && git config user.email "test@test" && git config user.name "test")
}

mkpkg() {
  name=$1
  if [ "$#" -eq "1" ]
  then
    version="0.0.1"
  else
    version="$2"
  fi
  mkdir -p $mock_packages/$name/$name.$version
  echo 'opam-version: "2.0"' > $mock_packages/$name/$name.$version/opam
  cat >>$mock_packages/$name/$name.$version/opam
  # Commit the package so single-file format can derive from repo hash
  (cd mock-opam-repository && git add -A && git commit -q -m "Add $name.$version")
}

mk_ocaml() {
  version="$1"
  major=$(echo "$version" | cut -d. -f1)
  minor=$(echo "$version" | cut -d. -f2)
  patch=$(echo "$version" | cut -d. -f3)
  next=$(echo "$patch + 1" | bc)
  constraint="{>= \"$major.$minor.$patch~\" & < \"$major.$minor.$next~\"}"

  mkpkg ocaml $version << EOF
  depends: [
   "ocaml-base-compiler" $constraint |
   "ocaml-variants" $constraint
  ]
EOF

  mkpkg ocaml-base-compiler $version << EOF
  depends: [
   "ocaml-compiler" $constraint
  ]
  flags: compiler
  conflict-class: "ocaml-core-compiler"
EOF

  mkpkg ocaml-variants $version << EOF
  depends: [
   "ocaml-compiler" {= "$version"}
  ]
  flags: compiler
  conflict-class: "ocaml-core-compiler"
EOF

  mkpkg ocaml-compiler $version << EOF
  depends: [
   "ocaml" {= "$version" & post}
  ]
EOF
}

set_pkg_to () {
  local value="${1}"
  if grep "(pkg .*)" dune-workspace > /dev/null; then
    dune_cmd substitute "(pkg .*)" "(pkg ${value})" dune-workspace
  else
    echo "(pkg ${value})" >> dune-workspace
  fi
}

enable_pkg() {
  set_pkg_to "enabled"
}

disable_pkg() {
  set_pkg_to "disabled"
}

unset_pkg() {
  dune_cmd delete "\(pkg" dune-workspace
}

add_mock_repo_if_needed() {
  # Use git+file:// for single-file lock format (needs repo hash for derivation)
  repo="${1:-git+file://$(pwd)/mock-opam-repository}"

  if [ ! -e dune-workspace ]
  then
      cat >dune-workspace <<EOF
(lang dune 3.20)
(lock_dir
 (repositories mock))
(repository
 (name mock)
 (url "${repo}"))
EOF
  else
    if ! grep '(name mock)' dune-workspace > /dev/null
    then
      # add the repo definition
      cat >>dune-workspace <<EOF
(repository
 (name mock)
 (url "${repo}"))
EOF
 
      # reference the repo - only add lock_dir if no existing lock_dir references mock
      if ! grep '(repositories' dune-workspace | grep 'mock' > /dev/null
      then
        cat >>dune-workspace <<EOF
(lock_dir
 (repositories mock))
EOF
      fi
  
    fi
  fi
}

create_mock_repo() {
  # Always create a fresh workspace with mock repository configuration
  repo="${1:-file://$(pwd)/mock-opam-repository}"
  cat >dune-workspace <<EOF
(lang dune 3.20)
(lock_dir
 (repositories mock))
(repository
 (name mock)
 (url "${repo}"))
EOF
}

make_lockpkg() {
  mkdir -p "${source_lock_dir}"
  local f="${source_lock_dir}/$1.pkg"
  cat > "$f"
}

append_to_lockpkg() {
  local pkg="${1}"
  cat >> "${source_lock_dir}/${pkg}.pkg"
}

make_lockpkg_file() {
  local pkg="${1}"
  local filename="${2}"
  mkdir -p "${source_lock_dir}/${pkg}.files"
  cat > "${source_lock_dir}/${pkg}.files/${filename}"
}

dune_pkg_lock_normalized() {
  out="$(mktemp)"
  # Use dune's default format (single-file).
  # Tests needing directory format should use --format=directory explicitly.
  # DUNE_TEST_LOCK_FORMAT env var can override for backward compatibility.
  local args="$@"
  local format_arg=""
  if ! echo "$args" | grep -q '\-\-format'; then
    local format="${DUNE_TEST_LOCK_FORMAT:-}"
    if [ -n "$format" ]; then
      format_arg="--format=$format"
    fi
  fi
  if dune pkg lock $format_arg $@ 2>> "${out}"; then
    processed="$(mktemp)"
    if [ "$DUNE_CONFIG__PORTABLE_LOCK_DIR" = "disabled" ]; then
      cp "${out}" "${processed}"
    else
      cat "${out}" \
        | awk '/Solution/{printf"%s:\n",$0;f=0};f{print};/Dependencies.*:/{f=1}' \
        | dune_cmd subst '\(none\)' '(no dependencies to lock)' \
        > "${processed}"
    fi
    cat "${processed}"
  else
    processed="$(mktemp)"
    cat "${out}" \
      | dune_cmd delete-between 'The dependency solver failed to find a solution for the following platforms:' '\.\.\.with this error:' \
      > "${processed}"
    cat "${processed}"
    return 1
  fi
}

solve_project() {
  cat >dune-project
  add_mock_repo_if_needed
  dune_pkg_lock_normalized $@
}

make_lockdir() {
  mkdir -p "${source_lock_dir}"
  cat > "${source_lock_dir}"/lock.dune <<EOF
(lang package 0.1)
(repositories (complete true))
EOF
}

make_project() {
  cat <<EOF
(lang dune 3.20)
 (package
  (name x)
  (allow_empty)
  (depends $@))
EOF
}

print_source() {
  cat "${default_lock_dir}"/"$1".pkg \
  | dune_cmd print-from 'source' \
  | dune_cmd print-until '^$' \
  | dune_cmd subst "$PWD" "PWD" \
  | tr '\n' ' ' \
  | tr -s " " \
  | dune_cmd subst '\s$' ''
}

solve() {
  local format_arg=""
  local pkgs=""
  for arg in "$@"; do
    case "$arg" in
      --format=*)
        format_arg="$arg"
        ;;
      *)
        pkgs="$pkgs $arg"
        ;;
    esac
  done
  make_project $pkgs | solve_project $format_arg
}

# Pass a string of the form PACKAGE_NAME.PACKAGE_VERSION and replaces the
# hashes in all package digests matching the specified package with
# "DIGEST_HASH". Use this for packages whose lockfiles have different contents
# on different machines, such as lockfiles generated by expanding the "$PWD"
# variable.
sanitize_pkg_digest() {
  local pkg_name_and_version="${1}"
  dune_cmd subst "$pkg_name_and_version-[0-9a-f]*" "$pkg_name_and_version-DIGEST_HASH"
}
