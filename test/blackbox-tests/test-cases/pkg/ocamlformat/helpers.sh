dev_tool_lock_dir="_build/.locks/tools-ocamlformat"

make_fake_ocamlformat() {
  version=$1
  if [ "$#" -eq "1" ]
  then
    ml_file=""
  else
    ml_file="$2"
  fi
  mkdir ocamlformat
  cat > ocamlformat/dune-project <<EOF
(lang dune 3.13)
(package (name ocamlformat))
EOF
  if [ ! "$ml_file" = "no-ml-file" ]
  then
    cat > ocamlformat/ocamlformat.ml <<EOF
let version = "$version"
let () =
  if Sys.file_exists ".ocamlformat-ignore" then
  print_endline "ignoring some files"
;;
let () = print_endline ("formatted with version "^version)
EOF
  fi
  cat > ocamlformat/dune <<EOF
(executable
 (public_name ocamlformat))
EOF
  tar cf ocamlformat-$version.tar ocamlformat
  rm -rf ocamlformat
}

make_ocamlformat_opam_pkg() {
  version=$1
  if [ "$#" -eq "2" ]
  then
    port="$2"
  else
    port=""
  fi
  # Use a fixed checksum for predictable cache keys in tests.
  # The checksum is version-dependent so different versions get different cache keys.
  # We put the version at the START so the 8-char prefix is unique per version.
  checksum="md5=${version//./}00000000000000000000000000"
  if [ ! "$port" = "" ]
  then
    mkpkg ocamlformat $version <<EOF
build: [
  [
     "dune"
     "build"
     "-p"
     name
     "@install"
  ]
]
url {
  src: "http://127.0.0.1:$port"
  checksum: [
    "$checksum"
  ]
}
EOF
  else
    mkpkg ocamlformat $version <<EOF
build: [
  [
     "dune"
     "build"
     "-p"
     name
     "@install"
  ]
]
url {
  src: "file://$PWD/ocamlformat-$version.tar"
  checksum: [
    "$checksum"
  ]
}
EOF
  fi
}

make_project_with_dev_tool_lockdir() {
  cat > dune-project <<EOF
(lang dune 3.13)
(package
 (name foo))
EOF
  cat > foo.ml <<EOF
let () = print_endline "Hello, world"
EOF
  cat > dune <<EOF
(executable
 (public_name foo))
EOF
  cat > dune-workspace <<EOF
(lang dune 3.20)

(lock_dir
 (path "${dev_tool_lock_dir}")
 (repositories mock))

(lock_dir
  (repositories mock))

(repository
 (name mock)
 (url "git+file://$(pwd)/mock-opam-repository"))
EOF
}


make_printer_lib() {
  version=$1
  mkdir printer
  cat > printer/dune-project <<EOF
(lang dune 3.13)
(package (name printer))
EOF
  if [ $version = "1.0" ]
  then
  cat > printer/printer.ml <<EOF
let print () = print_endline "formatted"
EOF
  else
  cat > printer/printer.ml <<EOF
let print () = print_endline "Hello World!"
EOF
  fi
  cat > printer/dune <<EOF
(library
 (public_name printer))
EOF
  tar cf printer.$version.tar printer
  rm -r printer
}

make_opam_printer() {
  version=$1
  mkpkg printer $version <<EOF
build: [
   [
    "dune"
    "build"
    "-p"
    name
    "@install"
   ]
 ]
 url {
 src: "file://$PWD/printer.$version.tar"
 checksum: [
  "md5=$(md5sum printer.$version.tar | cut -f1 -d' ')"
 ]
}
EOF
}

make_fake_ocamlformat_from_path() {
  mkdir .bin
  cat > .bin/ocamlformat <<EOF
#!/bin/sh
if [ -f ".ocamlformat-ignore" ]
then
  echo "ignoring some files"
fi
echo "fake ocamlformat from PATH"
EOF
  chmod +x .bin/ocamlformat
  PATH=$PWD/.bin:$PATH
}
