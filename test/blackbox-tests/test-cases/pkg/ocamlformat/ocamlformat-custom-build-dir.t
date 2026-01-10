Checks whether dev-tool locking takes custom build directories correctly into account.

Set up some ocamlformat that we want to install.

  $ ocamlformat_version="0.26.2"
  $ make_fake_ocamlformat "${ocamlformat_version}"
  $ make_ocamlformat_opam_pkg "${ocamlformat_version}"

  $ cat > .ocamlformat <<EOF
  > version = ${ocamlformat_version}
  > EOF

Override the build directory that we want to build in. We do this by replacing
the build directory in `$dev_tool_lock_dir` with our custom build directory.

  $ default_build_dir="_build"
  $ custom_build_dir="_other_build"
  $ default_dev_tool_lock_dir="${dev_tool_lock_dir}"
  $ dev_tool_lock_dir=$(echo "${dev_tool_lock_dir}" | dune_cmd subst "^$default_build_dir" "$custom_build_dir")

Create a configuration with this custom build directory

  $ make_project_with_dev_tool_lockdir
  $ enable_pkg

Make sure we don't have a lock dir

  $ [ -e "${dev_tool_lock_dir}"/lock.dune ] || echo "Lock dir does not exist in custom location"
  Lock dir does not exist in custom location
  $ [ -e "${default_dev_tool_lock_dir}"/lock.dune ] || echo "Lock dir does not exist in default location"
  Lock dir does not exist in default location

Install our fake ocamlformat, making sure to override the build directory.

  $ dune tools install ocamlformat --build-dir="${custom_build_dir}"
  Solution for _other_build/.locks/tools-ocamlformat (41 packages)
  dune:
  - base.v0.17.3
  - camlp-streams.5.0.1
  - csexp.1.5.2
  - dune-build-info.3.20.2
  - dune-configurator.3.20.2
  - either.1.0.0
  - fix.20250919
  - menhir.20250912
  - menhirCST.20250912
  - menhirLib.20250912
  - menhirSdk.20250912
  - ocaml-version.4.0.3
  - ocaml_intrinsics_kernel.v0.17.1
  - ocamlformat.0.28.1
  - ocamlformat-lib.0.28.1
  - ocp-indent.1.9.0
  - re.1.14.0
  - sexplib0.v0.17.0
  - stdio.v0.17.0
  
  opam:
  - astring.0.8.5
  - base-bigarray.base
  - base-domains.base
  - base-effects.base
  - base-nnp.base
  - base-threads.base
  - base-unix.base
  - cmdliner.2.1.0
  - compiler-cloning.enabled
  - fpath.0.7.3
  - ocaml.5.4.1
  - ocaml-base-compiler.5.4.1
  - ocaml-compiler.5.4.1
  - ocaml-config.3
  - ocaml-options-vanilla.1
  - ocamlbuild.0.16.1+dune
  - ocamlfind.1.9.8+dune
  - relocatable-compiler.5.4.1.20251109.1
  - topkg.1.1.1
  - uucp.17.0.0
  - uuseg.17.0.0
  - uutf.1.0.4
  File "relocatable-compiler.pkg", line 139, characters 3-89:
  Error: Download failed with code 404
         
  File "relocatable-compiler.pkg", line 147, characters 4-112:
  Error: Download failed with code 404
         
  [1]

This should've worked and picked up our ocamlformat using the lock dir
configuration from the dune-workspace. But also, we should now have a lock dir
at the right location, in our custom build dir.

  $ [ -e "${dev_tool_lock_dir}"/lock.dune ] && echo "Lock dir created in the correct, custom location"
  Lock dir created in the correct, custom location
  $ [ -e "${default_dev_tool_lock_dir}"/lock.dune ] || echo "Lock dir does not exist in default location"
  Lock dir does not exist in default location
