open Import

(* Global cache for dev tools.

   This module provides caching for dev tools to enable:
   1. Fast reinstall after dune clean (cache survives clean)
   2. Cross-workspace sharing (same binary used across projects)

   Cache structure:
   - ~/.cache/dune/tools/{cache_key}/bin/{exe_name}

   Cache keys:
   - Compiler-independent tools: {package_name}.{version}
     Examples: ocamlformat.0.26.2
   - Compiler-dependent tools: {package_name}.{version}-{ocaml_version}-{build_id_short}
     Examples: odoc.0.0.1-5.2.0-a1b2c3d4

   The build_id ensures that tools are recompiled if the OCaml compiler
   was built differently (different configure options, dependencies, etc). *)

type ocaml_compiler_info =
  { version : string
  ; build_id : Dune_digest.t
  }

let base_dir =
  lazy
    (let dir = Path.relative (Lazy.force Dune_util.cache_root_dir) "tools" in
     Path.as_outside_build_dir_exn dir)
;;

let base_dir () =
  let base_dir = Lazy.force base_dir in
  let path = Path.outside_build_dir base_dir in
  if not (Path.Untracked.exists path) then Path.mkdir_p path;
  if not (Path.Untracked.is_directory path)
  then
    User_error.raise
      [ Pp.textf "Expected %s to be a directory but it is not." (Path.to_string path) ];
  base_dir
;;

(* Compute the cache key for a dev tool.
   - Compiler-independent: {package_name}.{version}
   - Compiler-dependent: {package_name}.{version}-{ocaml_version}-{build_id_short} *)
let cache_key ~dev_tool ~version ~ocaml_compiler =
  let pkg_name = Dune_pkg.Dev_tool.package_name dev_tool in
  let base = sprintf "%s.%s" (Package.Name.to_string pkg_name) version in
  match Dune_pkg.Dev_tool.needs_to_build_with_same_compiler_as_project dev_tool with
  | false -> base
  | true ->
    (match ocaml_compiler with
     | None -> base
     | Some { version = ocaml_ver; build_id } ->
       (* Use first 8 chars of build_id for readability while maintaining uniqueness *)
       let build_id_short = String.sub (Dune_digest.to_string build_id) ~pos:0 ~len:8 in
       sprintf "%s-%s-%s" base ocaml_ver build_id_short)
;;

(* Get the cache directory for a dev tool *)
let cache_dir ~dev_tool ~version ~ocaml_compiler =
  let key = cache_key ~dev_tool ~version ~ocaml_compiler in
  Path.Outside_build_dir.relative (base_dir ()) key
;;

(* Get the installation prefix within the cache directory *)
let installation_prefix ~dev_tool ~version ~ocaml_compiler =
  cache_dir ~dev_tool ~version ~ocaml_compiler
;;

(* Get the path to a specific executable in the cache *)
let exe_path ~dev_tool ~version ~ocaml_compiler =
  let prefix = installation_prefix ~dev_tool ~version ~ocaml_compiler in
  let exe_components = Dune_pkg.Dev_tool.exe_path_components_within_package dev_tool in
  List.fold_left exe_components ~init:prefix ~f:Path.Outside_build_dir.relative
;;

(* Check if a dev tool is already installed in the cache *)
let is_installed ~dev_tool ~version ~ocaml_compiler =
  let exe = exe_path ~dev_tool ~version ~ocaml_compiler in
  Path.Untracked.exists (Path.outside_build_dir exe)
;;

(* Get the cache directory path for a dev tool (as Path.t) *)
let cache_dir_path ~dev_tool ~version ~ocaml_compiler =
  Path.outside_build_dir (cache_dir ~dev_tool ~version ~ocaml_compiler)
;;

(* Copy built dev tool to global cache.
   This should be called after the dev tool is successfully built. *)
let populate_cache ~dev_tool ~version ~ocaml_compiler ~source_dir =
  let cache_target = cache_dir_path ~dev_tool ~version ~ocaml_compiler in
  Path.mkdir_p cache_target;
  (* Copy the bin directory *)
  let source_bin = Path.relative source_dir "bin" in
  let target_bin = Path.relative cache_target "bin" in
  if Path.Untracked.exists source_bin
  then (
    Path.mkdir_p target_bin;
    let exe_name = Dune_pkg.Dev_tool.exe_name dev_tool in
    let source_exe = Path.relative source_bin exe_name in
    let target_exe = Path.relative target_bin exe_name in
    if Path.Untracked.exists source_exe
    then Io.copy_file ~src:source_exe ~dst:target_exe ())
;;
