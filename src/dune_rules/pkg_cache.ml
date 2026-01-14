open Import
module Pkg = Dune_pkg.Pkg

module Pkg_type = struct
  type compiler_info =
    { ocaml_version : string
    ; ocaml_build_id : Dune_digest.t
    }

  type t =
    | Workspace
    | Vendored
    | Locked
    | Toolchain
    | Dev_tool of
        { compiler_dependent : bool
        ; compiler_info : compiler_info option
        }

  let to_dyn = function
    | Workspace -> Dyn.variant "Workspace" []
    | Vendored -> Dyn.variant "Vendored" []
    | Locked -> Dyn.variant "Locked" []
    | Toolchain -> Dyn.variant "Toolchain" []
    | Dev_tool { compiler_dependent; compiler_info } ->
      Dyn.variant
        "Dev_tool"
        [ Dyn.record
            [ "compiler_dependent", Dyn.bool compiler_dependent
            ; ( "compiler_info"
              , Dyn.option
                  (fun { ocaml_version; ocaml_build_id } ->
                     Dyn.record
                       [ "ocaml_version", Dyn.string ocaml_version
                       ; "ocaml_build_id", Dune_digest.to_dyn ocaml_build_id
                       ])
                  compiler_info )
            ]
        ]
  ;;
end

let bid_short build_id = String.sub (Dune_digest.to_string build_id) ~pos:0 ~len:8

let ensure_dir_exists path =
  if not (Path.Untracked.exists path) then Path.mkdir_p path;
  if not (Path.Untracked.is_directory path)
  then
    User_error.raise
      [ Pp.textf "Expected %s to be a directory but it is not." (Path.to_string path) ]
;;

let index_dir_lazy =
  lazy
    (let dir = Path.relative (Lazy.force Dune_util.cache_root_dir) "index" in
     Log.info "Package cache index location" [ "dir", Dyn.string (Path.to_string dir) ];
     Path.as_outside_build_dir_exn dir)
;;

let index_dir () =
  let dir = Lazy.force index_dir_lazy in
  let path = Path.outside_build_dir dir in
  ensure_dir_exists path;
  dir
;;

let cache_key ~pkg_type ~name ~version ~build_id =
  let name_str = Package.Name.to_string name in
  let version_str = Package_version.to_string version in
  match pkg_type with
  | Pkg_type.Workspace | Pkg_type.Vendored -> None
  | Pkg_type.Locked | Pkg_type.Toolchain ->
    Some (sprintf "%s.%s-%s" name_str version_str (bid_short build_id))
  | Pkg_type.Dev_tool { compiler_dependent = false; _ } ->
    Some (sprintf "%s.%s" name_str version_str)
  | Pkg_type.Dev_tool { compiler_dependent = true; compiler_info = None } ->
    Some (sprintf "%s.%s" name_str version_str)
  | Pkg_type.Dev_tool { compiler_dependent = true; compiler_info = Some info } ->
    Some
      (sprintf
         "%s.%s-%s-%s"
         name_str
         version_str
         info.ocaml_version
         (bid_short info.ocaml_build_id))
;;

let is_cached ~cache_key =
  let index = Path.outside_build_dir (index_dir ()) in
  let symlink_path = Path.relative index cache_key in
  Path.Untracked.exists symlink_path
;;

let cached_content_path ~cache_key =
  let index = Path.outside_build_dir (index_dir ()) in
  let symlink_path = Path.relative index cache_key in
  match Unix.readlink (Path.to_string symlink_path) with
  | target -> Some (Path.of_string target)
  | exception Unix.Unix_error _ -> None
;;

(* Content-addressed storage directory: ~/.cache/dune/db/files/v5/ *)
let content_store_dir =
  lazy
    (let dir =
       Path.relative (Path.relative (Lazy.force Dune_util.cache_root_dir) "db") "files/v5"
     in
     Path.mkdir_p dir;
     dir)
;;

(* Copy directory tree recursively *)
let rec copy_dir_tree ~src ~dst =
  Path.mkdir_p dst;
  match Path.Untracked.readdir_unsorted_with_kinds src with
  | Ok entries ->
    List.iter entries ~f:(fun (name, kind) ->
      let src' = Path.relative src name in
      let dst' = Path.relative dst name in
      match kind with
      | Unix.S_REG -> Io.copy_file ~src:src' ~dst:dst' ()
      | Unix.S_DIR -> copy_dir_tree ~src:src' ~dst:dst'
      | _ -> ())
  | Error _ -> ()
;;

(* Store content in content-addressed cache and create symlink in index.
   Uses the content_digest to determine the storage location. *)
let store_in_cache ~cache_key ~source_dir ~content_digest =
  let digest_str = Dune_digest.to_string content_digest in
  let content_store = Lazy.force content_store_dir in
  (* Content path: db/files/v5/<2-char-prefix>/<full-digest> *)
  let prefix = String.sub digest_str ~pos:0 ~len:2 in
  let content_dir = Path.relative (Path.relative content_store prefix) digest_str in
  (* Only copy if not already in content store *)
  if not (Path.Untracked.exists content_dir)
  then (
    Path.mkdir_p (Path.parent_exn content_dir);
    copy_dir_tree ~src:source_dir ~dst:content_dir);
  (* Create symlink in index pointing to content store *)
  let index = Path.outside_build_dir (index_dir ()) in
  let symlink_path = Path.relative index cache_key in
  if not (Path.Untracked.exists symlink_path)
  then (
    (* Create relative symlink: index/<key> -> ../db/files/v5/<prefix>/<digest> *)
    let relative_target = sprintf "../db/files/v5/%s/%s" prefix digest_str in
    Unix.symlink relative_target (Path.to_string symlink_path))
;;

let populate_from_cache_action ~cache_key ~install_dir ~target_dir:_ =
  let index = Path.outside_build_dir (index_dir ()) in
  let cache_target = Path.relative index cache_key in
  let cache_target_str = Path.to_string cache_target in
  let target_dir_rel = "../target" in
  let install_dir_str = Path.Build.to_string install_dir in
  let cmd =
    sprintf
      "mkdir -p %s && mkdir -p %s && cp -a %s/* %s/"
      (Filename.quote target_dir_rel)
      (Filename.quote install_dir_str)
      (Filename.quote cache_target_str)
      (Filename.quote install_dir_str)
  in
  Dune_lang.Action.System (String_with_vars.make_text Loc.none cmd)
;;

module Toolchain = struct
  let relocatable_compiler_name = Package.Name.of_string "relocatable-compiler"
  let is_relocatable_compiler name = Package.Name.equal name relocatable_compiler_name

  let relocatable_base_version version =
    let version_str = Package_version.to_string version in
    match String.split version_str ~on:'.' with
    | major :: minor :: patch :: _ ->
      Some (String.concat ~sep:"." [ major; minor; patch ])
    | _ -> None
  ;;

  let platform_suffix (pkg : Pkg.t) =
    match pkg.enabled_on_platforms with
    | [] -> ""
    | [ platform ] ->
      let os =
        Dune_pkg.Solver_env.get platform Dune_lang.Package_variable_name.os
        |> Option.map ~f:Dune_pkg.Variable_value.to_string
      in
      let arch =
        Dune_pkg.Solver_env.get platform Dune_lang.Package_variable_name.arch
        |> Option.map ~f:Dune_pkg.Variable_value.to_string
      in
      (match os, arch with
       | Some os, Some arch -> sprintf "-%s-%s" os arch
       | Some os, None -> sprintf "-%s" os
       | None, Some arch -> sprintf "-%s" arch
       | None, None -> "")
    | platforms ->
      let platform_digest = Dune_digest.generic platforms in
      let hash = Dune_digest.to_string platform_digest in
      "-" ^ String.sub hash ~pos:0 ~len:(min 8 (String.length hash))
  ;;

  let pkg_dir (pkg : Pkg.t) ~build_id =
    let dir_name =
      let name = pkg.info.name in
      if is_relocatable_compiler name
      then (
        let base_version =
          relocatable_base_version pkg.info.version
          |> Option.value ~default:(Package_version.to_string pkg.info.version)
        in
        let platform = platform_suffix pkg in
        sprintf
          "%s.%s%s-%s"
          (Package.Name.to_string name)
          base_version
          platform
          (Dune_digest.to_string build_id))
      else
        sprintf
          "%s.%s-%s"
          (Package.Name.to_string name)
          (Package_version.to_string pkg.info.version)
          (Dune_digest.to_string build_id)
    in
    Path.Outside_build_dir.relative (index_dir ()) dir_name
  ;;

  let installation_prefix pkg ~build_id =
    let pkg_dir = pkg_dir pkg ~build_id in
    Path.Outside_build_dir.relative pkg_dir "target"
  ;;

  let is_compiler_and_toolchains_enabled name =
    match Config.get Compile_time.toolchains with
    | `Enabled -> Dune_pkg.Dev_tool.is_compiler_package name
    | `Disabled -> false
  ;;

  let install_roots ~prefix =
    Install.Roots.make prefix ~relative:Path.Outside_build_dir.relative
  ;;

  let is_installed pkg ~build_id =
    let prefix = installation_prefix pkg ~build_id in
    let cookie_path =
      Path.outside_build_dir (Path.Outside_build_dir.relative prefix "cookie")
    in
    Path.Untracked.exists cookie_path
  ;;

  let cache_dir pkg ~build_id = Path.outside_build_dir (pkg_dir pkg ~build_id)

  let populate_from_cache_action pkg ~build_id ~install_dir ~target_dir:_ =
    let cache_target = Path.outside_build_dir (installation_prefix pkg ~build_id) in
    let cache_target_str = Path.to_string cache_target in
    let target_dir_rel = "../target" in
    let install_dir_str = Path.Build.to_string install_dir in
    let cache_cookie = Filename.concat cache_target_str "cookie" in
    let target_cookie = Filename.concat target_dir_rel "cookie" in
    let cmd =
      sprintf
        "mkdir -p %s && mkdir -p %s && cp -a %s/* %s/ && cp %s %s"
        (Filename.quote target_dir_rel)
        (Filename.quote install_dir_str)
        (Filename.quote cache_target_str)
        (Filename.quote install_dir_str)
        (Filename.quote cache_cookie)
        (Filename.quote target_cookie)
    in
    Dune_lang.Action.System (String_with_vars.make_text Loc.none cmd)
  ;;
end

module Dev_tool = struct
  type ocaml_compiler_info =
    { version : string
    ; build_id : Dune_digest.t
    }

  let cache_key ~dev_tool ~version ~ocaml_compiler =
    let pkg_name = Dune_pkg.Dev_tool.package_name dev_tool in
    let base = sprintf "%s.%s" (Package.Name.to_string pkg_name) version in
    match Dune_pkg.Dev_tool.needs_to_build_with_same_compiler_as_project dev_tool with
    | false -> base
    | true ->
      (match ocaml_compiler with
       | None -> base
       | Some { version = ocaml_ver; build_id } ->
         let build_id_short = bid_short build_id in
         sprintf "%s-%s-%s" base ocaml_ver build_id_short)
  ;;

  let cache_dir ~dev_tool ~version ~ocaml_compiler =
    let key = cache_key ~dev_tool ~version ~ocaml_compiler in
    Path.Outside_build_dir.relative (index_dir ()) key
  ;;

  let exe_path ~dev_tool ~version ~ocaml_compiler =
    let prefix = cache_dir ~dev_tool ~version ~ocaml_compiler in
    let exe_components = Dune_pkg.Dev_tool.exe_path_components_within_package dev_tool in
    List.fold_left exe_components ~init:prefix ~f:Path.Outside_build_dir.relative
  ;;

  let is_installed ~dev_tool ~version ~ocaml_compiler =
    let exe = exe_path ~dev_tool ~version ~ocaml_compiler in
    Path.Untracked.exists (Path.outside_build_dir exe)
  ;;

  let cache_dir_path ~dev_tool ~version ~ocaml_compiler =
    Path.outside_build_dir (cache_dir ~dev_tool ~version ~ocaml_compiler)
  ;;

  let populate_cache ~dev_tool ~version ~ocaml_compiler ~source_dir =
    let cache_target = cache_dir_path ~dev_tool ~version ~ocaml_compiler in
    Path.mkdir_p cache_target;
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
end
