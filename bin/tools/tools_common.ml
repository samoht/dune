open Import
module Pkg_dev_tool = Dune_rules.Pkg_dev_tool
module Dev_tool_cache = Dune_rules.Dev_tool_cache

let dev_tool_bin_dirs =
  List.map Pkg_dev_tool.all ~f:(fun tool ->
    Pkg_dev_tool.exe_path tool |> Path.Build.parent_exn |> Path.build)
;;

let add_dev_tools_to_path env =
  List.fold_left dev_tool_bin_dirs ~init:env ~f:(fun acc dir -> Env_path.cons acc ~dir)
;;

let dev_tool_exe_path dev_tool = Path.build @@ Pkg_dev_tool.exe_path dev_tool

let dev_tool_build_target dev_tool =
  Dune_lang.Dep_conf.File
    (Dune_lang.String_with_vars.make_text
       Loc.none
       (Path.to_string (dev_tool_exe_path dev_tool)))
;;

(* Get the expected version for a dev tool from config files (not lock files).
   This is used for cache lookup before building. *)
let get_expected_version_from_config dev_tool =
  match (dev_tool : Dune_pkg.Dev_tool.t) with
  | Ocamlformat ->
    Dune_pkg.Ocamlformat.version_of_current_project's_ocamlformat_config ()
    |> Option.map ~f:Package_version.to_string
  | _ -> None
;;

(* Find a package by name in the lock file packages. *)
let find_pkg_in_packages packages pkg_name =
  Dune_pkg.Lock.Packages.to_pkg_list packages
  |> List.find_opt ~f:(fun (pkg : Dune_pkg.Lock.Pkg.t) ->
    Package_name.equal pkg.info.name pkg_name)
;;

(* Read version from a dev tool's lock file synchronously.
   Returns None if the lock file doesn't exist. *)
let read_dev_tool_version_sync ~workspace_root dev_tool =
  let tool_name = Dune_pkg.Dev_tool.package_name dev_tool |> Package_name.to_string in
  let lock_dir_str =
    Filename.concat
      (Path.to_string workspace_root)
      (Filename.concat "_build/.dev-tools.locks" tool_name)
  in
  let lock_dir_path = Path.of_string lock_dir_str in
  if not (Path.exists lock_dir_path)
  then None
  else (
    match Dune_pkg.Lock.read_disk lock_dir_path with
    | Error _ -> None
    | Ok { packages; _ } ->
      let pkg_name = Dune_pkg.Dev_tool.package_name dev_tool in
      find_pkg_in_packages packages pkg_name
      |> Option.map ~f:(fun (pkg : Dune_pkg.Lock.Pkg.t) ->
        Package_version.to_string pkg.info.version))
;;

(* Read OCaml version from project's lock file synchronously.
   Returns None if the lock file doesn't exist or has no compiler. *)
let read_project_ocaml_version_sync ~workspace_root =
  let lock_dir_str = Filename.concat (Path.to_string workspace_root) "dune.lock" in
  let lock_dir_path = Path.of_string lock_dir_str in
  if not (Path.exists lock_dir_path)
  then None
  else (
    match Dune_pkg.Lock.read_disk lock_dir_path with
    | Error _ -> None
    | Ok { packages; ocaml; _ } ->
      (match ocaml with
       | None -> None
       | Some (_loc, pkg_name) ->
         find_pkg_in_packages packages pkg_name
         |> Option.map ~f:(fun (pkg : Dune_pkg.Lock.Pkg.t) ->
           Package_version.to_string pkg.info.version)))
;;

(* Populate the global cache after a successful dev tool build. *)
let populate_cache_sync ~workspace_root dev_tool =
  (* Get the source directory where the tool was built.
     Path: workspace_root/_build/_private/default/.dev-tool/{name}/target *)
  let dev_tool_name =
    Dune_pkg.Dev_tool.package_name dev_tool |> Package_name.to_string
  in
  (* Build path as a string then convert - ensures correct absolute path handling *)
  let source_dir_str =
    Filename.concat
      (Path.to_string workspace_root)
      (String.concat
         ~sep:Filename.dir_sep
         [ "_build"; "_private"; "default"; ".dev-tool"; dev_tool_name; "target" ])
  in
  let source_dir = Path.of_string source_dir_str in
  (* Only proceed if the source directory exists *)
  if not (Path.exists source_dir)
  then ()
  else (
    match read_dev_tool_version_sync ~workspace_root dev_tool with
    | None -> ()
    | Some version ->
      let ocaml_version =
        if Dune_pkg.Dev_tool.needs_to_build_with_same_compiler_as_project dev_tool
        then read_project_ocaml_version_sync ~workspace_root
        else None
      in
      Dev_tool_cache.populate_cache ~dev_tool ~version ~ocaml_version ~source_dir)
;;

let build_dev_tool_directly common dev_tool =
  let open Fiber.O in
  let+ result =
    Build.run_build_system ~common ~auto_fetch:true ~request:(fun _build_system ->
      let open Action_builder.O in
      let* () = dev_tool |> Lock_dev_tool.lock_dev_tool |> Action_builder.of_memo in
      (* Make sure the tool's lockdir is generated before building the tool. *)
      Action_builder.path (dev_tool_exe_path dev_tool))
  in
  match result with
  | Error `Already_reported -> raise Dune_util.Report_error.Already_reported
  | Ok () ->
    (* Populate the cache after build completes.
       Use the workspace root from common to ensure correct path resolution. *)
    let workspace_root =
      let wr = Common.root common in
      Path.of_string wr.dir
    in
    populate_cache_sync ~workspace_root dev_tool
;;

let build_dev_tool_via_rpc builder lock_held_by dev_tool =
  let target = dev_tool_build_target dev_tool in
  let targets = Rpc.Rpc_common.prepare_targets [ target ] in
  let open Fiber.O in
  Rpc.Rpc_common.fire_request
    ~name:"build"
    ~wait:true
    ~lock_held_by
    builder
    Dune_rpc_impl.Decl.build
    targets
  >>| Rpc.Rpc_common.wrap_build_outcome_exn ~print_on_success:false
;;

let lock_and_build_dev_tool ~common ~config builder dev_tool =
  let open Fiber.O in
  match Dune_util.Global_lock.lock ~timeout:None with
  | Error lock_held_by ->
    Scheduler.no_build_no_rpc ~config (fun () ->
      let* () = Lock_dev_tool.lock_dev_tool dev_tool |> Memo.run in
      let+ () = build_dev_tool_via_rpc builder lock_held_by dev_tool in
      (* Populate cache after RPC build completes *)
      let workspace_root =
        let wr = Common.root common in
        Path.of_string wr.dir
      in
      populate_cache_sync ~workspace_root dev_tool)
  | Ok () ->
    Scheduler.go_with_rpc_server ~common ~config (fun () ->
      build_dev_tool_directly common dev_tool)
;;

let run_dev_tool_from_path workspace_root dev_tool ~exe_path_string ~args =
  let exe_name = Pkg_dev_tool.exe_name dev_tool in
  Console.print_user_message
    (Dune_rules.Pkg_build_progress.format_user_message
       ~verb:"Running"
       ~object_:(User_message.command (String.concat ~sep:" " (exe_name :: args))));
  Console.finish ();
  let env = add_dev_tools_to_path Env.initial in
  restore_cwd_and_execve workspace_root exe_path_string args env
;;

let run_dev_tool workspace_root dev_tool ~args =
  let exe_path_string = Path.to_string (dev_tool_exe_path dev_tool) in
  run_dev_tool_from_path workspace_root dev_tool ~exe_path_string ~args
;;

(* Try to get cached exe path using config-based version (not lock file).
   This is used for fast reinstall after dune clean. *)
let get_cached_exe_path_from_config dev_tool =
  match get_expected_version_from_config dev_tool with
  | None -> None
  | Some version ->
    (* For compiler-dependent tools, we'd need the OCaml version.
       But after dune clean, we can't get it without the project lock file.
       So for now, only compiler-independent tools support fast reinstall. *)
    if Dune_pkg.Dev_tool.needs_to_build_with_same_compiler_as_project dev_tool
    then None
    else if Dev_tool_cache.is_installed ~dev_tool ~version ~ocaml_version:None
    then Some (Dev_tool_cache.exe_path ~dev_tool ~version ~ocaml_version:None)
    else None
;;

let lock_build_and_run_dev_tool ~common ~config builder dev_tool ~args =
  (* Check if tool is already in global cache (fast reinstall after clean) *)
  match get_cached_exe_path_from_config dev_tool with
  | Some cached_exe_path ->
    (* Tool is in cache, run directly without lock/build *)
    let exe_path_string = Path.to_string (Path.outside_build_dir cached_exe_path) in
    run_dev_tool_from_path (Common.root common) dev_tool ~exe_path_string ~args
  | None ->
    (* Need to lock and build *)
    lock_and_build_dev_tool ~common ~config builder dev_tool;
    run_dev_tool (Common.root common) dev_tool ~args
;;

(* Get the cached exe path for a dev tool if it exists in the global cache.
   First tries the lock file, then falls back to config-based version. *)
let get_cached_exe_path ~workspace_root dev_tool =
  (* First try reading version from lock file *)
  match read_dev_tool_version_sync ~workspace_root dev_tool with
  | Some version ->
    let ocaml_version =
      if Dune_pkg.Dev_tool.needs_to_build_with_same_compiler_as_project dev_tool
      then read_project_ocaml_version_sync ~workspace_root
      else None
    in
    if Dev_tool_cache.is_installed ~dev_tool ~version ~ocaml_version
    then Some (Dev_tool_cache.exe_path ~dev_tool ~version ~ocaml_version)
    else None
  | None ->
    (* Lock file doesn't exist (e.g., after clean), try config-based version *)
    get_cached_exe_path_from_config dev_tool
;;

let which_command dev_tool =
  let exe_path = dev_tool_exe_path dev_tool in
  let exe_name = Pkg_dev_tool.exe_name dev_tool in
  let term =
    let+ builder = Common.Builder.term
    and+ allow_not_installed =
      Arg.(
        value
        & flag
        & info
            [ "allow-not-installed" ]
            ~doc:
              (Some
                 (sprintf
                    "If %s is not installed as a dev tool, still print where it would be \
                     installed."
                    exe_name)))
    in
    let common, _ = Common.init builder in
    let workspace_root =
      let wr = Common.root common in
      Path.of_string wr.dir
    in
    (* Check global cache first *)
    match get_cached_exe_path ~workspace_root dev_tool with
    | Some cached_path ->
      print_endline (Path.to_string (Path.outside_build_dir cached_path))
    | None ->
      if allow_not_installed || Path.exists exe_path
      then print_endline (Path.to_string exe_path)
      else User_error.raise [ Pp.textf "%s is not installed as a dev tool" exe_name ]
  in
  let info =
    let doc =
      sprintf
        "Prints the path to the %s dev tool executable if it exists, errors out \
         otherwise."
        exe_name
    in
    Cmd.info exe_name ~doc
  in
  Cmd.v info term
;;

let install_command dev_tool =
  let exe_name = Pkg_dev_tool.exe_name dev_tool in
  let term =
    let+ builder = Common.Builder.term in
    let common, config = Common.init builder in
    lock_and_build_dev_tool ~common ~config builder dev_tool
  in
  let info =
    let doc = sprintf "Install %s as a dev tool" exe_name in
    Cmd.info exe_name ~doc
  in
  Cmd.v info term
;;

let exec_command dev_tool =
  let exe_name = Pkg_dev_tool.exe_name dev_tool in
  let term =
    let+ builder = Common.Builder.term
    (* CR-someday Alizter: document this option *)
    and+ args = Arg.(value & pos_all string [] (info [] ~docv:"ARGS" ~doc:None)) in
    let common, config = Common.init builder in
    lock_build_and_run_dev_tool ~common ~config builder dev_tool ~args
  in
  let info =
    let doc =
      sprintf
        {|Wrapper for running %s intended to be run automatically
          by a text editor. All positional arguments will be passed to the
          %s executable (pass flags to %s after the '--'
          argument, such as 'dune tools exec %s -- --help').|}
        exe_name
        exe_name
        exe_name
        exe_name
    in
    Cmd.info exe_name ~doc
  in
  Cmd.v info term
;;

let env_command =
  let term =
    let+ builder = Common.Builder.term
    and+ fish =
      Arg.(
        value
        & flag
        & info
            [ "fish" ]
            ~doc:(Some "Print command for the fish shell rather than POSIX shells"))
    in
    let _ : Common.t * Dune_config.t = Common.init builder in
    if fish
    then (
      let space_separated_dev_tool_paths =
        List.map dev_tool_bin_dirs ~f:Path.to_string_maybe_quoted
        |> String.concat ~sep:" "
      in
      print_endline (sprintf "fish_add_path --prepend %s" space_separated_dev_tool_paths))
    else (
      let initial_path = Env.get Env.initial Env_path.var in
      let new_path =
        List.fold_left dev_tool_bin_dirs ~init:initial_path ~f:(fun acc bin_dir ->
          Some (Bin.cons_path bin_dir ~_PATH:acc))
      in
      match new_path with
      | None -> ()
      | Some new_path -> print_endline (sprintf "export %s=%s" Env_path.var new_path))
  in
  let info =
    let doc =
      "Print a command which can be eval'd to enter an environment where all dev tools \
       are runnable as commands."
    in
    Cmd.info "env" ~doc
  in
  Cmd.v info term
;;
