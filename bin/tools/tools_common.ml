open Import
module Dev_tool = Dune_rules.Dev_tool
module Dev_tool_cache = Dune_rules.Pkg_cache.Dev_tool

let dev_tool_bin_dirs =
  List.map Dev_tool.all ~f:(fun tool ->
    Dev_tool.exe_path tool |> Path.Build.parent_exn |> Path.build)
;;

let add_dev_tools_to_path env =
  List.fold_left dev_tool_bin_dirs ~init:env ~f:(fun acc dir -> Env_path.cons acc ~dir)
;;

let dev_tool_exe_path dev_tool = Path.build @@ Dev_tool.exe_path dev_tool

let dev_tool_build_target dev_tool =
  Dune_lang.Dep_conf.File
    (Dune_lang.String_with_vars.make_text
       Loc.none
       (Path.to_string (dev_tool_exe_path dev_tool)))
;;

(* Find a package by name in the lock file packages. *)
let find_pkg_in_packages packages pkg_name =
  Dune_pkg.Lock.Packages.to_pkg_list packages
  |> List.find_opt ~f:(fun (pkg : Dune_pkg.Pkg.t) ->
    Package_name.equal pkg.info.name pkg_name)
;;

(* Extract the source checksum from a package's info.
   The source checksum (from opam url) is used for compiler-independent
   dev tools to enable cross-project cache sharing. *)
let get_source_checksum (pkg : Dune_pkg.Pkg.t) =
  match pkg.info.source with
  | Some { checksum = Some (_, checksum); _ } -> Some checksum
  | _ -> None
;;

(** Info returned from reading a dev tool lock file.
    For cache key computation:
    - Compiler-independent tools use source_checksum for cross-project sharing
    - Compiler-dependent tools use build_id (recursive hash including deps) *)
type dev_tool_lock_info =
  { version : string
  ; source_checksum : Dune_pkg.Checksum.t option
  ; build_id : Dune_digest.t option
  }

(* Read version and checksums from a dev tool's lock file synchronously.
   Returns None if the lock file doesn't exist. *)
let read_dev_tool_lock_info_sync ~workspace_root dev_tool =
  let lock_dir = Dev_tool.lock_dir dev_tool in
  let lock_dir_str =
    Filename.concat (Path.to_string workspace_root) (Path.Build.to_string lock_dir)
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
      |> Option.map ~f:(fun (pkg : Dune_pkg.Pkg.t) ->
        { version = Package_version.to_string pkg.info.version
        ; source_checksum = get_source_checksum pkg
        ; build_id = pkg.build_id
        }))
;;

(* Create a symlink in _build/install/default/bin/ pointing to the cached binary. *)
let create_install_symlink ~workspace_root dev_tool ~cached_exe_path =
  let exe_name = Dune_pkg.Dev_tool.exe_name dev_tool in
  let install_bin_dir_str =
    Filename.concat
      (Path.to_string workspace_root)
      (String.concat ~sep:Filename.dir_sep [ "_build"; "install"; "default"; "bin" ])
  in
  let install_bin_dir = Path.of_string install_bin_dir_str in
  Path.mkdir_p install_bin_dir;
  let symlink_path = Path.relative install_bin_dir exe_name in
  (* Remove existing file/symlink if present *)
  (try Unix.unlink (Path.to_string symlink_path) with
   | Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  (* Create symlink to cached binary *)
  Unix.symlink (Path.to_string cached_exe_path) (Path.to_string symlink_path)
;;

(* Populate the global cache after a successful dev tool build. *)
let populate_cache_sync ~workspace_root dev_tool =
  (* The install directory for the dev tool context contains the built tool.
     Path: workspace_root/_build/install/dev-tools-{name}/ *)
  let ctx = Dev_tool.context_name dev_tool in
  let source_dir_build = Install.Context.dir ~context:ctx in
  let source_dir_str =
    Filename.concat
      (Path.to_string workspace_root)
      (Path.Build.to_string source_dir_build)
  in
  let source_dir = Path.of_string source_dir_str in
  (* Only proceed if the source directory exists *)
  if not (Path.exists source_dir)
  then ()
  else (
    match read_dev_tool_lock_info_sync ~workspace_root dev_tool with
    | None -> ()
    | Some { version; source_checksum; build_id } ->
      Dev_tool_cache.populate_cache
        ~dev_tool
        ~version
        ~source_checksum
        ~build_id
        ~source_dir;
      (* Create symlink in _build/install/default/bin/ pointing to cached binary *)
      (match Dev_tool_cache.exe_path ~dev_tool ~version ~source_checksum ~build_id with
       | Some exe_path ->
         let cached_exe_path = Path.outside_build_dir exe_path in
         create_install_symlink ~workspace_root dev_tool ~cached_exe_path
       | None -> ()))
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
  let exe_name = Dev_tool.exe_name dev_tool in
  Console.message
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

let lock_build_and_run_dev_tool ~common ~config builder dev_tool ~args =
  (* Always lock and build - we need the lock file for the source checksum.
     The lock process is fast and the cache will be used if available. *)
  lock_and_build_dev_tool ~common ~config builder dev_tool;
  run_dev_tool (Common.root common) dev_tool ~args
;;

(* Get the cached exe path for a dev tool if it exists in the global cache.
   Requires the lock file to get version and cache key info. *)
let get_cached_exe_path ~workspace_root dev_tool =
  match read_dev_tool_lock_info_sync ~workspace_root dev_tool with
  | Some { version; source_checksum; build_id } ->
    if Dev_tool_cache.is_installed ~dev_tool ~version ~source_checksum ~build_id
    then Dev_tool_cache.exe_path ~dev_tool ~version ~source_checksum ~build_id
    else None
  | None -> None
;;

let which_command dev_tool =
  let exe_path = dev_tool_exe_path dev_tool in
  let exe_name = Dev_tool.exe_name dev_tool in
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

(* Build a dev tool with an optional specific version. *)
let build_dev_tool_with_version_directly common dev_tool version =
  let open Fiber.O in
  let+ result =
    Build.run_build_system ~common ~auto_fetch:true ~request:(fun _build_system ->
      let open Action_builder.O in
      let* () =
        Lock_dev_tool.lock_dev_tool_with_version dev_tool version
        |> Action_builder.of_memo
      in
      Action_builder.path (dev_tool_exe_path dev_tool))
  in
  match result with
  | Error `Already_reported -> raise Dune_util.Report_error.Already_reported
  | Ok () ->
    let workspace_root =
      let wr = Common.root common in
      Path.of_string wr.dir
    in
    populate_cache_sync ~workspace_root dev_tool
;;

let lock_and_build_dev_tool_with_version ~common ~config builder dev_tool version =
  let open Fiber.O in
  match Dune_util.Global_lock.lock ~timeout:None with
  | Error lock_held_by ->
    Scheduler.no_build_no_rpc ~config (fun () ->
      let* () = Lock_dev_tool.lock_dev_tool_with_version dev_tool version |> Memo.run in
      let+ () = build_dev_tool_via_rpc builder lock_held_by dev_tool in
      let workspace_root =
        let wr = Common.root common in
        Path.of_string wr.dir
      in
      populate_cache_sync ~workspace_root dev_tool)
  | Ok () ->
    Scheduler.go_with_rpc_server ~common ~config (fun () ->
      build_dev_tool_with_version_directly common dev_tool version)
;;

(* Parse a tool specification like "ocamlformat" or "ocamlformat.0.27.0" into
   (tool_name, optional_version). *)
let parse_tool_spec spec =
  (* Find the dev tool that matches the beginning of the spec *)
  let find_matching_tool () =
    List.find_opt Dune_pkg.Dev_tool.all ~f:(fun tool ->
      let name = Dune_pkg.Dev_tool.exe_name tool in
      String.is_prefix spec ~prefix:name
      && (String.length spec = String.length name
          || String.get spec (String.length name) = '.'))
  in
  match find_matching_tool () with
  | None -> None
  | Some tool ->
    let name = Dune_pkg.Dev_tool.exe_name tool in
    let version =
      if String.length spec > String.length name
      then (
        (* Skip the '.' after the tool name *)
        let version_str = String.drop_prefix spec ~prefix:(name ^ ".") in
        Option.map version_str ~f:Package_version.of_string)
      else None
    in
    Some (tool, version)
;;

let install_command dev_tool =
  let exe_name = Dev_tool.exe_name dev_tool in
  let term =
    let+ builder = Common.Builder.term
    and+ version_suffix =
      Arg.(value & pos 0 (some string) None & info [] ~docv:"VERSION" ~doc:None)
    in
    let common, config = Common.init builder in
    (* Version can be specified as positional arg: ocamlformat 0.27.0 *)
    let version = Option.map version_suffix ~f:Package_version.of_string in
    lock_and_build_dev_tool_with_version ~common ~config builder dev_tool version
  in
  let info =
    let doc = sprintf "Install %s as a dev tool. Optionally specify VERSION." exe_name in
    Cmd.info exe_name ~doc
  in
  Cmd.v info term
;;

(* Unified install command that accepts tool.version syntax *)
let install_unified_command =
  let term =
    let+ builder = Common.Builder.term
    and+ tool_spec =
      Arg.(
        required
        & pos 0 (some string) None
        & info
            []
            ~docv:"TOOL[.VERSION]"
            ~doc:(Some "Tool name, optionally with version (e.g., ocamlformat.0.27.0)"))
    in
    let common, config = Common.init builder in
    match parse_tool_spec tool_spec with
    | None ->
      User_error.raise
        [ Pp.textf "Unknown dev tool: %s" tool_spec
        ; Pp.text "Available tools:"
        ; Pp.enumerate Dune_pkg.Dev_tool.all ~f:(fun t ->
            Pp.text (Dune_pkg.Dev_tool.exe_name t))
        ]
    | Some (dev_tool, version) ->
      lock_and_build_dev_tool_with_version ~common ~config builder dev_tool version
  in
  let info =
    let doc =
      "Install a dev tool. Use TOOL.VERSION syntax to install a specific version."
    in
    Cmd.info "install" ~doc
  in
  Cmd.v info term
;;

let exec_command dev_tool =
  let exe_name = Dev_tool.exe_name dev_tool in
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
