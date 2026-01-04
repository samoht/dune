open Import
module Lock_dir = Dune_pkg.Lock
module Lock_pkg = Dune_pkg.Lock_pkg
module Source = Dune_pkg.Source
module Duniverse = Dune_pkg.Duniverse
module Rev_store = Dune_pkg.Rev_store
module OpamUrl = Dune_pkg.OpamUrl
module Pkg_cache = Dune_pkg.Pkg_cache
module Solver_env = Dune_pkg.Solver_env
module Package_version = Dune_pkg.Package_version

(* Default patches directory for user patches *)
let default_patches_dir = Path.Source.of_string "patches"

(* Get the user patch file path for a package *)
let user_patch_path ~patches_dir name version =
  let filename =
    sprintf
      "%s@%s.patch"
      (Package_name.to_string name)
      (Package_version.to_string version)
  in
  Path.Source.relative patches_dir filename
;;

(* Get the default lock dir path *)
let get_default_lock_dir_path () = Dune_rules.Lock_dir.default_source_path |> Path.source

let handle_fetch_error ~name ~version = function
  | Ok () -> Fiber.return ()
  | Error (Dune_pkg.Fetch.Unavailable msg) ->
    let msg =
      match msg with
      | Some m -> User_message.to_string m
      | None -> "unavailable"
    in
    User_error.raise
      [ Pp.textf
          "Failed to fetch %s.%s: %s"
          (Package_name.to_string name)
          (Dune_pkg.Package_version.to_string version)
          msg
      ]
  | Error (Dune_pkg.Fetch.Checksum_mismatch actual) ->
    User_error.raise
      [ Pp.textf
          "Checksum mismatch for %s.%s (got %s)"
          (Package_name.to_string name)
          (Dune_pkg.Package_version.to_string version)
          (Dune_pkg.Checksum.to_string actual)
      ]
;;

(* Copy a directory tree to the target *)
let copy_directory ~src ~dst =
  let rec copy_tree src_path dst_path =
    let src_string = Path.External.to_string src_path in
    match (Unix.stat src_string).st_kind with
    | Unix.S_DIR ->
      Path.mkdir_p (Path.source dst_path);
      Path.readdir_unsorted_with_kinds (Path.external_ src_path)
      |> Result.value ~default:[]
      |> List.iter ~f:(fun (name, _kind) ->
        copy_tree
          (Path.External.relative src_path name)
          (Path.Source.relative dst_path name))
    | Unix.S_REG ->
      let src_file = Path.external_ src_path in
      let dst_file = Path.source dst_path in
      let parent = Path.parent_exn dst_file in
      if not (Path.exists parent) then Path.mkdir_p parent;
      Io.copy_file ~src:src_file ~dst:dst_file ()
    | Unix.S_LNK ->
      let target = Unix.readlink src_string in
      let dst_file = Path.Source.to_string dst_path in
      (try Unix.symlink target dst_file with
       | Unix.Unix_error (Unix.EEXIST, _, _) -> ())
    | _ -> ()
    | exception Unix.Unix_error _ -> ()
  in
  copy_tree src dst;
  Fiber.return (Ok ())
;;

let do_fetch ~rev_store ~source ~target =
  (* Convert target string to a Path.t, handling both absolute and relative paths *)
  let target_path =
    if Filename.is_relative target
    then Path.relative Path.root target
    else Path.of_string target
  in
  match Source.kind source with
  | `Directory_or_archive src_path ->
    (* Local directory copy *)
    let is_dir =
      match (Unix.stat (Path.External.to_string src_path)).st_kind with
      | Unix.S_DIR -> true
      | _ -> false
    in
    if is_dir
    then copy_directory ~src:src_path ~dst:(Path.Source.of_string target)
    else (
      (* Local archive file - extract it *)
      let { Source.url; checksum } = source in
      let checksum_opt = Option.map checksum ~f:snd in
      Dune_pkg.Fetch.fetch ~unpack:true ~checksum:checksum_opt ~target:target_path ~url)
  | `Fetch ->
    let { Source.url; checksum } = source in
    let _, opam_url = url in
    let checksum_opt = Option.map checksum ~f:snd in
    (* Check if this is a git URL *)
    if OpamUrl.is_version_control opam_url
    then Dune_pkg.Fetch.fetch_git rev_store ~target:target_path ~url
    else
      (* HTTP archive fetch - use Fetch.fetch directly *)
      Dune_pkg.Fetch.fetch ~unpack:true ~checksum:checksum_opt ~target:target_path ~url
;;

(* Fetch a single extra source file to the target directory *)
let fetch_extra_source ~rev_store ~target_dir (local_path, (source : Source.t)) =
  let dst = Path.append_local target_dir local_path in
  (* Create parent directory if needed *)
  let parent = Path.parent_exn dst in
  if not (Path.exists parent) then Path.mkdir_p parent;
  let { Source.url; checksum } = source in
  let _, opam_url = url in
  let checksum_opt = Option.map checksum ~f:snd in
  (* Check if git or regular HTTP *)
  if OpamUrl.is_version_control opam_url
  then Dune_pkg.Fetch.fetch_git rev_store ~target:dst ~url
  else Dune_pkg.Fetch.fetch ~unpack:false ~checksum:checksum_opt ~target:dst ~url
;;

(* Apply a patch file to the target directory *)
let apply_patch ~target_dir ~patch_file =
  let open Fiber.O in
  let patch_path = Path.append_local target_dir patch_file in
  if not (Path.exists patch_path)
  then
    User_error.raise
      [ Pp.textf
          "Patch file %s not found in package directory"
          (Path.Local.to_string patch_file)
      ];
  let stderr =
    Dune_engine.Process.Io.make_stderr
      ~output_on_success:Swallow
      ~output_limit:Dune_engine.Execution_parameters.Action_output_limit.default
  in
  let+ () =
    Dune_patch.For_tests.exec
      Dune_engine.Display.Quiet
      ~patch:patch_path
      ~dir:target_dir
      ~stderr
  in
  ()
;;

(* Apply a user patch from the patches/ directory *)
let apply_user_patch ~target_dir ~patch_source_path =
  let open Fiber.O in
  let patch_path = Path.source patch_source_path in
  if not (Path.exists patch_path)
  then Fiber.return () (* No user patch for this package *)
  else (
    Console.print_user_message
      (User_message.make
         [ Pp.textf
             "  Applying user patch %s..."
             (Path.Source.to_string patch_source_path)
         ]);
    let stderr =
      Dune_engine.Process.Io.make_stderr
        ~output_on_success:Swallow
        ~output_limit:Dune_engine.Execution_parameters.Action_output_limit.default
    in
    let+ () =
      Dune_patch.For_tests.exec
        Dune_engine.Display.Quiet
        ~patch:patch_path
        ~dir:target_dir
        ~stderr
    in
    ())
;;

(* Returns true if the package was actually fetched, false if skipped *)
let fetch_package ~rev_store ~platform ~patches_dir pkg =
  let open Fiber.O in
  let { Lock_dir.Pkg_info.name; version; source; extra_sources; _ } =
    pkg.Lock_dir.Pkg.info
  in
  match source with
  | None ->
    (* No source means local package or pinned without URL *)
    Fiber.return false
  | Some source ->
    let target = Duniverse.package_dir name version |> Path.source in
    let target_path = Path.to_string target in
    (* Skip if already fetched *)
    if Path.exists target
    then (
      Console.print_user_message
        (User_message.make
           [ Pp.textf
               "Package %s.%s already fetched"
               (Package_name.to_string name)
               (Dune_pkg.Package_version.to_string version)
           ]);
      Fiber.return false)
    else (
      Console.print_user_message
        (User_message.make
           [ Pp.textf
               "Fetching %s.%s to %s"
               (Package_name.to_string name)
               (Dune_pkg.Package_version.to_string version)
               target_path
           ]);
      (* Step 1: Fetch main source directly to target *)
      (* TODO: Add caching with Pkg_cache once basic flow works *)
      let* result = do_fetch ~rev_store ~source ~target:target_path in
      let* () = handle_fetch_error ~name ~version result in
      (* Step 2: Fetch extra sources (patches, additional files) *)
      let* () =
        if List.is_empty extra_sources
        then Fiber.return ()
        else (
          Console.print_user_message
            (User_message.make
               [ Pp.textf "  Fetching %d extra source(s)..." (List.length extra_sources) ]);
          Fiber.sequential_iter extra_sources ~f:(fun extra ->
            let* result = fetch_extra_source ~rev_store ~target_dir:target extra in
            let local_path, _ = extra in
            match result with
            | Ok () -> Fiber.return ()
            | Error (Dune_pkg.Fetch.Unavailable msg) ->
              let msg_str =
                match msg with
                | Some m -> User_message.to_string m
                | None -> "unavailable"
              in
              User_error.raise
                [ Pp.textf
                    "Failed to fetch extra source %s: %s"
                    (Path.Local.to_string local_path)
                    msg_str
                ]
            | Error (Dune_pkg.Fetch.Checksum_mismatch actual) ->
              User_error.raise
                [ Pp.textf
                    "Checksum mismatch for extra source %s (got %s)"
                    (Path.Local.to_string local_path)
                    (Dune_pkg.Checksum.to_string actual)
                ]))
      in
      (* Step 3: Apply patches from build command (opam patches) *)
      let patches = Duniverse.get_patches pkg ~platform in
      let* () =
        if List.is_empty patches
        then Fiber.return ()
        else (
          Console.print_user_message
            (User_message.make
               [ Pp.textf "  Applying %d opam patch(es)..." (List.length patches) ]);
          Fiber.sequential_iter patches ~f:(fun patch_sw ->
            (* Patches are String_with_vars, but in lock files they should be literals *)
            match Dune_lang.String_with_vars.text_only patch_sw with
            | None ->
              User_error.raise
                [ Pp.text "Patch file path contains variables, which is not supported" ]
            | Some patch_file ->
              let patch_local = Path.Local.of_string patch_file in
              apply_patch ~target_dir:target ~patch_file:patch_local))
      in
      (* Step 4: Apply user patches from patches/ directory *)
      let user_patch = user_patch_path ~patches_dir name version in
      let+ () = apply_user_patch ~target_dir:target ~patch_source_path:user_patch in
      true)
;;

let fetch_duniverse ~lock_dir_path ~solver_env () =
  let open Fiber.O in
  (* Read the lock - handles both single-file and directory formats *)
  let lock_path = Path.source lock_dir_path in
  let* lock_dir = Lock_pkg.read_disk_fiber ~solver_env lock_path in
  let all_pkgs = Lock_dir.Packages.to_pkg_list lock_dir.packages in
  (* Fetch ALL packages that have a source URL to duniverse (both dune and non-dune) *)
  let fetchable_pkgs =
    List.filter all_pkgs ~f:(fun (pkg : Lock_dir.Pkg.t) -> Option.is_some pkg.info.source)
  in
  if List.is_empty fetchable_pkgs
  then (
    (* Check if there are packages without sources (local packages) *)
    let pkgs_without_sources =
      List.filter all_pkgs ~f:(fun (pkg : Lock_dir.Pkg.t) ->
        Option.is_none pkg.info.source)
    in
    if not (List.is_empty pkgs_without_sources)
    then
      Console.print_user_message
        (User_message.make
           [ Pp.textf
               "%d package(s) have no source URL (likely local packages)."
               (List.length pkgs_without_sources)
           ])
    else
      Console.print_user_message (User_message.make [ Pp.text "No packages to fetch." ]);
    Fiber.return ())
  else (
    (* Create duniverse directory if it doesn't exist *)
    let duniverse_path = Path.source Duniverse.duniverse_dir in
    if not (Path.exists duniverse_path) then Path.mkdir_p duniverse_path;
    (* Create marker file *)
    let marker_path =
      Path.source (Path.Source.relative Duniverse.duniverse_dir Duniverse.marker_filename)
    in
    if not (Path.exists marker_path)
    then Io.write_file marker_path "# This directory is managed by dune pkg\n";
    (* Initialize rev store for git fetches *)
    let* rev_store = Rev_store.get in
    (* Get current platform for patch selection *)
    let* platform = Pkg_common.poll_solver_env_from_current_system () in
    (* Use default patches directory *)
    let patches_dir = default_patches_dir in
    (* Fetch each package and count how many were actually fetched *)
    let+ fetched_results =
      Fiber.sequential_map fetchable_pkgs ~f:(fun pkg ->
        fetch_package ~rev_store ~platform ~patches_dir pkg)
    in
    let num_fetched = List.filter fetched_results ~f:Fun.id |> List.length in
    if num_fetched > 0
    then
      Console.print_user_message
        (User_message.make
           [ Pp.textf
               "Fetched %d package(s) to %s/"
               num_fetched
               (Path.Source.to_string Duniverse.duniverse_dir)
           ]))
;;

let term =
  let+ builder = Common.Builder.term
  and+ lock_dirs_arg = Pkg_common.Lock_dirs_arg.term in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler.go_with_rpc_server ~common ~config (fun () ->
    let open Fiber.O in
    Pkg_common.check_pkg_management_enabled ()
    >>>
    let* solver_env = Pkg_common.poll_solver_env_from_current_system ()
    and* workspace = Memo.run (Workspace.workspace ()) in
    let lock_dirs =
      Pkg_common.Lock_dirs_arg.lock_dirs_of_workspace lock_dirs_arg workspace
    in
    match lock_dirs with
    | [] ->
      User_error.raise [ Pp.text "No lock directories found. Run 'dune pkg lock' first." ]
    | lock_dir_path :: _ -> fetch_duniverse ~lock_dir_path ~solver_env ())
;;

let info =
  let doc = "Fetch package sources to duniverse" in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        "Fetches the source code for all locked packages to the duniverse directory. \
         Packages using dune as their build system are built alongside your project \
         code, while other packages are built in the .pkg sandbox but from duniverse \
         sources."
    ; `P
        "All packages are downloaded to duniverse/<name>.<version>/ and can be edited \
         directly. Changes will be reflected in your project builds."
    ; `S "EXAMPLES"
    ; `Pre "  dune pkg fetch"
    ; `Pre "  dune pkg fetch dune.lock"
    ]
  in
  Cmd.info "fetch" ~doc ~man
;;

let command = Cmd.v info term
