open Import
module Lock_dir = Dune_pkg.Lock
module Lock_pkg = Dune_pkg.Lock_pkg
module Pkg = Dune_pkg.Pkg
module Source = Dune_pkg.Source
module Vendor = Dune_pkg.Vendor
module Vendor_rules = Dune_rules.Vendor_rules
module Rev_store = Dune_pkg.Rev_store
module OpamUrl = Dune_pkg.OpamUrl
module Solver_env = Dune_pkg.Solver_env
module Package_version = Dune_pkg.Package_version

(* Default patches directory for user patches *)
let default_patches_dir = Path.Source.of_string "patches"

(* Source marker file to track what was fetched.
   This allows detecting stale directories when re-locking. *)
let source_marker_filename = ".dune-source-url"

(* Write marker for non-git sources (URL + checksum) *)
let write_source_marker ~target_dir (source : Source.t) =
  let marker_path = Path.relative target_dir source_marker_filename in
  let _, url = source.url in
  let url_str = OpamUrl.to_string url in
  let content =
    match source.checksum with
    | None -> url_str
    | Some (_, checksum) ->
      sprintf "%s\n%s" url_str (Dune_pkg.Checksum.to_string checksum)
  in
  Io.write_file marker_path content
;;

(* Write marker for git sources (URL + commit hash) *)
let write_git_source_marker ~target_dir (source : Source.t) ~commit =
  let marker_path = Path.relative target_dir source_marker_filename in
  let _, url = source.url in
  let url_str = OpamUrl.to_string url in
  let content = sprintf "%s\n%s" url_str commit in
  Io.write_file marker_path content
;;

let read_source_marker target_dir =
  let marker_path = Path.relative target_dir source_marker_filename in
  if Path.exists marker_path then Some (Io.read_file marker_path) else None
;;

(* For git sources, we need to resolve to check if there's a new commit.
   This is called when checking staleness. *)
let resolve_git_head rev_store ~url:(url_loc, url) =
  let open Fiber.O in
  OpamUrl.resolve url ~loc:url_loc rev_store
  >>| function
  | Error _ -> None
  | Ok r ->
    let obj =
      match r with
      | OpamUrl.Resolved obj -> (obj :> Dune_pkg.Rev_store.Object.t)
      | OpamUrl.Unresolved obj -> obj
    in
    Some (Dune_pkg.Rev_store.Object.to_hex obj)
;;

(* Check if the marker matches the source.
   For git sources, we compare against what would be fetched (current HEAD). *)
let source_matches_marker_fiber rev_store target_dir (source : Source.t) =
  let open Fiber.O in
  match read_source_marker target_dir with
  | None -> Fiber.return false (* No marker = stale or never fetched properly *)
  | Some content ->
    let _, url = source.url in
    let url_str = OpamUrl.to_string url in
    if OpamUrl.is_version_control url
    then
      (* Git source - resolve current HEAD and compare *)
      let+ current_head_opt = resolve_git_head rev_store ~url:source.url in
      match current_head_opt with
      | None -> false (* Can't resolve, consider stale *)
      | Some current_head ->
        let expected = sprintf "%s\n%s" url_str current_head in
        String.equal content expected
    else (
      (* Non-git source - compare URL + checksum *)
      let expected =
        match source.checksum with
        | None -> url_str
        | Some (_, checksum) ->
          sprintf "%s\n%s" url_str (Dune_pkg.Checksum.to_string checksum)
      in
      Fiber.return (String.equal content expected))
;;

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
  | Ok commit_opt -> Fiber.return commit_opt
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

(* Returns (commit_hash option, error) - commit_hash is Some for git sources *)
let do_fetch ~rev_store ~source ~target =
  let open Fiber.O in
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
    then
      copy_directory ~src:src_path ~dst:(Path.Source.of_string target)
      >>| Result.map ~f:(fun () -> None)
    else (
      (* Local archive file - extract it *)
      let { Source.url; checksum } = source in
      let checksum_opt = Option.map checksum ~f:snd in
      Dune_pkg.Fetch.fetch ~unpack:true ~checksum:checksum_opt ~target:target_path ~url
      >>| Result.map ~f:(fun () -> None))
  | `Fetch ->
    let { Source.url; checksum } = source in
    let _, opam_url = url in
    let checksum_opt = Option.map checksum ~f:snd in
    (* Check if this is a git URL *)
    if OpamUrl.is_version_control opam_url
    then
      Dune_pkg.Fetch.fetch_git_with_rev rev_store ~target:target_path ~url
      >>| Result.map ~f:Option.some
    else
      (* HTTP archive fetch - use Fetch.fetch directly *)
      Dune_pkg.Fetch.fetch ~unpack:true ~checksum:checksum_opt ~target:target_path ~url
      >>| Result.map ~f:(fun () -> None)
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

let apply_user_patch ~verbose ~target_dir ~patch_source_path =
  let open Fiber.O in
  let patch_path = Path.source patch_source_path in
  if not (Path.exists patch_path)
  then Fiber.return ()
  else (
    verbose (sprintf "  user-patch: %s" (Path.Source.to_string patch_source_path));
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

(* Two levels of verbosity:
   - status: one line per package (shown in status line, not new lines)
   - verbose: detailed debug messages (-vv only, prints new lines) *)
let status msg =
  match !Dune_engine.Clflags.display with
  | Verbose | Short ->
    (* Use status line to avoid adding newlines - updates in place *)
    Console.Status_line.set (Constant (Pp.text msg))
  | Quiet -> ()
;;

let verbose msg =
  match !Dune_engine.Clflags.display with
  | Verbose -> Console.print_user_message (User_message.make [ Pp.text msg ])
  | Quiet | Short -> ()
;;

let start_fetch ~name ~version =
  let pkg_str =
    sprintf
      "%s.%s"
      (Package_name.to_string name)
      (Dune_pkg.Package_version.to_string version)
  in
  status (sprintf "Fetching %s to duniverse/%s" pkg_str pkg_str);
  Dune_engine.Progress.start_target (Dune_engine.Progress.Target.fetch pkg_str);
  pkg_str
;;

(* Determine the final directory name for a source group.
   Uses the project name from dune-project if available, otherwise falls back
   to the primary package name. Returns (dirname, version). *)
let determine_dirname group fetched_dir =
  let { Vendor.primary_name; primary_version; _ } = group in
  (* Check if fetched source has a dune-project with a name *)
  let fetched_source_dir = Path.Source.of_string (Path.to_string fetched_dir) in
  match Vendor_rules.read_project_name fetched_source_dir with
  | Some project_name -> project_name, primary_version
  | None -> Package_name.to_string primary_name, primary_version
;;

(* Check if a directory exists and has a matching source marker.
   Returns Some path if fresh, None if missing or stale.
   For git sources, this resolves the current HEAD to check freshness. *)
let find_fresh_dir ~rev_store ~source initial_target primary_name =
  let open Fiber.O in
  (* First check the initial target *)
  let check_dir dir_path =
    if not (Path.exists dir_path)
    then Fiber.return None
    else
      let+ is_fresh = source_matches_marker_fiber rev_store dir_path source in
      if is_fresh then Some dir_path else None
  in
  check_dir initial_target
  >>= function
  | Some _ as result -> Fiber.return result
  | None ->
    (* Look up in duniverse/dune's library->directory mapping.
       Use the primary package name as the expected library name. *)
    let pkg_name = Package_name.to_string primary_name in
    (match Vendor_rules.find_dir_for_library pkg_name with
     | Some dirname ->
       let dir_path = Path.source (Path.Source.relative Vendor.default_dir dirname) in
       check_dir dir_path
     | None -> Fiber.return None)
;;

(* Fetch a source group - packages sharing the same source are fetched once.
   Returns (was_fetched, final_dirname) where final_dirname is based on the
   project name from dune-project if available. *)
let fetch_source_group ~rev_store ~platform ~patches_dir ~pkgs_by_name group =
  let open Fiber.O in
  let { Vendor.packages; source; primary_name; primary_version } = group in
  let initial_target = Vendor.source_group_dir group |> Path.source in
  let initial_target_path = Path.to_string initial_target in
  find_fresh_dir ~rev_store ~source initial_target primary_name
  >>= function
  | Some existing ->
    (* Already fetched with matching source - count all packages as cached *)
    let dirname = Path.basename existing in
    status (sprintf "Cached %s" dirname);
    List.iter packages ~f:(fun _ -> Dune_engine.Progress.incr_cached ());
    Fiber.return (false, dirname)
  | None ->
    (* Remove stale directory if it exists but source changed *)
    if Path.exists initial_target
    then (
      verbose (sprintf "  source changed, re-fetching %s" (Path.to_string initial_target));
      Path.rm_rf initial_target);
    let pkg_str = start_fetch ~name:primary_name ~version:primary_version in
    let* result = do_fetch ~rev_store ~source ~target:initial_target_path in
    let* commit_opt =
      handle_fetch_error ~name:primary_name ~version:primary_version result
    in
    (* Determine the final dirname based on dune-project *)
    let final_name, final_version = determine_dirname group initial_target in
    let final_dirname =
      sprintf "%s.%s" final_name (Package_version.to_string final_version)
    in
    let final_target =
      Path.source (Path.Source.relative Vendor.default_dir final_dirname)
    in
    (* Rename if the project name differs from the initial target *)
    let target =
      if String.equal (Path.to_string initial_target) (Path.to_string final_target)
      then initial_target
      else (
        verbose (sprintf "  renaming to %s (from dune-project)" final_dirname);
        (* Remove existing target if present (stale from previous fetch) *)
        if Path.exists final_target then Path.rm_rf final_target;
        Unix.rename (Path.to_string initial_target) (Path.to_string final_target);
        final_target)
    in
    (* Apply extra sources and patches for all packages in the group *)
    let* () =
      Fiber.sequential_iter packages ~f:(fun (name, _version) ->
        match Package_name.Map.find pkgs_by_name name with
        | None -> Fiber.return ()
        | Some pkg ->
          let { Pkg.Info.extra_sources; _ } = pkg.Pkg.info in
          Fiber.sequential_iter extra_sources ~f:(fun extra ->
            let local_path, _ = extra in
            verbose (sprintf "  extra-source: %s" (Path.Local.to_string local_path));
            let* result = fetch_extra_source ~rev_store ~target_dir:target extra in
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
    (* Apply patches for all packages in the group *)
    let* () =
      Fiber.sequential_iter packages ~f:(fun (name, version) ->
        match Package_name.Map.find pkgs_by_name name with
        | None -> Fiber.return ()
        | Some pkg ->
          let patches = Vendor.get_patches pkg ~platform in
          let* () =
            Fiber.sequential_iter patches ~f:(fun patch_sw ->
              match Dune_lang.String_with_vars.text_only patch_sw with
              | None ->
                User_error.raise
                  [ Pp.text "Patch file path contains variables, which is not supported" ]
              | Some patch_file ->
                verbose (sprintf "  patch: %s" patch_file);
                let patch_local = Path.Local.of_string patch_file in
                apply_patch ~target_dir:target ~patch_file:patch_local)
          in
          let user_patch = user_patch_path ~patches_dir name version in
          apply_user_patch ~verbose ~target_dir:target ~patch_source_path:user_patch)
    in
    (* Write source marker so we can detect stale directories on re-lock *)
    (match commit_opt with
     | Some commit -> write_git_source_marker ~target_dir:target source ~commit
     | None -> write_source_marker ~target_dir:target source);
    Dune_engine.Progress.finish_target ~name:pkg_str;
    Fiber.return (true, final_dirname)
;;

let fetch_duniverse ~lock_dir_path ~solver_env ~local_packages () =
  let open Fiber.O in
  let lock_path = Path.source lock_dir_path in
  let* lock_dir, opam_files =
    Lock_pkg.read_disk_with_opam_files ~solver_env ~local_packages lock_path
  in
  let all_pkgs = Lock_dir.Packages.to_pkg_list lock_dir.packages in
  let fetchable_pkgs =
    List.filter all_pkgs ~f:(fun (pkg : Pkg.t) -> Option.is_some pkg.info.source)
  in
  let no_source_count = List.length all_pkgs - List.length fetchable_pkgs in
  if List.is_empty fetchable_pkgs
  then (
    if no_source_count > 0
    then
      verbose
        (sprintf
           "%d package(s) have no source URL (likely local packages)."
           no_source_count);
    Fiber.return ())
  else (
    let duniverse_path = Path.source Vendor.default_dir in
    if not (Path.exists duniverse_path) then Path.mkdir_p duniverse_path;
    let marker_path =
      Path.source (Path.Source.relative Vendor.default_dir Vendor.marker_filename)
    in
    (* Group packages by source to avoid duplicate fetches *)
    let source_groups = Vendor.group_by_source fetchable_pkgs in
    (* Build map for classifying packages *)
    let pkg_classifications =
      List.fold_left fetchable_pkgs ~init:Package_name.Map.empty ~f:(fun acc pkg ->
        let name = pkg.Pkg.info.name in
        Package_name.Map.set acc name (Vendor.classify_build_method pkg))
    in
    (* Build map for looking up packages by name *)
    let pkgs_by_name =
      List.fold_left fetchable_pkgs ~init:Package_name.Map.empty ~f:(fun acc pkg ->
        Package_name.Map.add_exn acc pkg.Pkg.info.name pkg)
    in
    Dune_engine.Progress.reset ();
    Dune_engine.Progress.set_total (List.length source_groups);
    let* rev_store = Rev_store.get in
    let* platform = Pkg_common.poll_solver_env_from_current_system () in
    let patches_dir = default_patches_dir in
    let* fetch_results =
      Fiber.parallel_map source_groups ~f:(fun group ->
        fetch_source_group ~rev_store ~platform ~patches_dir ~pkgs_by_name group)
    in
    (* Build a map from source groups to their actual directory names *)
    let group_dirnames =
      List.map2 source_groups fetch_results ~f:(fun group (_was_fetched, dirname) ->
        group, dirname)
    in
    (* Write opam files for each package in duniverse.
       For single-file format, opam_files comes from repo derivation.
       For directory format, opam_files is empty - tests should use single-file format. *)
    let opam_files_by_name =
      List.fold_left
        opam_files
        ~init:Package_name.Map.empty
        ~f:(fun acc (name, content) -> Package_name.Map.set acc name content)
    in
    List.iter group_dirnames ~f:(fun (group, dirname) ->
      let { Vendor.packages; _ } = group in
      List.iter packages ~f:(fun (name, _version) ->
        match Package_name.Map.find opam_files_by_name name with
        | None -> ()
        | Some opam_content ->
          (* Write opam file to the package directory.
             If opam/ is a directory, write inside it as opam/opam or opam/<pkg>.opam *)
          let pkg_dir = Path.source (Path.Source.relative Vendor.default_dir dirname) in
          let pkg_name = Package_name.to_string name in
          let base_opam_path = Path.relative pkg_dir "opam" in
          let opam_path =
            match Path.stat base_opam_path with
            | Ok { st_kind = S_DIR; _ } ->
              (* opam/ is a directory - write inside it *)
              let inside_path = Path.relative base_opam_path "opam" in
              if Path.exists inside_path
              then inside_path
              else Path.relative base_opam_path (pkg_name ^ ".opam")
            | _ -> base_opam_path
          in
          (* Only write if content changed or file doesn't exist *)
          let should_write =
            match Path.stat opam_path with
            | Error _ -> true (* doesn't exist *)
            | Ok { st_kind = S_REG; _ } -> Io.read_file opam_path <> opam_content
            | Ok _ -> false (* unexpected kind, don't overwrite *)
          in
          if should_write
          then (
            verbose (sprintf "  writing %s" (Path.to_string opam_path));
            Io.write_file opam_path opam_content)));
    (* After fetching, generate duniverse/dune with vendor stanzas *)
    (* We scan each fetched directory for libraries to include in vendor stanzas *)
    let dune_content =
      let vendor_stanzas =
        List.filter_map group_dirnames ~f:(fun (group, dirname) ->
          let { Vendor.packages; _ } = group in
          let pkg_dir = Path.Source.relative Vendor.default_dir dirname in
          (* Check if this is an opam package (needs sandbox) *)
          let is_opam =
            List.for_all packages ~f:(fun (name, _) ->
              match Package_name.Map.find pkg_classifications name with
              | Some Vendor.Opam_sandboxed -> true
              | _ -> false)
          in
          (* Scan for libraries using cascading strategy:
             dune files -> META files -> opam files *)
          let pkg_name =
            match packages with
            | (name, _) :: _ -> Package_name.to_string name
            | [] -> dirname
          in
          let libs = Vendor_rules.scan_libraries pkg_dir ~pkg_name in
          if is_opam
          then
            (* Opam package - needs sandbox, include libraries if found *)
            if List.is_empty libs
            then Some (sprintf "(vendor %s (mode opam))" dirname)
            else (
              let libs_str = String.concat ~sep:" " libs in
              Some (sprintf "(vendor %s (mode opam) (libraries %s))" dirname libs_str))
          else if List.is_empty libs
          then None
          else (
            let libs_str = String.concat ~sep:" " libs in
            Some (sprintf "(vendor %s (libraries %s))" dirname libs_str)))
      in
      let lines =
        [ "; This directory is managed by dune pkg"; "(vendored_dirs *)" ]
        @ vendor_stanzas
      in
      String.concat ~sep:"\n" lines ^ "\n"
    in
    if (not (Path.exists marker_path)) || Io.read_file marker_path <> dune_content
    then Io.write_file marker_path dune_content;
    (* Generate .gitignore to exclude fetched sources by default *)
    let gitignore_path =
      Path.source (Path.Source.relative Vendor.default_dir ".gitignore")
    in
    let gitignore_content =
      {|# Auto-generated by dune pkg fetch
# User patches are stored in patches/, not here
*
!.gitignore
!dune
|}
    in
    if
      (not (Path.exists gitignore_path))
      || Io.read_file gitignore_path <> gitignore_content
    then Io.write_file gitignore_path gitignore_content;
    Fiber.return ())
;;

(* NOTE: This function is currently unused. Auto-fetch is now a no-op because
   the build system handles fetching through fetch_rules.ml. Keeping this code
   for reference in case we need pre-fetching to _build/.pkgs/ in the future. *)
let _auto_fetch_missing ~lock_dir_path ~solver_env ~local_packages () =
  let open Fiber.O in
  let lock_path = Path.source lock_dir_path in
  let* lock_dir = Lock_pkg.read_disk ~solver_env ~local_packages lock_path in
  let all_pkgs = Lock_dir.Packages.to_pkg_list lock_dir.packages in
  (* Filter to packages with sources *)
  let fetchable_pkgs =
    List.filter all_pkgs ~f:(fun (pkg : Pkg.t) -> Option.is_some pkg.info.source)
  in
  if List.is_empty fetchable_pkgs
  then Fiber.return ()
  else (
    let source_groups = Vendor.group_by_source fetchable_pkgs in
    let pkgs_by_name =
      List.fold_left fetchable_pkgs ~init:Package_name.Map.empty ~f:(fun acc pkg ->
        Package_name.Map.add_exn acc pkg.Pkg.info.name pkg)
    in
    let* rev_store = Rev_store.get in
    let* platform = Pkg_common.poll_solver_env_from_current_system () in
    let patches_dir = default_patches_dir in
    (* Check which groups need fetching (missing from _build/.pkgs/) *)
    let* groups_to_fetch =
      Fiber.parallel_map source_groups ~f:(fun group ->
        let { Vendor.primary_name; _ } = group in
        let pkg_dir =
          Path.Build.relative
            (Path.Build.relative Dune_engine.Dpath.Build.pkgs_dir "default")
            (Package_name.to_string primary_name)
        in
        let target = Path.Build.relative pkg_dir "source" in
        let target_path = Path.build target in
        if Path.exists target_path then Fiber.return None else Fiber.return (Some group))
      >>| List.filter_map ~f:Fun.id
    in
    if List.is_empty groups_to_fetch
    then Fiber.return ()
    else (
      Dune_engine.Progress.reset ();
      Dune_engine.Progress.set_total (List.length groups_to_fetch);
      (* Fetch to _build/.pkgs/default/<pkg>/source/ *)
      let* _ =
        Fiber.parallel_map groups_to_fetch ~f:(fun group ->
          let { Vendor.primary_name; primary_version; source; _ } = group in
          let pkg_str =
            sprintf
              "%s.%s"
              (Package_name.to_string primary_name)
              (Package_version.to_string primary_version)
          in
          let pkg_dir =
            Path.Build.relative
              (Path.Build.relative Dune_engine.Dpath.Build.pkgs_dir "default")
              (Package_name.to_string primary_name)
          in
          let target = Path.Build.relative pkg_dir "source" in
          let target_path = Path.build target in
          let target_str = Path.to_string target_path in
          Path.mkdir_p (Path.build pkg_dir);
          (* Use do_fetch which handles all source types *)
          let* result = do_fetch ~rev_store ~source ~target:target_str in
          (match result with
           | Ok _ -> ()
           | Error (Dune_pkg.Fetch.Unavailable msg) ->
             let msg_str =
               match msg with
               | None -> "unknown error"
               | Some m -> Format.asprintf "%a" Pp.to_fmt (User_message.pp m)
             in
             User_error.raise [ Pp.textf "Failed to fetch %s: %s" pkg_str msg_str ]
           | Error (Dune_pkg.Fetch.Checksum_mismatch actual) ->
             User_error.raise
               [ Pp.textf
                   "Checksum mismatch for %s (got %s)"
                   pkg_str
                   (Dune_pkg.Checksum.to_string actual)
               ]);
          (* Fetch extra sources *)
          let* () =
            match Package_name.Map.find pkgs_by_name primary_name with
            | None -> Fiber.return ()
            | Some pkg ->
              Fiber.sequential_iter
                pkg.Pkg.info.extra_sources
                ~f:(fun (local_path, extra_source) ->
                  let extra_target = Path.append_local target_path local_path in
                  let checksum = Option.map extra_source.Source.checksum ~f:snd in
                  let* result =
                    Dune_pkg.Fetch.fetch
                      ~unpack:false
                      ~checksum
                      ~target:extra_target
                      ~url:extra_source.url
                  in
                  match result with
                  | Ok () -> Fiber.return ()
                  | Error (Dune_pkg.Fetch.Unavailable msg) ->
                    let msg_str =
                      match msg with
                      | None -> "unknown error"
                      | Some m -> Format.asprintf "%a" Pp.to_fmt (User_message.pp m)
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
                      ])
          in
          (* Apply patches *)
          let* () =
            match Package_name.Map.find pkgs_by_name primary_name with
            | None -> Fiber.return ()
            | Some pkg ->
              let patches = Vendor.get_patches pkg ~platform in
              let* () =
                Fiber.sequential_iter patches ~f:(fun patch_sw ->
                  match Dune_lang.String_with_vars.text_only patch_sw with
                  | None ->
                    User_error.raise
                      [ Pp.text
                          "Patch file path contains variables, which is not supported"
                      ]
                  | Some patch_file ->
                    let patch_local = Path.Local.of_string patch_file in
                    apply_patch ~target_dir:target_path ~patch_file:patch_local)
              in
              let user_patch =
                user_patch_path ~patches_dir primary_name primary_version
              in
              apply_user_patch
                ~verbose
                ~target_dir:target_path
                ~patch_source_path:user_patch
          in
          Dune_engine.Progress.finish_target ~name:pkg_str;
          Fiber.return (true, pkg_str))
      in
      Fiber.return ()))
;;

(** Auto-fetch is now a no-op.

    For non-vendored builds, the build system fetches sources on-demand through
    fetch_rules.ml. Auto-fetch was designed for vendoring to duniverse/, which
    should now be done explicitly with 'dune pkg vendor'.

    If you want to pre-fetch for offline builds, use 'dune pkg fetch'. *)
let do_auto_fetch () = Fiber.return ()

(* Fetch command: pre-fetch package sources to _build for offline builds *)
let fetch_term =
  let+ builder = Common.Builder.term
  and+ lock_dirs_arg = Pkg_common.Lock_dirs_arg.term in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler.go_with_rpc_server ~common ~config (fun () ->
    let open Fiber.O in
    Pkg_common.check_pkg_management_enabled ()
    >>>
    let* solver_env = Pkg_common.poll_solver_env_from_current_system ()
    and* workspace = Memo.run (Workspace.workspace ())
    and* local_packages = Memo.run Pkg_common.find_local_packages in
    let local_packages =
      Package_name.Map.values local_packages
      |> List.map ~f:Dune_pkg.Local_package.for_solver
    in
    let lock_dirs =
      Pkg_common.Lock_dirs_arg.lock_dirs_of_workspace lock_dirs_arg workspace
    in
    match lock_dirs with
    | [] ->
      User_error.raise [ Pp.text "No lock directories found. Run 'dune pkg lock' first." ]
    | lock_dir_path :: _ ->
      (* Pre-fetch all packages to _build for offline builds.
         This triggers the standard fetch mechanism without vendoring. *)
      let lock_path = Path.source lock_dir_path in
      let* lock_dir = Lock_pkg.read_disk ~solver_env ~local_packages lock_path in
      let all_pkgs = Lock_dir.Packages.to_pkg_list lock_dir.packages in
      let fetchable_pkgs =
        List.filter all_pkgs ~f:(fun (pkg : Pkg.t) -> Option.is_some pkg.info.source)
      in
      if List.is_empty fetchable_pkgs
      then (
        verbose "No packages with sources to fetch.";
        Fiber.return ())
      else (
        let source_groups = Vendor.group_by_source fetchable_pkgs in
        let pkgs_by_name =
          List.fold_left fetchable_pkgs ~init:Package_name.Map.empty ~f:(fun acc pkg ->
            Package_name.Map.add_exn acc pkg.Pkg.info.name pkg)
        in
        Dune_engine.Progress.reset ();
        Dune_engine.Progress.set_total (List.length source_groups);
        let* rev_store = Rev_store.get in
        let* platform = Pkg_common.poll_solver_env_from_current_system () in
        let patches_dir = default_patches_dir in
        (* Fetch to _build/.pkgs/default/<pkg>/source/ *)
        let* _ =
          Fiber.parallel_map source_groups ~f:(fun group ->
            let { Vendor.primary_name; primary_version; source; _ } = group in
            let pkg_str =
              sprintf
                "%s.%s"
                (Package_name.to_string primary_name)
                (Package_version.to_string primary_version)
            in
            let pkg_dir =
              Path.Build.relative
                (Path.Build.relative Dune_engine.Dpath.Build.pkgs_dir "default")
                (Package_name.to_string primary_name)
            in
            let target = Path.Build.relative pkg_dir "source" in
            let target_path = Path.build target in
            let target_str = Path.to_string target_path in
            (* Check if already fetched *)
            if Path.exists target_path
            then (
              verbose (sprintf "  %s (cached)" pkg_str);
              Dune_engine.Progress.finish_target ~name:pkg_str;
              Fiber.return (false, pkg_str))
            else (
              Path.mkdir_p (Path.build pkg_dir);
              verbose (sprintf "  fetching %s" pkg_str);
              (* Use do_fetch which handles all source types *)
              let* result = do_fetch ~rev_store ~source ~target:target_str in
              (match result with
               | Ok _ -> ()
               | Error (Dune_pkg.Fetch.Unavailable msg) ->
                 let msg_str =
                   match msg with
                   | None -> "unknown error"
                   | Some m -> Format.asprintf "%a" Pp.to_fmt (User_message.pp m)
                 in
                 User_error.raise [ Pp.textf "Failed to fetch %s: %s" pkg_str msg_str ]
               | Error (Dune_pkg.Fetch.Checksum_mismatch actual) ->
                 User_error.raise
                   [ Pp.textf
                       "Checksum mismatch for %s (got %s)"
                       pkg_str
                       (Dune_pkg.Checksum.to_string actual)
                   ]);
              (* Fetch extra sources *)
              let* () =
                match Package_name.Map.find pkgs_by_name primary_name with
                | None -> Fiber.return ()
                | Some pkg ->
                  Fiber.sequential_iter
                    pkg.Pkg.info.extra_sources
                    ~f:(fun (local_path, extra_source) ->
                      let extra_target = Path.append_local target_path local_path in
                      verbose (sprintf "    extra: %s" (Path.Local.to_string local_path));
                      let checksum = Option.map extra_source.Source.checksum ~f:snd in
                      let* result =
                        Dune_pkg.Fetch.fetch
                          ~unpack:false
                          ~checksum
                          ~target:extra_target
                          ~url:extra_source.url
                      in
                      match result with
                      | Ok () -> Fiber.return ()
                      | Error (Dune_pkg.Fetch.Unavailable msg) ->
                        let msg_str =
                          match msg with
                          | None -> "unknown error"
                          | Some m -> Format.asprintf "%a" Pp.to_fmt (User_message.pp m)
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
                          ])
              in
              (* Apply patches *)
              let* () =
                match Package_name.Map.find pkgs_by_name primary_name with
                | None -> Fiber.return ()
                | Some pkg ->
                  let patches = Vendor.get_patches pkg ~platform in
                  let* () =
                    Fiber.sequential_iter patches ~f:(fun patch_sw ->
                      match Dune_lang.String_with_vars.text_only patch_sw with
                      | None ->
                        User_error.raise
                          [ Pp.text
                              "Patch file path contains variables, which is not supported"
                          ]
                      | Some patch_file ->
                        verbose (sprintf "  patch: %s" patch_file);
                        let patch_local = Path.Local.of_string patch_file in
                        apply_patch ~target_dir:target_path ~patch_file:patch_local)
                  in
                  let user_patch =
                    user_patch_path ~patches_dir primary_name primary_version
                  in
                  apply_user_patch
                    ~verbose
                    ~target_dir:target_path
                    ~patch_source_path:user_patch
              in
              Dune_engine.Progress.finish_target ~name:pkg_str;
              Fiber.return (true, pkg_str)))
        in
        verbose "Packages pre-fetched to _build/.pkgs/";
        Fiber.return ()))
;;

let fetch_info =
  let doc = "Pre-fetch package sources for offline builds" in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        "Pre-fetches the source code for all locked packages to _build/.pkgs/ for \
         offline builds. This is the same location where packages are fetched during \
         normal builds."
    ; `P "Use 'dune pkg vendor' to vendor packages into your source tree instead."
    ; `S "EXAMPLES"
    ; `Pre "  dune pkg fetch"
    ]
  in
  Cmd.info "fetch" ~doc ~man
;;

let command = Cmd.v fetch_info fetch_term

(* Vendor command: vendor package sources to duniverse/ *)
let vendor_term =
  let+ builder = Common.Builder.term
  and+ lock_dirs_arg = Pkg_common.Lock_dirs_arg.term in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler.go_with_rpc_server ~common ~config (fun () ->
    let open Fiber.O in
    Pkg_common.check_pkg_management_enabled ()
    >>>
    let* solver_env = Pkg_common.poll_solver_env_from_current_system ()
    and* workspace = Memo.run (Workspace.workspace ())
    and* local_packages = Memo.run Pkg_common.find_local_packages in
    let local_packages =
      Package_name.Map.values local_packages
      |> List.map ~f:Dune_pkg.Local_package.for_solver
    in
    let lock_dirs =
      Pkg_common.Lock_dirs_arg.lock_dirs_of_workspace lock_dirs_arg workspace
    in
    match lock_dirs with
    | [] ->
      User_error.raise [ Pp.text "No lock directories found. Run 'dune pkg lock' first." ]
    | lock_dir_path :: _ -> fetch_duniverse ~lock_dir_path ~solver_env ~local_packages ())
;;

let vendor_info =
  let doc = "Vendor package sources to duniverse directory" in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        "Vendors the source code for all locked packages to the duniverse directory. \
         Packages using dune as their build system are built alongside your project \
         code, while other packages are built in the .pkg sandbox but from duniverse \
         sources."
    ; `P
        "All packages are downloaded to duniverse/<name>.<version>/ and can be edited \
         directly. Changes will be reflected in your project builds."
    ; `S "EXAMPLES"
    ; `Pre "  dune pkg vendor"
    ]
  in
  Cmd.info "vendor" ~doc ~man
;;

let vendor_command = Cmd.v vendor_info vendor_term
