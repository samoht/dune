open Import
module Lock_dir = Dune_pkg.Lock
module Duniverse = Dune_pkg.Duniverse
module Package_version = Dune_pkg.Package_version

(* Default patches directory *)
let default_patches_dir = Path.Source.of_string "patches"

(* Get the patch file path for a package *)
let patch_path ~patches_dir name version =
  let filename =
    sprintf
      "%s@%s.patch"
      (Package_name.to_string name)
      (Package_version.to_string version)
  in
  Path.Source.relative patches_dir filename
;;

(* Find a package in the lock directory by name *)
let find_package lock_dir name =
  let all_pkgs = Lock_dir.Packages.to_pkg_list lock_dir.Lock_dir.packages in
  List.find all_pkgs ~f:(fun pkg -> Package_name.equal pkg.Lock_dir.Pkg.info.name name)
;;

(* List all patches and their status *)
let list_patches ~lock_dir_path ~patches_dir =
  let lock_dir = Lock_dir.read_disk_exn (Path.source lock_dir_path) in
  let patches_path = Path.source patches_dir in
  if not (Path.exists patches_path)
  then (
    Console.print_user_message
      (User_message.make [ Pp.text "No patches directory found." ]);
    Fiber.return ())
  else (
    let patch_files =
      match Path.readdir_unsorted patches_path with
      | Ok files -> files
      | Error _ -> []
    in
    let patch_entries =
      List.filter_map patch_files ~f:(fun filename ->
        (* Parse filename: name@version.patch *)
        match String.rsplit2 filename ~on:'.' with
        | Some (base, "patch") ->
          (match String.rsplit2 base ~on:'@' with
           | Some (name_str, version_str) ->
             let name = Package_name.of_string name_str in
             let version = Package_version.of_string version_str in
             Some (name, version, filename)
           | None -> None)
        | _ -> None)
    in
    if List.is_empty patch_entries
    then Console.print_user_message (User_message.make [ Pp.text "No patches found." ])
    else (
      Console.print_user_message (User_message.make [ Pp.text "Patches:" ]);
      List.iter patch_entries ~f:(fun (name, version, filename) ->
        let pkg_opt = find_package lock_dir name in
        let status =
          match pkg_opt with
          | None -> "(package not in lock)"
          | Some pkg ->
            if Package_version.equal pkg.info.version version
            then "(current)"
            else
              sprintf
                "(stale: locked version is %s)"
                (Package_version.to_string pkg.info.version)
        in
        Console.print_user_message
          (User_message.make
             [ Pp.textf
                 "  %s  %s/%s  %s"
                 filename
                 (Path.Source.to_string patches_dir)
                 filename
                 status
             ])));
    Fiber.return ())
;;

(* Remove a patch *)
let remove_patch ~patches_dir name version =
  let patch_file = patch_path ~patches_dir name version in
  let patch_full_path = Path.source patch_file in
  if not (Path.exists patch_full_path)
  then
    User_error.raise
      [ Pp.textf "Patch %s does not exist" (Path.Source.to_string patch_file) ]
  else (
    Fpath.unlink_exn (Path.to_string patch_full_path);
    Console.print_user_message
      (User_message.make
         [ Pp.textf "Removed %s" (Path.Source.to_string patch_file)
         ; Pp.text "Re-fetch with: dune pkg fetch"
         ]);
    Fiber.return ())
;;

(* Generate a patch from local changes *)
let commit_patch ~patches_dir name version =
  let duniverse_pkg_dir = Duniverse.package_dir name version in
  let pkg_path = Path.source duniverse_pkg_dir in
  if not (Path.exists pkg_path)
  then
    User_error.raise
      [ Pp.textf
          "Package directory %s does not exist. Run 'dune pkg fetch' first."
          (Path.Source.to_string duniverse_pkg_dir)
      ];
  (* For now, we provide instructions to the user on how to generate the patch
     manually. A future version could integrate with git directly. *)
  let git_dir = Path.relative pkg_path ".git" in
  if Path.exists git_dir
  then (
    let patch_file = patch_path ~patches_dir name version in
    Console.print_user_message
      (User_message.make
         [ Pp.textf
             "To generate patch for %s.%s:"
             (Package_name.to_string name)
             (Package_version.to_string version)
         ; Pp.nop
         ; Pp.textf "  cd %s" (Path.to_string pkg_path)
         ; Pp.text "  git diff > ../../patches/$(basename $PWD | tr '.' '@').patch"
         ; Pp.nop
         ; Pp.text "Or manually:"
         ; Pp.textf
             "  git -C %s diff > %s"
             (Path.to_string pkg_path)
             (Path.Source.to_string patch_file)
         ; Pp.nop
         ; Pp.text "The patch will be applied on next 'dune pkg fetch'"
         ]);
    Fiber.return ())
  else (
    Console.print_user_message
      (User_message.make
         [ Pp.textf
             "Package directory %s is not a git repository."
             (Path.Source.to_string duniverse_pkg_dir)
         ; Pp.nop
         ; Pp.text "To enable patching, initialize the duniverse directory with git:"
         ; Pp.textf
             "  cd %s && git init && git add . && git commit -m 'initial'"
             (Path.Source.to_string duniverse_pkg_dir)
         ]);
    Fiber.return ())
;;

(* Prepare a package for patching *)
let prepare_patch ~lock_dir_path name =
  let lock_dir = Lock_dir.read_disk_exn (Path.source lock_dir_path) in
  let pkg =
    match find_package lock_dir name with
    | Some pkg -> pkg
    | None ->
      User_error.raise
        [ Pp.textf "Package %s not found in lock directory" (Package_name.to_string name)
        ]
  in
  let version = pkg.info.version in
  let duniverse_pkg_dir = Duniverse.package_dir name version in
  let pkg_path = Path.source duniverse_pkg_dir in
  if not (Path.exists pkg_path)
  then
    User_error.raise
      [ Pp.textf
          "Package directory %s does not exist."
          (Path.Source.to_string duniverse_pkg_dir)
      ; Pp.text "Run 'dune pkg fetch' first to download package sources."
      ];
  Console.print_user_message
    (User_message.make
       [ Pp.textf
           "Preparing %s.%s for patching..."
           (Package_name.to_string name)
           (Package_version.to_string version)
       ; Pp.textf "Edit files in %s/ then run:" (Path.Source.to_string duniverse_pkg_dir)
       ; Pp.textf "  dune pkg patch --commit %s" (Package_name.to_string name)
       ]);
  Fiber.return ()
;;

(* Main command logic *)
type action =
  | List
  | Prepare of Package_name.t
  | Commit of Package_name.t
  | Remove of Package_name.t

let run ~lock_dir_path ~patches_dir action =
  match action with
  | List -> list_patches ~lock_dir_path ~patches_dir
  | Prepare name -> prepare_patch ~lock_dir_path name
  | Commit name ->
    let lock_dir = Lock_dir.read_disk_exn (Path.source lock_dir_path) in
    let pkg =
      match find_package lock_dir name with
      | Some pkg -> pkg
      | None ->
        User_error.raise
          [ Pp.textf
              "Package %s not found in lock directory"
              (Package_name.to_string name)
          ]
    in
    commit_patch ~patches_dir name pkg.info.version
  | Remove name ->
    let lock_dir = Lock_dir.read_disk_exn (Path.source lock_dir_path) in
    let pkg =
      match find_package lock_dir name with
      | Some pkg -> pkg
      | None ->
        User_error.raise
          [ Pp.textf
              "Package %s not found in lock directory"
              (Package_name.to_string name)
          ]
    in
    remove_patch ~patches_dir name pkg.info.version
;;

let term =
  let+ builder = Common.Builder.term
  and+ list_flag =
    Arg.(
      value & flag & info [ "list"; "l" ] ~doc:(Some "List all patches and their status"))
  and+ commit_flag =
    Arg.(
      value
      & opt (some string) None
      & info
          [ "commit"; "c" ]
          ~docv:"PKG"
          ~doc:(Some "Generate patch from local changes to PKG"))
  and+ remove_flag =
    Arg.(
      value
      & opt (some string) None
      & info [ "remove"; "r" ] ~docv:"PKG" ~doc:(Some "Remove patch for PKG"))
  and+ pkg_arg = Arg.(value & pos 0 (some string) None & info [] ~docv:"PKG" ~doc:None) in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler.go_with_rpc_server ~common ~config (fun () ->
    let open Fiber.O in
    Pkg_common.check_pkg_management_enabled ()
    >>>
    let* _workspace = Memo.run (Workspace.workspace ()) in
    let lock_dir_path = Dune_rules.Lock_dir.default_source_path in
    let patches_dir = default_patches_dir in
    let action =
      match list_flag, commit_flag, remove_flag, pkg_arg with
      | true, None, None, None -> List
      | false, Some pkg, None, None -> Commit (Package_name.of_string pkg)
      | false, None, Some pkg, None -> Remove (Package_name.of_string pkg)
      | false, None, None, Some pkg -> Prepare (Package_name.of_string pkg)
      | false, None, None, None ->
        User_error.raise
          [ Pp.text "No action specified."
          ; Pp.text
              "Usage: dune pkg patch <PKG> | --list | --commit <PKG> | --remove <PKG>"
          ]
      | _ ->
        User_error.raise
          [ Pp.text "Conflicting options. Specify only one action at a time." ]
    in
    run ~lock_dir_path ~patches_dir action)
;;

let info =
  let doc = "Manage patches for duniverse packages" in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        "Create and manage patches for packages in the duniverse. Patches are stored in \
         the patches/ directory and applied automatically when running 'dune pkg fetch'."
    ; `S "ACTIONS"
    ; `P "$(b,dune pkg patch PKG) - Prepare a package for patching"
    ; `P "$(b,dune pkg patch --commit PKG) - Generate patch from local modifications"
    ; `P "$(b,dune pkg patch --remove PKG) - Remove a patch"
    ; `P "$(b,dune pkg patch --list) - List all patches and their status"
    ; `S "EXAMPLES"
    ; `P "Prepare fmt for patching:"
    ; `Pre "  dune pkg patch fmt"
    ; `P "Edit files in duniverse/fmt.0.9.0/, then commit the patch:"
    ; `Pre "  dune pkg patch --commit fmt"
    ; `P "List all patches:"
    ; `Pre "  dune pkg patch --list"
    ]
  in
  Cmd.info "patch" ~doc ~man
;;

let command = Cmd.v info term
