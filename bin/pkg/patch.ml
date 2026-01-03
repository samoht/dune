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

(* Common setup for patch commands *)
let with_lock_dir f =
  let open Fiber.O in
  Pkg_common.check_pkg_management_enabled ()
  >>>
  let* _workspace = Memo.run (Workspace.workspace ()) in
  let lock_dir_path = Dune_rules.Lock_dir.default_source_path in
  let patches_dir = default_patches_dir in
  f ~lock_dir_path ~patches_dir
;;

(* ---- List subcommand ---- *)

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

module List_cmd = struct
  let term =
    let+ builder = Common.Builder.term in
    let builder = Common.Builder.forbid_builds builder in
    let common, config = Common.init builder in
    Scheduler.go_with_rpc_server ~common ~config (fun () ->
      with_lock_dir (fun ~lock_dir_path ~patches_dir ->
        list_patches ~lock_dir_path ~patches_dir))
  ;;

  let info =
    let doc = "List all patches and their status" in
    let man =
      [ `S "DESCRIPTION"
      ; `P "Lists all patch files in the patches/ directory and shows their status."
      ; `P "Status can be:"
      ; `P "  $(b,(current)) - patch matches locked version"
      ; `P "  $(b,(stale: ...)) - patch is for different version"
      ; `P "  $(b,(package not in lock)) - package not in lock directory"
      ]
    in
    Cmd.info "list" ~doc ~man
  ;;

  let command = Cmd.v info term
end

(* ---- Diff subcommand (show what would be in a patch) ---- *)

let show_diff ~patches_dir name version =
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
  let git_dir = Path.relative pkg_path ".git" in
  if Path.exists git_dir
  then (
    (* Has git - show diff *)
    Console.print_user_message
      (User_message.make
         [ Pp.textf
             "Changes in %s.%s:"
             (Package_name.to_string name)
             (Package_version.to_string version)
         ; Pp.nop
         ; Pp.textf "Run: git -C %s diff" (Path.to_string pkg_path)
         ]);
    Fiber.return ())
  else (
    let patch_file = patch_path ~patches_dir name version in
    Console.print_user_message
      (User_message.make
         [ Pp.textf
             "Package %s is not a git repository."
             (Path.Source.to_string duniverse_pkg_dir)
         ; Pp.nop
         ; Pp.text "To track changes, initialize with git first:"
         ; Pp.textf
             "  cd %s && git init && git add -A && git commit -m 'original'"
             (Path.Source.to_string duniverse_pkg_dir)
         ; Pp.nop
         ; Pp.text "Then make your changes and run:"
         ; Pp.textf
             "  git -C %s diff > %s"
             (Path.to_string pkg_path)
             (Path.Source.to_string patch_file)
         ]);
    Fiber.return ())
;;

module Diff_cmd = struct
  let term =
    let+ builder = Common.Builder.term
    and+ pkg_name =
      Arg.(required & pos 0 (some string) None & info [] ~docv:"PKG" ~doc:None)
    in
    let builder = Common.Builder.forbid_builds builder in
    let common, config = Common.init builder in
    Scheduler.go_with_rpc_server ~common ~config (fun () ->
      with_lock_dir (fun ~lock_dir_path ~patches_dir ->
        let lock_dir = Lock_dir.read_disk_exn (Path.source lock_dir_path) in
        let name = Package_name.of_string pkg_name in
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
        show_diff ~patches_dir name pkg.info.version))
  ;;

  let info =
    let doc = "Show local changes to a package" in
    let man =
      [ `S "DESCRIPTION"
      ; `P "Shows instructions for viewing local modifications to a duniverse package."
      ; `P
          "Edit files directly in duniverse/<pkg>.<version>/, then use this command to \
           see your changes."
      ; `S "EXAMPLES"
      ; `Pre "  dune pkg patch diff fmt"
      ]
    in
    Cmd.info "diff" ~doc ~man
  ;;

  let command = Cmd.v info term
end

(* ---- Commit subcommand ---- *)

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

module Commit_cmd = struct
  let term =
    let+ builder = Common.Builder.term
    and+ pkg_name =
      Arg.(required & pos 0 (some string) None & info [] ~docv:"PKG" ~doc:None)
    in
    let builder = Common.Builder.forbid_builds builder in
    let common, config = Common.init builder in
    Scheduler.go_with_rpc_server ~common ~config (fun () ->
      with_lock_dir (fun ~lock_dir_path ~patches_dir ->
        let lock_dir = Lock_dir.read_disk_exn (Path.source lock_dir_path) in
        let name = Package_name.of_string pkg_name in
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
        commit_patch ~patches_dir name pkg.info.version))
  ;;

  let info =
    let doc = "Generate patch from local changes" in
    let man =
      [ `S "DESCRIPTION"
      ; `P
          "Shows instructions for generating a patch file from local modifications to a \
           duniverse package."
      ; `P
          "The generated patch will be stored in patches/<name>@<version>.patch and will \
           be automatically applied on 'dune pkg fetch'."
      ; `S "EXAMPLES"
      ; `Pre "  dune pkg patch commit fmt"
      ]
    in
    Cmd.info "commit" ~doc ~man
  ;;

  let command = Cmd.v info term
end

(* ---- Remove subcommand ---- *)

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

module Remove_cmd = struct
  let term =
    let+ builder = Common.Builder.term
    and+ pkg_name =
      Arg.(required & pos 0 (some string) None & info [] ~docv:"PKG" ~doc:None)
    in
    let builder = Common.Builder.forbid_builds builder in
    let common, config = Common.init builder in
    Scheduler.go_with_rpc_server ~common ~config (fun () ->
      with_lock_dir (fun ~lock_dir_path ~patches_dir ->
        let lock_dir = Lock_dir.read_disk_exn (Path.source lock_dir_path) in
        let name = Package_name.of_string pkg_name in
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
        remove_patch ~patches_dir name pkg.info.version))
  ;;

  let info =
    let doc = "Remove a patch" in
    let man =
      [ `S "DESCRIPTION"
      ; `P "Removes the patch file for the specified package."
      ; `P "After removal, run 'dune pkg fetch' to restore the original package source."
      ; `S "EXAMPLES"
      ; `Pre "  dune pkg patch remove fmt"
      ]
    in
    Cmd.info "remove" ~doc ~man
  ;;

  let command = Cmd.v info term
end

(* ---- Command group ---- *)

let info =
  let doc = "Manage patches for duniverse packages" in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        "Create and manage patches for packages in the duniverse. Patches are stored in \
         the patches/ directory and applied automatically when running 'dune pkg fetch'."
    ; `S "COMMANDS"
    ; `P "$(b,dune pkg patch list) - List all patches and their status"
    ; `P "$(b,dune pkg patch diff PKG) - Show local changes to a package"
    ; `P "$(b,dune pkg patch commit PKG) - Generate patch from local modifications"
    ; `P "$(b,dune pkg patch remove PKG) - Remove a patch"
    ; `S "WORKFLOW"
    ; `P "1. Edit files directly in duniverse/fmt.0.9.0/"
    ; `P "2. Run $(b,dune pkg patch diff fmt) to view your changes"
    ; `P "3. Run $(b,dune pkg patch commit fmt) to generate the patch"
    ; `P "4. The patch is applied automatically on $(b,dune pkg fetch)"
    ]
  in
  Cmd.info "patch" ~doc ~man
;;

let subcommands =
  [ List_cmd.command; Diff_cmd.command; Commit_cmd.command; Remove_cmd.command ]
;;

let command = Cmd.group info subcommands
