open Import
module Lock_dir = Dune_pkg.Lock_dir
module Source = Dune_pkg.Source
module Duniverse = Dune_pkg.Duniverse
module Rev_store = Dune_pkg.Rev_store
module OpamUrl = Dune_pkg.OpamUrl

(* Get the default lock dir path *)
let get_default_lock_dir_path () = Dune_rules.Lock_dir.default_source_path |> Path.source

let fetch_package ~rev_store pkg =
  let open Fiber.O in
  let { Lock_dir.Pkg_info.name; version; source; _ } = pkg.Lock_dir.Pkg.info in
  match source with
  | None ->
    (* No source means local package or pinned without URL *)
    Fiber.return ()
  | Some { Source.url; checksum } ->
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
      Fiber.return ())
    else (
      Console.print_user_message
        (User_message.make
           [ Pp.textf
               "Fetching %s.%s to %s"
               (Package_name.to_string name)
               (Dune_pkg.Package_version.to_string version)
               target_path
           ]);
      let loc, opam_url = url in
      let* result =
        let checksum = Option.map checksum ~f:snd in
        match OpamUrl.classify opam_url loc with
        | `Git ->
          Dune_pkg.Fetch.fetch_git
            rev_store
            ~target:(Path.build (Path.Build.of_string target_path))
            ~url
        | `Path _ | `Archive ->
          Dune_pkg.Fetch.fetch
            ~unpack:true
            ~checksum
            ~target:(Path.build (Path.Build.of_string target_path))
            ~url
      in
      match result with
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
          ])
;;

let fetch_duniverse ~lock_dir_path () =
  let open Fiber.O in
  (* Read the lock directory *)
  let lock_dir = Lock_dir.read_disk_exn (Path.source lock_dir_path) in
  let all_pkgs = Lock_dir.Packages.to_pkg_list lock_dir.packages in
  (* Filter to only duniverse packages that have a source URL *)
  let duniverse_pkgs =
    List.filter all_pkgs ~f:(fun pkg ->
      match Duniverse.classify pkg with
      | Duniverse.Duniverse -> Option.is_some pkg.info.source
      | Duniverse.Opam_sandbox -> false)
  in
  if List.is_empty duniverse_pkgs
  then (
    (* Check if there are duniverse packages without sources *)
    let duniverse_without_sources =
      List.filter all_pkgs ~f:(fun pkg ->
        match Duniverse.classify pkg with
        | Duniverse.Duniverse -> Option.is_none pkg.info.source
        | Duniverse.Opam_sandbox -> false)
    in
    if not (List.is_empty duniverse_without_sources)
    then
      Console.print_user_message
        (User_message.make
           [ Pp.textf
               "%d duniverse package(s) have no source URL (likely local packages)."
               (List.length duniverse_without_sources)
           ])
    else
      Console.print_user_message
        (User_message.make
           [ Pp.text "No duniverse packages to fetch (all packages use opam sandbox)." ]);
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
    (* Fetch each package *)
    let+ () =
      Fiber.sequential_iter duniverse_pkgs ~f:(fun pkg -> fetch_package ~rev_store pkg)
    in
    Console.print_user_message
      (User_message.make
         [ Pp.textf
             "Fetched %d duniverse package(s) to %s/"
             (List.length duniverse_pkgs)
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
    let* workspace = Memo.run (Workspace.workspace ()) in
    let lock_dirs =
      Pkg_common.Lock_dirs_arg.lock_dirs_of_workspace lock_dirs_arg workspace
    in
    match lock_dirs with
    | [] ->
      User_error.raise [ Pp.text "No lock directories found. Run 'dune pkg lock' first." ]
    | lock_dir_path :: _ -> fetch_duniverse ~lock_dir_path ())
;;

let info =
  let doc = "Fetch duniverse package sources" in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        "Fetches the source code for packages that will be built in the duniverse \
         directory. These are packages that use dune as their build system and will be \
         built alongside your project code, enabling full editor tooling support."
    ; `P
        "Packages are downloaded to duniverse/<name>.<version>/ and can be edited \
         directly. Changes will be reflected in your project builds."
    ; `S "EXAMPLES"
    ; `Pre "  dune pkg fetch"
    ; `Pre "  dune pkg fetch dune.lock"
    ]
  in
  Cmd.info "fetch" ~doc ~man
;;

let command = Cmd.v info term
