open Import
open Pkg_common
module Package_universe = Dune_pkg.Package_universe
module Lock_dir = Dune_pkg.Lock
module Lock_pkg = Dune_pkg.Lock_pkg
module Opam_repo = Dune_pkg.Opam_repo
module Package_version = Dune_pkg.Package_version
module Opam_solver = Dune_pkg.Opam_solver

let info =
  let doc = "Validate that a lockdir contains a solution for local packages" in
  let man = [ `S "DESCRIPTION"; `P doc ] in
  Cmd.info "validate-lockdir" ~doc ~man
;;

(* CR-someday alizter: The logic here is a little more complicated than it needs
   to be and can be simplified. *)

let enumerate_lock_dir_paths ~lock_dirs () =
  let open Memo.O in
  let+ per_contexts =
    Workspace.workspace () >>| Pkg_common.Lock_dirs_arg.lock_dirs_of_workspace lock_dirs
  in
  List.filter per_contexts ~f:(fun lock_dir_path ->
    Path.exists (Path.source lock_dir_path))
;;

module Local_package = Dune_pkg.Local_package

let read_and_validate_lock_dir ~local_packages lock_dir_path =
  let open Fiber.O in
  let path = Path.source lock_dir_path in
  let* solver_env = solver_env_from_system_and_context ~lock_dir_path:path in
  (* Convert local_packages for Lock_pkg.read_disk *)
  let local_packages_for_solver =
    Package_name.Map.values local_packages |> List.map ~f:Local_package.for_solver
  in
  let+ result =
    Fiber.map_reduce_errors
      (module Monoid.Unit)
      ~on_error:(fun _ -> Fiber.return ())
      (fun () ->
         Lock_pkg.read_disk ~solver_env ~local_packages:local_packages_for_solver path)
  in
  match result with
  | Ok lock_dir ->
    (match Package_universe.create ~platform:solver_env local_packages lock_dir with
     | Ok _ -> None
     | Error e -> Some (path, `Lock_dir_out_of_sync e))
  | Error () ->
    (* Try to get the actual error by re-reading *)
    (try
       let _ = Lock_pkg.read_disk_minimal ~local_packages path in
       (* If minimal read succeeds but full read failed, likely a derivation error *)
       Some
         ( path
         , `Lock_dir_out_of_sync
             (User_message.make
                [ Pp.text "Failed to derive lock from opam repositories" ]) )
     with
     | User_error.E e -> Some (path, `Parse_error e))
;;

let validate_lock_dirs ~lock_dirs () =
  let open Fiber.O in
  let* local_packages = Memo.run Pkg_common.find_local_packages in
  let* lock_dir_paths = Memo.run (enumerate_lock_dir_paths ~lock_dirs ()) in
  if List.is_empty lock_dir_paths
  then
    let+ () = Fiber.return () in
    Console.print [ Pp.text "No lockdirs to validate." ]
  else
    let+ errors =
      Fiber.parallel_map lock_dir_paths ~f:(fun lock_dir_path ->
        read_and_validate_lock_dir ~local_packages lock_dir_path)
      >>| List.filter_opt
    in
    match errors with
    | [] -> ()
    | errors_by_path ->
      List.iter errors_by_path ~f:(fun (path, error) ->
        match error with
        | `Parse_error error ->
          User_message.prerr
            (User_message.make
               [ Pp.textf "Failed to parse %s:" (Path.to_string_maybe_quoted path)
               ; User_message.pp error
               ])
        | `Lock_dir_out_of_sync error -> User_message.prerr error);
      User_error.raise
        [ Pp.text "Lock file validation failed:"
        ; Pp.enumerate errors_by_path ~f:(fun (path, _) -> Pp.text (Path.to_string path))
        ]
;;

let term =
  let+ builder = Common.Builder.term
  and+ lock_dirs = Pkg_common.Lock_dirs_arg.term in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler.go_with_rpc_server ~common ~config (fun () ->
    let open Fiber.O in
    Pkg_common.check_pkg_management_enabled () >>> validate_lock_dirs ~lock_dirs ())
;;

let command = Cmd.v info term
