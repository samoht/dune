open Import
open Pkg_common
module Lock_dir = Dune_pkg.Lock
module Pkg = Dune_pkg.Pkg

(* Build dependency graph from lock directory *)
module Dep_graph = struct
  type t =
    { packages : Pkg.t Package_name.Map.t
    ; reverse_deps : Package_name.Set.t Package_name.Map.t
    }

  let of_lock_dir (lock_dir : Lock_dir.t) ~platform =
    let packages =
      Lock_dir.Packages.pkgs_on_platform_by_name lock_dir.packages ~platform
    in
    (* Build reverse dependency map (what depends on each package) *)
    let reverse_deps =
      Package_name.Map.fold packages ~init:Package_name.Map.empty ~f:(fun pkg acc ->
        let deps =
          Pkg.Conditional_choice.choose_for_platform pkg.depends ~platform
          |> Option.value ~default:[]
        in
        List.fold_left deps ~init:acc ~f:(fun acc (dep : Pkg.Dependency.t) ->
          Package_name.Map.update acc dep.name ~f:(function
            | None -> Some (Package_name.Set.singleton pkg.info.name)
            | Some set -> Some (Package_name.Set.add set pkg.info.name))))
    in
    { packages; reverse_deps }
  ;;

  let deps_of _t (pkg : Pkg.t) ~platform =
    Pkg.Conditional_choice.choose_for_platform pkg.depends ~platform
    |> Option.value ~default:[]
    |> List.map ~f:(fun (d : Pkg.Dependency.t) -> d.name)
  ;;

  let dependents_of t name =
    Package_name.Map.find t.reverse_deps name
    |> Option.value ~default:Package_name.Set.empty
  ;;
end

(* Print flat list of dependencies *)
let print_flat ~graph =
  let sorted_pkgs =
    Package_name.Map.values graph.Dep_graph.packages
    |> List.sort ~compare:(fun (a : Pkg.t) (b : Pkg.t) ->
      Package_name.compare a.info.name b.info.name)
  in
  let lines =
    List.map sorted_pkgs ~f:(fun (pkg : Pkg.t) ->
      Pp.textf
        "%s.%s"
        (Package_name.to_string pkg.info.name)
        (Dune_pkg.Package_version.to_string pkg.info.version))
  in
  Console.print [ Pp.concat_map ~sep:Pp.newline ~f:Fun.id lines ]
;;

(* Print tree view of dependencies *)
let print_tree ~graph ~platform =
  let rec print_deps ~visited ~indent name =
    if Package_name.Set.mem visited name
    then [ Pp.textf "%s%s ..." indent (Package_name.to_string name) ]
    else (
      match Package_name.Map.find graph.Dep_graph.packages name with
      | None -> []
      | Some pkg ->
        let visited = Package_name.Set.add visited name in
        let header =
          Pp.textf
            "%s%s.%s"
            indent
            (Package_name.to_string pkg.info.name)
            (Dune_pkg.Package_version.to_string pkg.info.version)
        in
        let deps = Dep_graph.deps_of graph pkg ~platform in
        let children =
          List.concat_map deps ~f:(print_deps ~visited ~indent:(indent ^ "  "))
        in
        header :: children)
  in
  (* Find root packages (packages that nothing depends on) *)
  let roots =
    Package_name.Map.fold graph.packages ~init:[] ~f:(fun pkg acc ->
      let dependents = Dep_graph.dependents_of graph pkg.info.name in
      if Package_name.Set.is_empty dependents then pkg.info.name :: acc else acc)
  in
  let trees =
    List.concat_map (List.sort roots ~compare:Package_name.compare) ~f:(fun name ->
      print_deps ~visited:Package_name.Set.empty ~indent:"" name)
  in
  Console.print [ Pp.concat_map ~sep:Pp.newline ~f:Fun.id trees ]
;;

(* Print why a package is needed *)
let print_why ~graph ~platform:_ ~target =
  match Package_name.Map.find graph.Dep_graph.packages target with
  | None ->
    Console.print
      [ Pp.textf
          "Package %s is not in the lock directory."
          (Package_name.to_string target)
      ]
  | Some _ ->
    (* Find all paths to roots *)
    let rec find_paths_to_roots ~visited name =
      if Package_name.Set.mem visited name
      then []
      else (
        let visited = Package_name.Set.add visited name in
        let dependents = Dep_graph.dependents_of graph name in
        if Package_name.Set.is_empty dependents
        then [ [ name ] ] (* This is a root *)
        else
          Package_name.Set.to_list dependents
          |> List.concat_map ~f:(fun parent ->
            find_paths_to_roots ~visited parent |> List.map ~f:(fun path -> name :: path)))
    in
    let paths = find_paths_to_roots ~visited:Package_name.Set.empty target in
    (match paths with
     | [] ->
       Console.print
         [ Pp.textf
             "%s is a root package (nothing depends on it)."
             (Package_name.to_string target)
         ]
     | paths ->
       let unique_paths =
         List.sort_uniq paths ~compare:(List.compare ~compare:Package_name.compare)
       in
       let lines =
         List.map unique_paths ~f:(fun path ->
           let path_str =
             List.rev path
             |> List.map ~f:Package_name.to_string
             |> String.concat ~sep:" -> "
           in
           Pp.verbatim path_str)
       in
       Console.print
         [ Pp.textf "%s is needed by:" (Package_name.to_string target)
         ; Pp.nop
         ; Pp.concat_map ~sep:Pp.newline ~f:Fun.id lines
         ])
;;

let run_deps ~lock_dirs_arg ~tree ~why () =
  let open Fiber.O in
  let* workspace = Memo.run (Workspace.workspace ()) in
  let lock_dirs =
    Pkg_common.Lock_dirs_arg.lock_dirs_of_workspace lock_dirs_arg workspace
  in
  let+ () =
    Fiber.sequential_iter lock_dirs ~f:(fun lock_dir_path ->
      let lock_dir_path = Path.source lock_dir_path in
      let* platform = solver_env_from_system_and_context ~lock_dir_path in
      let+ lock_dir = Dune_pkg.Lock_pkg.read_disk ~solver_env:platform lock_dir_path in
      let graph = Dep_graph.of_lock_dir lock_dir ~platform in
      match why with
      | Some target ->
        let target = Package_name.of_string target in
        print_why ~graph ~platform ~target
      | None -> if tree then print_tree ~graph ~platform else print_flat ~graph)
  in
  ()
;;

let term =
  let+ builder = Common.Builder.term
  and+ tree =
    Arg.(value & flag & info [ "tree" ] ~doc:(Some "Show dependencies as a tree"))
  and+ why =
    Arg.(
      value
      & opt (some string) None
      & info [ "why" ] ~docv:"PACKAGE" ~doc:(Some "Show why a package is needed"))
  and+ lock_dirs_arg = Pkg_common.Lock_dirs_arg.term in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler.go_with_rpc_server ~common ~config (fun () ->
    let open Fiber.O in
    Pkg_common.check_pkg_management_enabled () >>> run_deps ~lock_dirs_arg ~tree ~why ())
;;

let info =
  let doc = "Show package dependencies" in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        "Display dependencies from the lock directory. By default, shows a flat list of \
         all packages. Use --tree for a hierarchical view or --why to trace why a \
         specific package is included."
    ; `S "EXAMPLES"
    ; `Pre "  $ dune pkg deps"
    ; `Pre "  cmdliner.1.3.0"
    ; `Pre "  fmt.0.9.0"
    ; `Pre "  ..."
    ; `Noblank
    ; `Pre "  "
    ; `Pre "  $ dune pkg deps --tree"
    ; `Pre "  fmt.0.9.0"
    ; `Pre "    cmdliner.1.3.0"
    ; `Pre "  ..."
    ; `Noblank
    ; `Pre "  "
    ; `Pre "  $ dune pkg deps --why cmdliner"
    ; `Pre "  cmdliner is needed by:"
    ; `Pre "  fmt -> cmdliner"
    ]
  in
  Cmd.info "deps" ~doc ~man
;;

let command = Cmd.v info term
