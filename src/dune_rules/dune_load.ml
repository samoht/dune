open Import
open Memo.O

(* Key for packages: (name, version option) to support multi-version vendoring *)
module Package_key = struct
  type t = Package.Name.t * Package_version.t option

  let compare (n1, v1) (n2, v2) =
    match Package.Name.compare n1 n2 with
    | Eq -> Option.compare Package_version.compare v1 v2
    | ord -> ord
  ;;

  let to_dyn (name, version) =
    Dyn.pair Package.Name.to_dyn (Dyn.option Package_version.to_dyn) (name, version)
  ;;
end

module Package_key_map = Map.Make (Package_key)

module Dune_file_db = struct
  type t = Dune_file.t Path.Source.Map.t

  let make all =
    Path.Source.Map.of_list_map_exn all ~f:(fun dune_file ->
      Dune_file.dir dune_file, dune_file)
  ;;

  let per_context dune_files =
    Per_context.create_by_name ~name:"dune-file-db" (fun ctx ->
      Memo.lazy_ (fun () -> dune_files ctx >>| make) |> Memo.Lazy.force)
  ;;
end

type t =
  { dune_files : Dune_file.t list Per_context.t
  ; packages : Package.t Package.Name.Map.t
  ; all_packages : Package.t Package_key_map.t
  ; projects : Dune_project.t list
  ; projects_by_root : Dune_project.t Path.Source.Map.t
  ; dune_file_by_dir : Dune_file_db.t Per_context.t
  ; mask : Only_packages.t
  ; vendored_packages : Package.Name.Set.t
  }

type status =
  [ `Vendored
  | `Regular
  ]

module Projects_and_dune_files =
  Monoid.Product
    (Monoid.Appendable_list (struct
      type t = status * Dune_project.t
    end))
    (Monoid.Appendable_list (struct
         type t = Path.Source.t * Dune_project.t * Source.Dune_file.t
       end))

module Source_tree_map_reduce =
  Source_tree.Make_map_reduce_with_progress (Memo) (Projects_and_dune_files)

let load () =
  let status dir =
    match Source_tree.Dir.status dir with
    | Vendored -> `Vendored
    | Normal | Data_only -> `Regular
  in
  let* projects, dune_files =
    let f dir : Projects_and_dune_files.t Memo.t =
      let path = Source_tree.Dir.path dir in
      let project = Source_tree.Dir.project dir in
      let projects =
        if Path.Source.equal path (Dune_project.root project)
        then Appendable_list.singleton (status dir, project)
        else Appendable_list.empty
      in
      let dune_files =
        match Source_tree.Dir.dune_file dir with
        | None -> Appendable_list.empty
        | Some d -> Appendable_list.singleton (path, project, d)
      in
      Memo.return (projects, dune_files)
    in
    Source_tree_map_reduce.map_reduce
      ~traverse:Source_dir_status.Set.all
      ~trace_event_name:"Dune load"
      ~f
  in
  let projects = Appendable_list.to_list_rev projects in
  let packages_by_name, all_packages_by_key, vendored_packages =
    List.fold_left
      projects
      ~init:(Package.Name.Map.empty, Package_key_map.empty, Package.Name.Set.empty)
      ~f:(fun (acc_by_name, acc_by_key, vendored) (status, (project : Dune_project.t)) ->
        let packages = Dune_project.including_hidden_packages project in
        let vendored =
          match status with
          | `Regular -> vendored
          | `Vendored ->
            Package.Name.Set.of_keys packages |> Package.Name.Set.union vendored
        in
        (* Add to key map (name, version) - always succeeds for unique name+version *)
        let acc_by_key =
          Package.Name.Map.fold packages ~init:acc_by_key ~f:(fun pkg acc ->
            let key = Package.name pkg, Package.version pkg in
            match Package_key_map.find acc key with
            | None -> Package_key_map.add_exn acc key pkg
            | Some existing ->
              User_error.raise
                [ Pp.textf
                    "The package %S (version %s) is defined more than once:"
                    (Package.Name.to_string (Package.name pkg))
                    (match Package.version pkg with
                     | None -> "<none>"
                     | Some v -> Package_version.to_string v)
                ; Pp.textf "- %s" (Loc.to_file_colon_line (Package.loc existing))
                ; Pp.textf "- %s" (Loc.to_file_colon_line (Package.loc pkg))
                ])
        in
        (* Add to name map - for backwards compatibility, keep first on conflict *)
        let acc_by_name =
          Package.Name.Map.union acc_by_name packages ~f:(fun name a b ->
            match status with
            | `Vendored ->
              (* Vendored packages with same name but different versions: keep first *)
              Some a
            | `Regular ->
              User_error.raise
                [ Pp.textf
                    "The package %S is defined more than once:"
                    (Package.Name.to_string name)
                ; Pp.textf "- %s" (Loc.to_file_colon_line (Package.loc a))
                ; Pp.textf "- %s" (Loc.to_file_colon_line (Package.loc b))
                ])
        in
        acc_by_name, acc_by_key, vendored)
  in
  let all_packages = all_packages_by_key in
  let mask = Only_packages.mask packages_by_name ~vendored:vendored_packages in
  let packages = Only_packages.filter_packages mask packages_by_name in
  let projects = List.rev_map projects ~f:snd in
  let dune_files =
    let without_ctx =
      Memo.lazy_ ~name:"dune-files-eval" (fun () ->
        let (_ : Package.Name.t Path.Source.Map.t) =
          match
            Package.Name.Map.values packages_by_name
            |> List.filter_map ~f:(fun pkg ->
              match Package.exclusive_dir pkg with
              | None -> None
              | Some d -> Some (d, pkg))
            |> Path.Source.Map.of_list_map ~f:(fun ((_loc, d), pkg) ->
              d, Package.name pkg)
          with
          | Ok s -> s
          | Error (dir, ((loc, _), p1), (_, p2)) ->
            let name p = Package.Name.to_string (Package.name p) in
            User_error.raise
              ~loc
              [ Pp.textf
                  "Directory %s cannot belong to package %s"
                  (Path.Source.to_string_maybe_quoted dir)
                  (name p1)
              ; Pp.textf "It already belongs to package %s" (name p2)
              ]
        in
        Dune_file.eval dune_files mask)
    in
    Per_context.create_by_name ~name:"dune-files" (fun ctx ->
      Memo.Lazy.create (fun () ->
        let* f = Memo.Lazy.force without_ctx in
        f ctx)
      |> Memo.Lazy.force)
    |> Staged.unstage
  in
  let dune_file_by_dir = Dune_file_db.per_context dune_files |> Staged.unstage in
  Memo.return
    { dune_files
    ; mask
    ; dune_file_by_dir
    ; packages
    ; all_packages
    ; projects
    ; projects_by_root =
        Path.Source.Map.of_list_map_exn projects ~f:(fun project ->
          Dune_project.root project, project)
    ; vendored_packages
    }
;;

let load =
  let memo = Memo.lazy_ ~name:"dune_load" load in
  fun () -> Memo.Lazy.force memo
;;

let find_project ~dir =
  let+ { projects_by_root; _ } = load () in
  Find_closest_source_dir.find_by_dir_exn projects_by_root ~dir
;;

let stanzas_in_dir dir =
  if Path.Build.is_root dir
  then Memo.return None
  else (
    match Install.Context.of_path dir with
    | None -> Memo.return None
    | Some ctx ->
      let dir = Path.Build.drop_build_context_exn dir in
      let* { dune_file_by_dir; _ } = load () in
      let+ map = dune_file_by_dir ctx in
      Path.Source.Map.find map dir)
;;

let mask () =
  let+ { mask; _ } = load () in
  mask
;;

let packages () =
  let+ { packages; _ } = load () in
  packages
;;

let dune_files context =
  let* t = load () in
  t.dune_files context
;;

let projects_by_root () =
  let+ t = load () in
  t.projects_by_root
;;

let projects () =
  let+ t = load () in
  t.projects
;;

let vendored_packages () =
  let+ t = load () in
  t.vendored_packages
;;

let all_packages () =
  let+ t = load () in
  t.all_packages
;;
