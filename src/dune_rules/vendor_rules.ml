open Import

let extract_public_name_from_sexp sexp =
  let open Dune_sexp.Ast in
  let atom_to_string = function
    | Atom (_, a) -> Some (Dune_sexp.Atom.to_string a)
    | _ -> None
  in
  match sexp with
  | List (_, Atom (_, lib_atom) :: fields)
    when String.equal (Dune_sexp.Atom.to_string lib_atom) "library" ->
    List.find_map fields ~f:(fun field ->
      match field with
      | List (_, [ Atom (_, key); value ])
        when String.equal (Dune_sexp.Atom.to_string key) "public_name" ->
        atom_to_string value
      | _ -> None)
  | _ -> None
;;

let scan_dune_file path =
  let full_path = Path.source path in
  if Path.Untracked.exists full_path
  then (
    let contents = Io.read_file ~binary:true full_path in
    match
      Dune_sexp.Parser.parse_string
        ~fname:(Path.Source.to_string path)
        ~mode:Many
        contents
    with
    | exception _ -> []
    | sexps -> List.filter_map sexps ~f:extract_public_name_from_sexp)
  else []
;;

let rec scan_dir_for_dune_files dir =
  let full_path = Path.source dir in
  if not (Path.Untracked.exists full_path)
  then []
  else (
    let dune_file = Path.Source.relative dir "dune" in
    let libs = scan_dune_file dune_file in
    match Path.Untracked.readdir_unsorted_with_kinds full_path with
    | Error _ -> libs
    | Ok entries ->
      let subdirs =
        List.filter_map entries ~f:(fun (entry, kind) ->
          match kind with
          | Unix.S_DIR -> Some (Path.Source.relative dir entry)
          | _ -> None)
      in
      let subdir_libs = List.concat_map subdirs ~f:scan_dir_for_dune_files in
      libs @ subdir_libs)
;;

let scan_public_libraries dir = scan_dir_for_dune_files dir

let scan_meta_libraries dir ~pkg_name =
  let meta_file_in_pkg = Path.Source.relative dir (pkg_name ^ "/META") in
  let meta_file = Path.Source.relative dir "META" in
  if
    Path.Untracked.exists (Path.source meta_file_in_pkg)
    || Path.Untracked.exists (Path.source meta_file)
  then [ pkg_name ]
  else []
;;

let scan_opam_libraries dir =
  match Path.Untracked.readdir_unsorted (Path.source dir) with
  | Error _ -> []
  | Ok entries ->
    List.filter_map entries ~f:(fun entry ->
      if String.is_suffix entry ~suffix:".opam"
      then Some (Filename.remove_extension entry)
      else None)
;;

let scan_libraries dir ~pkg_name =
  match scan_public_libraries dir with
  | [] ->
    (match scan_meta_libraries dir ~pkg_name with
     | [] -> scan_opam_libraries dir
     | libs -> libs)
  | libs -> libs
;;

let read_project_name dir =
  let dune_project = Path.Source.relative dir "dune-project" in
  let full_path = Path.source dune_project in
  if Path.Untracked.exists full_path
  then (
    let contents = Io.read_file ~binary:true full_path in
    match
      Dune_sexp.Parser.parse_string
        ~fname:(Path.Source.to_string dune_project)
        ~mode:Many
        contents
    with
    | exception _ -> None
    | sexps ->
      List.find_map sexps ~f:(fun sexp ->
        let open Dune_sexp.Ast in
        match sexp with
        | List (_, Atom (_, name_atom) :: Atom (_, value) :: _)
          when String.equal (Dune_sexp.Atom.to_string name_atom) "name" ->
          Some (Dune_sexp.Atom.to_string value)
        | _ -> None))
  else None
;;

let lib_cache_file =
  lazy
    (let pkg_dir = Path.build (Path.Build.relative Path.Build.root "_build/.pkg") in
     Path.relative pkg_dir "lib-cache")
;;

let lib_cache : string String.Table.t option ref = ref None

let load_lib_cache () =
  let cache_file = Lazy.force lib_cache_file in
  if Path.Untracked.exists cache_file
  then (
    let lines = Io.lines_of_file cache_file in
    let tbl = String.Table.create 256 in
    List.iter lines ~f:(fun line ->
      match String.lsplit2 line ~on:':' with
      | Some (lib, dir) -> String.Table.set tbl lib dir
      | None -> ());
    Some tbl)
  else None
;;

let find_dir_for_library lib_name =
  let cache =
    match !lib_cache with
    | Some c -> Some c
    | None ->
      let c = load_lib_cache () in
      lib_cache := c;
      c
  in
  match cache with
  | None -> None
  | Some tbl -> String.Table.find tbl lib_name
;;

let invalidate_lib_cache () = lib_cache := None
