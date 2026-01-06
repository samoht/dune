open Import

type package_target =
  | Duniverse
  | Opam_sandbox

let package_target_to_dyn = function
  | Duniverse -> Dyn.variant "Duniverse" []
  | Opam_sandbox -> Dyn.variant "Opam_sandbox" []
;;

let marker_filename = "dune"
let marker_dirname = "duniverse"
let duniverse_dir = Path.Source.of_string marker_dirname

let package_dir name version =
  let dirname =
    sprintf "%s.%s" (Package_name.to_string name) (Package_version.to_string version)
  in
  Path.Source.relative duniverse_dir dirname
;;

(* Source comparison that ignores location information *)
module Source_key = struct
  type t =
    { url : string
    ; checksum : Checksum.t option
    }

  let of_source (source : Source.t) =
    let _, url = source.url in
    let checksum = Option.map source.checksum ~f:snd in
    { url = OpamUrl.to_string url; checksum }
  ;;

  let compare a b =
    match String.compare a.url b.url with
    | (Lt | Gt) as x -> x
    | Eq ->
      Option.compare
        (fun x y -> String.compare (Checksum.to_string x) (Checksum.to_string y))
        a.checksum
        b.checksum
  ;;

  let to_dyn { url; checksum } =
    Dyn.record [ "url", Dyn.string url; "checksum", Dyn.option Checksum.to_dyn checksum ]
  ;;
end

module Source_key_map = Map.Make (Source_key)

(* A group of packages that share the same source *)
type source_group =
  { packages : (Package_name.t * Package_version.t) list
  ; source : Source.t
  ; primary_name : Package_name.t (* Shortest name, for directory naming *)
  ; primary_version : Package_version.t
  }

let group_by_source pkgs =
  (* Group packages by their source, keeping track of all packages per source *)
  let groups =
    List.fold_left pkgs ~init:Source_key_map.empty ~f:(fun acc pkg ->
      let { Lock.Pkg_info.name; version; source; _ } = pkg.Lock.Pkg.info in
      match source with
      | None -> acc
      | Some source ->
        let key = Source_key.of_source source in
        let entry = name, version, source, pkg in
        Source_key_map.update acc key ~f:(function
          | None -> Some [ entry ]
          | Some entries -> Some (entry :: entries)))
  in
  (* Convert to source_group list, picking shortest name as primary *)
  Source_key_map.values groups
  |> List.map ~f:(fun entries ->
    (* Sort by name length (shortest first), then alphabetically for ties *)
    let sorted =
      List.sort entries ~compare:(fun (n1, _, _, _) (n2, _, _, _) ->
        let s1 = Package_name.to_string n1 in
        let s2 = Package_name.to_string n2 in
        match Int.compare (String.length s1) (String.length s2) with
        | (Lt | Gt) as x -> x
        | Eq -> Package_name.compare n1 n2)
    in
    match sorted with
    | [] -> assert false
    | (primary_name, primary_version, source, _) :: _ ->
      let packages = List.map sorted ~f:(fun (n, v, _, _) -> n, v) in
      { packages; source; primary_name; primary_version })
;;

(* Get directory path for a source group - uses primary package name *)
let source_group_dir group = package_dir group.primary_name group.primary_version

(* Get all packages in a source group *)
let source_group_packages group = group.packages

(* Check if a Slang expression is a literal "dune" *)
let is_dune_command = function
  | Slang.Literal sw ->
    (match String_with_vars.text_only sw with
     | Some "dune" -> true
     | _ -> false)
  | _ -> false
;;

(* Recursively check if an action uses dune as the build command *)
let rec action_uses_dune (action : Action.t) =
  match action with
  | Run (cmd :: _) -> is_dune_command cmd
  | Progn actions | Concurrent actions -> List.exists actions ~f:action_uses_dune
  | Chdir (_, action)
  | Setenv (_, _, action)
  | Redirect_out (_, _, _, action)
  | Redirect_in (_, _, action)
  | Ignore (_, action)
  | No_infer action
  | With_accepted_exit_codes (_, action)
  | Withenv (_, action)
  | When (_, action) -> action_uses_dune action
  | Pipe (_, actions) -> List.exists actions ~f:action_uses_dune
  | Run []
  | Dynamic_run _
  | Echo _
  | Cat _
  | Copy _
  | Symlink _
  | Copy_and_add_line_directive _
  | System _
  | Bash _
  | Write_file _
  | Mkdir _
  | Diff _
  | Cram _
  | Patch _
  | Substitute _
  | Format_dune_file _ -> false
;;

let classify (pkg : Lock.Pkg.t) =
  (* A package goes to duniverse if it uses dune as its build system.
     We check if the build command is [Dune] or contains a dune action for any platform. *)
  let uses_dune =
    Lock.Conditional_choice.exists pkg.build_command ~f:(function
      | Lock.Build_command.Dune -> true
      | Lock.Build_command.Action action -> action_uses_dune action)
  in
  if uses_dune then Duniverse else Opam_sandbox
;;

let classify_all pkgs = Package_name.Map.map pkgs ~f:classify

(* Extract patch file paths from an action, in order of application *)
let rec extract_patches_from_action (action : Action.t) =
  match action with
  | Patch sw -> [ sw ]
  | Progn actions -> List.concat_map actions ~f:extract_patches_from_action
  | Concurrent actions -> List.concat_map actions ~f:extract_patches_from_action
  | Chdir (_, action)
  | Setenv (_, _, action)
  | Redirect_out (_, _, _, action)
  | Redirect_in (_, _, action)
  | Ignore (_, action)
  | No_infer action
  | With_accepted_exit_codes (_, action)
  | Withenv (_, action)
  | When (_, action) -> extract_patches_from_action action
  | Pipe (_, actions) -> List.concat_map actions ~f:extract_patches_from_action
  | Run _
  | Dynamic_run _
  | Echo _
  | Cat _
  | Copy _
  | Symlink _
  | Copy_and_add_line_directive _
  | System _
  | Bash _
  | Write_file _
  | Mkdir _
  | Diff _
  | Cram _
  | Substitute _
  | Format_dune_file _ -> []
;;

(* Get patches from a package's build command for the current platform *)
let get_patches (pkg : Lock.Pkg.t) ~platform =
  match Lock.Conditional_choice.choose_for_platform pkg.build_command ~platform with
  | None -> []
  | Some Lock.Build_command.Dune -> []
  | Some (Lock.Build_command.Action action) -> extract_patches_from_action action
;;

(* Extract public_name from a library stanza sexp *)
let extract_public_name_from_sexp sexp =
  let open Dune_sexp.Ast in
  let atom_to_string = function
    | Atom (_, a) -> Some (Dune_sexp.Atom.to_string a)
    | Quoted_string (_, s) -> Some s
    | _ -> None
  in
  match sexp with
  | List (_, Atom (_, lib_atom) :: fields)
    when String.equal (Dune_sexp.Atom.to_string lib_atom) "library" ->
    List.find_map fields ~f:(function
      | List (_, [ Atom (_, pn_atom); name_sexp ])
        when String.equal (Dune_sexp.Atom.to_string pn_atom) "public_name" ->
        atom_to_string name_sexp
      | _ -> None)
  | _ -> None
;;

(* Find dune files in a directory and immediate subdirectories only.
   We don't recurse deeply to avoid picking up test fixtures and examples. *)
let find_dune_files dir =
  let path = Path.source dir in
  if not (Path.exists path)
  then []
  else (
    match Path.readdir_unsorted_with_kinds path with
    | Error _ -> []
    | Ok entries ->
      let dune_file = Path.Source.relative dir "dune" in
      let current = if Path.exists (Path.source dune_file) then [ dune_file ] else [] in
      (* Only check immediate subdirectories (src/, lib/, etc.), not deep recursion *)
      let subdir_dune_files =
        List.filter_map entries ~f:(fun (name, kind) ->
          match kind with
          | Unix.S_DIR when not (String.is_prefix name ~prefix:".") ->
            let subdir = Path.Source.relative dir name in
            let subdir_dune = Path.Source.relative subdir "dune" in
            if Path.exists (Path.source subdir_dune) then Some subdir_dune else None
          | _ -> None)
      in
      current @ subdir_dune_files)
;;

(* Check if a string is a valid library name (may include dots for sub-libraries) *)
let is_valid_lib_name s =
  if String.is_empty s
  then false
  else (
    (* Must start with a letter or underscore *)
    match s.[0] with
    | 'A' .. 'Z' | 'a' .. 'z' | '_' ->
      (* Rest can be alphanumeric, underscore, hyphen, or dot (for sub-libs) *)
      String.for_all s ~f:(function
        | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' | '.' -> true
        | _ -> false)
    | _ -> false)
;;

(* Scan a vendored directory for public library names *)
let scan_public_libraries dir =
  let dune_files = find_dune_files dir in
  List.concat_map dune_files ~f:(fun dune_file ->
    let path = Path.source dune_file in
    match Io.read_file path with
    | exception _ -> []
    | contents ->
      (match
         Dune_sexp.Parser.parse_string ~fname:(Path.to_string path) ~mode:Many contents
       with
       | exception _ -> []
       | sexps ->
         List.filter_map sexps ~f:extract_public_name_from_sexp
         |> List.filter ~f:is_valid_lib_name))
;;

(* Extract project name from dune-project file *)
let extract_project_name_from_sexp sexp =
  let open Dune_sexp.Ast in
  let atom_to_string = function
    | Atom (_, a) -> Some (Dune_sexp.Atom.to_string a)
    | Quoted_string (_, s) -> Some s
    | _ -> None
  in
  match sexp with
  | List (_, Atom (_, name_atom) :: name_sexp :: _)
    when String.equal (Dune_sexp.Atom.to_string name_atom) "name" ->
    atom_to_string name_sexp
  | _ -> None
;;

(* Read the project name from a dune-project file in the given directory.
   Returns None if the file doesn't exist or doesn't have a name. *)
let read_project_name dir =
  let dune_project_path = Path.Source.relative dir "dune-project" in
  let path = Path.source dune_project_path in
  if not (Path.exists path)
  then None
  else (
    match Io.read_file path with
    | exception _ -> None
    | contents ->
      (match
         Dune_sexp.Parser.parse_string ~fname:(Path.to_string path) ~mode:Many contents
       with
       | exception _ -> None
       | sexps -> List.find_map sexps ~f:extract_project_name_from_sexp))
;;

module Meta = Dune_findlib.Findlib.Meta

(* Extract all library names from a META file, including sub-packages *)
let rec collect_lib_names_from_meta ~prefix (meta : Meta.Simplified.t) =
  let name =
    match meta.name with
    | Some n -> Lib_name.to_string n
    | None -> prefix
  in
  let full_name =
    if String.is_empty prefix
    then name
    else if String.is_empty name
    then prefix
    else prefix ^ "." ^ name
  in
  let self = if String.is_empty full_name then [] else [ full_name ] in
  let subs =
    List.concat_map meta.subs ~f:(collect_lib_names_from_meta ~prefix:full_name)
  in
  self @ subs
;;

(* Scan META file for library names - checks both pkg/META and META *)
let scan_meta_libraries dir ~pkg_name =
  let try_path path =
    if not (Path.exists (Path.source path))
    then None
    else (
      match Io.read_file (Path.source path) with
      | exception _ -> None
      | contents ->
        (match Meta.of_string contents ~name:(Some (Package_name.of_string pkg_name)) with
         | exception _ -> None
         | simplified -> Some (collect_lib_names_from_meta ~prefix:"" simplified)))
  in
  let pkg_meta = Path.Source.relative dir "pkg/META" in
  let root_meta = Path.Source.relative dir "META" in
  match try_path pkg_meta with
  | Some libs -> libs
  | None ->
    (match try_path root_meta with
     | Some libs -> libs
     | None -> [])
;;

(* Scan for .opam files and use their basenames as library names *)
let scan_opam_libraries dir =
  let path = Path.source dir in
  if not (Path.exists path)
  then []
  else (
    match Path.readdir_unsorted path with
    | Error _ -> []
    | Ok entries ->
      List.filter_map entries ~f:(fun name ->
        if Filename.check_suffix name ".opam"
        then (
          let base = Filename.chop_suffix name ".opam" in
          if is_valid_lib_name base then Some base else None)
        else None))
;;

(* Scan a vendored directory for library names.
   Cascades through: dune files -> META files -> opam files *)
let scan_libraries dir ~pkg_name =
  (* Try dune first (most accurate for dune packages) *)
  match scan_public_libraries dir with
  | _ :: _ as libs -> libs
  | [] ->
    (* Try META file (works for topkg, oasis, etc.) *)
    (match scan_meta_libraries dir ~pkg_name with
     | _ :: _ as libs -> libs
     | [] ->
       (* Fall back to opam file names *)
       scan_opam_libraries dir)
;;

(* Library -> directory mapping cache stored in _build/.pkg/libs *)
module Lib_to_dir_cache = struct
  let cache_dir = Path.Build.relative Path.Build.root ".pkg"
  let cache_file = Path.Build.relative cache_dir "libs"

  let parse_duniverse_dune () =
    let dune_path = Path.source (Path.Source.relative duniverse_dir marker_filename) in
    if not (Path.exists dune_path)
    then String.Map.empty
    else (
      match Io.read_file dune_path with
      | exception _ -> String.Map.empty
      | contents ->
        (match
           Dune_sexp.Parser.parse_string
             ~fname:(Path.to_string dune_path)
             ~mode:Many
             contents
         with
         | exception _ -> String.Map.empty
         | sexps ->
           (* Look for (vendor dirname ...) stanzas and extract libraries *)
           List.fold_left sexps ~init:String.Map.empty ~f:(fun acc sexp ->
             let open Dune_sexp.Ast in
             match sexp with
             | List (_, Atom (_, vendor_atom) :: Atom (_, dirname_atom) :: rest)
               when String.equal (Dune_sexp.Atom.to_string vendor_atom) "vendor" ->
               let dirname = Dune_sexp.Atom.to_string dirname_atom in
               (* Find (libraries ...) in rest *)
               let libs =
                 List.find_map rest ~f:(function
                   | List (_, Atom (_, libs_atom) :: lib_atoms)
                     when String.equal (Dune_sexp.Atom.to_string libs_atom) "libraries" ->
                     Some
                       (List.filter_map lib_atoms ~f:(function
                          | Atom (_, a) -> Some (Dune_sexp.Atom.to_string a)
                          | _ -> None))
                   | _ -> None)
               in
               (match libs with
                | Some lib_list ->
                  List.fold_left lib_list ~init:acc ~f:(fun acc lib ->
                    String.Map.set acc lib dirname)
                | None -> acc)
             | _ -> acc)))
  ;;

  (* Encode map as s-expression: ((lib1 dir1) (lib2 dir2) ...) *)
  let encode_map m =
    String.Map.to_list m
    |> List.map ~f:(fun (lib, dir) ->
      Dune_sexp.List [ Dune_sexp.atom lib; Dune_sexp.atom dir ])
    |> fun l -> Dune_sexp.List l
  ;;

  (* Decode map from s-expression *)
  let decode_map sexp =
    match sexp with
    | Dune_sexp.List entries ->
      List.fold_left entries ~init:String.Map.empty ~f:(fun acc entry ->
        match entry with
        | Dune_sexp.List [ Dune_sexp.Atom lib; Dune_sexp.Atom dir ] ->
          String.Map.set acc (Dune_sexp.Atom.to_string lib) (Dune_sexp.Atom.to_string dir)
        | _ -> acc)
    | _ -> String.Map.empty
  ;;

  let write_cache m =
    let path = Path.build cache_file in
    Path.mkdir_p (Path.build cache_dir);
    let sexp = encode_map m in
    Io.write_file path (Dune_sexp.to_string sexp)
  ;;

  let read_cache () =
    let path = Path.build cache_file in
    if not (Path.exists path)
    then None
    else (
      match Io.read_file path with
      | exception _ -> None
      | contents ->
        (match
           Dune_sexp.Parser.parse_string
             ~fname:(Path.to_string path)
             ~mode:Single
             contents
         with
         | exception _ -> None
         | sexp -> Some (decode_map (Dune_sexp.Ast.remove_locs sexp))))
  ;;

  let is_cache_valid () =
    let cache_path = Path.build cache_file in
    let dune_path = Path.source (Path.Source.relative duniverse_dir marker_filename) in
    if (not (Path.exists cache_path)) || not (Path.exists dune_path)
    then false
    else (
      match Path.stat cache_path, Path.stat dune_path with
      | Ok cache_stat, Ok dune_stat -> cache_stat.st_mtime >= dune_stat.st_mtime
      | _ -> false)
  ;;

  let get () =
    if is_cache_valid ()
    then (
      match read_cache () with
      | Some m -> m
      | None ->
        let m = parse_duniverse_dune () in
        write_cache m;
        m)
    else (
      let m = parse_duniverse_dune () in
      write_cache m;
      m)
  ;;

  let invalidate () =
    let path = Path.build cache_file in
    if Path.exists path then Path.rm_rf path
  ;;
end

(* Find which directory in duniverse contains a given library.
   Returns Some dirname if found, None otherwise. *)
let find_dir_for_library lib_name = String.Map.find (Lib_to_dir_cache.get ()) lib_name

(* Invalidate the library cache (call after modifying duniverse/dune) *)
let invalidate_lib_cache () = Lib_to_dir_cache.invalidate ()
