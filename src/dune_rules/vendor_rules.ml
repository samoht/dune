open Import
module Vendor = Dune_pkg.Vendor
module Vendor_stanza = Dune_lang.Vendor_stanza

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

(* Find opam file in a directory. Checks both "opam" and "<name>.opam".
   We check that it exists and is not a directory to avoid matching opam/ directories. *)
let find_opam_file ~pkg_name ~pkg_dir =
  let candidates =
    [ Path.Source.relative pkg_dir "opam"
    ; Path.Source.relative pkg_dir (pkg_name ^ ".opam")
    ]
  in
  List.find candidates ~f:(fun p ->
    let full_path = Path.source p in
    Path.Untracked.exists full_path && not (Path.Untracked.is_directory full_path))
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

(* Parse "name.version" strings into components. These are the canonical helpers
   for parsing opam-style package directory names. *)
let parse_name_version dir_name =
  match OpamPackage.of_string_opt dir_name with
  | Some pkg ->
    let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
    let version = OpamPackage.Version.to_string (OpamPackage.version pkg) in
    Some (name, version)
  | None -> None
;;

let parse_pkg_name_from_dir dir_name =
  match parse_name_version dir_name with
  | Some (name, _) -> name
  | None -> dir_name
;;

(* Lib-cache for looking up library→package→directory mappings.
   Format: lib_name:pkg_name:dirname
   - lib_name: the library name (e.g., "cmdliner")
   - pkg_name: the package name (e.g., "cmdliner")
   - dirname: the directory name in duniverse (e.g., "cmdliner.1.2.0") *)

type lib_cache_entry =
  { lib_name : string
  ; pkg_name : string
  ; dirname : string
  }

let lib_cache_file =
  lazy
    (let pkg_dir = Path.build Dpath.Build.pkgs_dir in
     Path.relative pkg_dir "lib-cache")
;;

(* Internal cache indexed by lib_name for fast lookup *)
let lib_cache : lib_cache_entry String.Table.t option ref = ref None

let load_lib_cache () =
  let cache_file = Lazy.force lib_cache_file in
  if Path.Untracked.exists cache_file
  then (
    let lines = Io.lines_of_file cache_file in
    let tbl = String.Table.create 256 in
    List.iter lines ~f:(fun line ->
      match String.lsplit2 line ~on:':' with
      | Some (lib_name, rest) ->
        (match String.lsplit2 rest ~on:':' with
         | Some (pkg_name, dirname) ->
           String.Table.set tbl lib_name { lib_name; pkg_name; dirname }
         | None -> ())
      | None -> ());
    Some tbl)
  else None
;;

(* Find the package that provides a given library.
   Uses the lib-cache file written by dune pkg fetch. *)
let find_pkg_for_library lib_name =
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
  | Some tbl ->
    (match String.Table.find tbl lib_name with
     | Some entry -> Some entry.pkg_name
     | None -> None)
;;

(* Find the directory that contains a given library.
   Uses the lib-cache file written by dune pkg fetch. *)
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
  | Some tbl ->
    (match String.Table.find tbl lib_name with
     | Some entry -> Some entry.dirname
     | None -> None)
;;

let invalidate_lib_cache () = lib_cache := None

(* Get all entries from lib-cache as (lib_name, pkg_name) pairs *)
let lib_cache_entries () =
  let cache =
    match !lib_cache with
    | Some c -> Some c
    | None ->
      let c = load_lib_cache () in
      lib_cache := c;
      c
  in
  match cache with
  | None -> []
  | Some tbl ->
    String.Table.to_list tbl |> List.map ~f:(fun (lib, entry) -> lib, entry.pkg_name)
;;

(* Return the path to the lib-cache file as a build path *)
let lib_cache_path () = Path.Build.relative Dpath.Build.pkgs_dir "lib-cache"

(* Write lib-cache entries to file. *)
let write_lib_cache entries =
  let cache_path = Path.build (lib_cache_path ()) in
  let parent = Path.parent_exn cache_path in
  Path.mkdir_p parent;
  let lines =
    List.map entries ~f:(fun { lib_name; pkg_name; dirname } ->
      sprintf "%s:%s:%s" lib_name pkg_name dirname)
  in
  Io.write_lines cache_path lines;
  invalidate_lib_cache ()
;;

module Vendored_map = struct
  type package_info =
    { version : Package_version.t
    ; source_dir : Path.Source.t
    ; libraries : string list
    ; build_method : Dune_lang.Vendor_stanza.Build_method.t option
      (* None or Some Dune_native = built as workspace code, no marker needed
         Some Opam_sandboxed = built in sandbox, needs marker file *)
    ; install_to_prefix : bool
      (* Whether artifacts are copied to shared install prefix.
         When false, depend on cookie file instead of installed marker. *)
    ; compiler : Package.Name.t option
      (* If set, this package provides an OCaml compiler with this name.
         Referenced by (context (workspace (compiler ...))) in dune-workspace. *)
    ; toolchain : string option
      (* If set, this package provides a findlib toolchain for cross-compilation.
         The name matches what (targets ...) references (e.g., "windows"). *)
    }

  type t =
    { packages : package_info Package.Name.Map.t
    ; lib_to_package : Package.Name.t String.Map.t
    }

  let empty = { packages = Package.Name.Map.empty; lib_to_package = String.Map.empty }

  let add
        t
        ~name
        ~version
        ~source_dir
        ~libraries
        ~build_method
        ~install_to_prefix
        ~compiler
        ~toolchain
    =
    let info =
      { version
      ; source_dir
      ; libraries
      ; build_method
      ; install_to_prefix
      ; compiler
      ; toolchain
      }
    in
    let packages = Package.Name.Map.set t.packages name info in
    let lib_to_package =
      List.fold_left libraries ~init:t.lib_to_package ~f:(fun acc lib ->
        String.Map.set acc lib name)
    in
    { packages; lib_to_package }
  ;;

  let find t name = Package.Name.Map.find t.packages name
  let is_installed t name = Package.Name.Map.mem t.packages name

  let version t name =
    match Package.Name.Map.find t.packages name with
    | Some info -> Some info.version
    | None -> None
  ;;

  let source_dir t name =
    match Package.Name.Map.find t.packages name with
    | Some info -> Some info.source_dir
    | None -> None
  ;;

  let package_for_library t lib_name = String.Map.find t.lib_to_package lib_name
  let all_packages t = Package.Name.Map.keys t.packages

  let needs_marker t name =
    match Package.Name.Map.find t.packages name with
    | None -> false
    | Some info ->
      (match info.build_method with
       | Some Vendor_stanza.Build_method.Opam_sandboxed -> true
       | Some Dune_native | None -> false)
  ;;

  let install_to_prefix t name =
    match Package.Name.Map.find t.packages name with
    | None -> true (* Default to true for unknown packages *)
    | Some info -> info.install_to_prefix
  ;;

  let compiler t name =
    match Package.Name.Map.find t.packages name with
    | None -> None
    | Some info -> info.compiler
  ;;

  let toolchain t name =
    match Package.Name.Map.find t.packages name with
    | None -> None
    | Some info -> info.toolchain
  ;;

  let find_compiler t compiler_name =
    Package.Name.Map.to_list t.packages
    |> List.find_map ~f:(fun (pkg_name, info) ->
      match info.compiler with
      | Some c when Package.Name.equal c compiler_name -> Some pkg_name
      | _ -> None)
  ;;

  let find_toolchain t toolchain_name =
    Package.Name.Map.to_list t.packages
    |> List.find_map ~f:(fun (pkg_name, info) ->
      match info.toolchain with
      | Some t when String.equal t toolchain_name -> Some pkg_name
      | _ -> None)
  ;;
end

(* Scan a vendor package directory and extract package info *)
let scan_vendor_package ~pkg_name ~pkg_dir =
  let version =
    match find_opam_file ~pkg_name ~pkg_dir with
    | None -> Package_version.of_string "dev"
    | Some opam_file ->
      let contents = Io.read_file ~binary:true (Path.source opam_file) in
      (match OpamFile.OPAM.read_from_string contents with
       | exception _ -> Package_version.of_string "dev"
       | opam ->
         (match OpamFile.OPAM.version_opt opam with
          | Some v -> Package_version.of_string (OpamPackage.Version.to_string v)
          | None -> Package_version.of_string "dev"))
  in
  let libraries = scan_libraries pkg_dir ~pkg_name in
  version, libraries
;;

(* Scan duniverse directory and build vendored package map *)
let scan_vendor_dir vendor_dir =
  let full_path = Path.source vendor_dir in
  if not (Path.Untracked.exists full_path)
  then Vendored_map.empty
  else (
    match Path.Untracked.readdir_unsorted_with_kinds full_path with
    | Error _ -> Vendored_map.empty
    | Ok entries ->
      List.fold_left entries ~init:Vendored_map.empty ~f:(fun map (entry, kind) ->
        match kind with
        | Unix.S_DIR ->
          let pkg_name = entry in
          let pkg_dir = Path.Source.relative vendor_dir entry in
          let version, libraries = scan_vendor_package ~pkg_name ~pkg_dir in
          Vendored_map.add
            map
            ~name:(Package.Name.of_string pkg_name)
            ~version
            ~source_dir:pkg_dir
            ~libraries
            ~build_method:None
            ~install_to_prefix:true
            ~compiler:None
            ~toolchain:None
        | _ -> map))
;;

(* Try to read package name from opam file in directory *)
let read_pkg_name_from_opam pkg_dir =
  let full_path = Path.source pkg_dir in
  match Path.Untracked.readdir_unsorted full_path with
  | Error _ -> None
  | Ok entries ->
    List.find_map entries ~f:(fun entry ->
      if String.equal entry "opam" || String.is_suffix entry ~suffix:".opam"
      then (
        let opam_path = Path.relative full_path entry in
        if Path.Untracked.exists opam_path
        then (
          let contents = Io.read_file ~binary:true opam_path in
          match OpamFile.OPAM.read_from_string contents with
          | exception _ -> None
          | opam ->
            (match OpamFile.OPAM.name_opt opam with
             | Some n -> Some (OpamPackage.Name.to_string n)
             | None -> None))
        else None)
      else None)
;;

let get_vendored_map =
  let impl () =
    let open Memo.O in
    let+ stanzas = Source_tree.all_vendor_stanzas () in
    List.fold_left stanzas ~init:Vendored_map.empty ~f:(fun map (pkg_dir, stanza) ->
      let subdir = Path.Source.basename pkg_dir in
      (* Try to get package name from opam file, fallback to directory parsing *)
      let pkg_name =
        match read_pkg_name_from_opam pkg_dir with
        | Some name -> name
        | None -> parse_pkg_name_from_dir subdir
      in
      let version, libraries = scan_vendor_package ~pkg_name ~pkg_dir in
      Vendored_map.add
        map
        ~name:(Package.Name.of_string pkg_name)
        ~version
        ~source_dir:pkg_dir
        ~libraries
        ~build_method:stanza.Vendor_stanza.build_method
        ~install_to_prefix:stanza.Vendor_stanza.install
        ~compiler:stanza.Vendor_stanza.compiler
        ~toolchain:stanza.Vendor_stanza.toolchain)
  in
  Memo.lazy_ ~name:"vendored-map" impl
;;

let get_vendored_map () = Memo.Lazy.force get_vendored_map

module Paths = struct
  type 'a t =
    { source_dir : 'a
    ; target_dir : 'a
    ; extra_sources : 'a
    ; name : Package.Name.t
    ; install_roots : 'a Install.Roots.t Lazy.t
    ; install_paths : 'a Install.Paths.t Lazy.t
    ; prefix : 'a
    }

  let map_path t ~f =
    { t with
      source_dir = f t.source_dir
    ; target_dir = f t.target_dir
    ; extra_sources = f t.extra_sources
    ; install_roots = Lazy.map ~f:(Install.Roots.map ~f) t.install_roots
    ; install_paths = Lazy.map ~f:(Install.Paths.map ~f) t.install_paths
    ; prefix = f t.prefix
    }
  ;;

  let install_roots ~target_dir ~relative =
    Install.Roots.opam_from_prefix ~relative target_dir
  ;;

  let install_paths roots package ~relative = Install.Paths.make ~relative ~package ~roots

  let of_root name ~root ~relative =
    let source_dir = relative root "source" in
    let target_dir = relative root "target" in
    let extra_sources = relative root "extra_source" in
    let install_roots = lazy (install_roots ~target_dir ~relative) in
    let install_paths = lazy (install_paths (Lazy.force install_roots) name ~relative) in
    { source_dir
    ; target_dir
    ; extra_sources
    ; name
    ; install_paths
    ; install_roots
    ; prefix = target_dir
    }
  ;;

  (* Legacy cookie path inside target_dir - kept for backward compatibility *)
  let make_install_cookie target_dir ~relative = relative target_dir "cookie"

  let install_cookie' target_dir =
    make_install_cookie target_dir ~relative:Path.Build.relative
  ;;

  let install_cookie t = make_install_cookie t.target_dir ~relative:Path.relative
  let target_dir t = t.target_dir
  let source_dir t = t.source_dir
  let install_paths t = Lazy.force t.install_paths
end

(* Path to a package's build directory: _build/.pkgs/<ctx>/<name>/
   Note: Path uses only package name (no version) to enable cleanup on upgrade.
   Exposed for use by callers that need to compute marker paths. *)
let pkg_build_dir ~context ~pkg_name =
  let ctx_dir =
    Path.Build.relative Dpath.Build.pkgs_dir (Context_name.to_string context)
  in
  Path.Build.relative ctx_dir (Package.Name.to_string pkg_name)
;;
