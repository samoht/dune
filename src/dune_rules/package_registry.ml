open Import

module Source = struct
  type t =
    | From_vendor of
        { source_dir : Path.Source.t
        ; stanza : Dune_lang.Vendor_stanza.t
        }
    | From_lock of { pkg : Dune_pkg.Pkg.t }
    | From_workspace of
        { pkg : Package.t
        ; source_dir : Path.Source.t
        }
end

type entry =
  { name : Package.Name.t
  ; version : Package_version.t
  ; source : Source.t
  }

type t =
  { entries : entry Package.Name.Map.t
  ; lib_to_package : Package.Name.t String.Map.t
  }

let find t name = Package.Name.Map.find t.entries name

let find_by_name_version t ~name ~version =
  match Package.Name.Map.find t.entries name with
  | Some entry when Package_version.equal entry.version version -> Some entry
  | _ -> None
;;

let mem t name = Package.Name.Map.mem t.entries name
let to_list t = Package.Name.Map.values t.entries

let version t name =
  match Package.Name.Map.find t.entries name with
  | Some entry -> Some entry.version
  | None -> None
;;

let package_for_library t lib_name = String.Map.find t.lib_to_package lib_name

(* Check if a package needs a marker file for dependency tracking.
   Lock packages and opam-sandboxed vendor packages need markers. *)
let needs_marker entry =
  match entry.source with
  | Source.From_lock _ -> true
  | Source.From_vendor { stanza; _ } ->
    (match stanza.build_method with
     | Some Dune_lang.Vendor_stanza.Build_method.Opam_sandboxed -> true
     | Some Dune_native | None -> false)
  | Source.From_workspace _ -> false
;;

(* Check if a package should install to the shared prefix.
   Most packages do, except vendor packages with install=false. *)
let install_to_prefix entry =
  match entry.source with
  | Source.From_lock _ -> true
  | Source.From_vendor { stanza; _ } -> stanza.install
  | Source.From_workspace _ -> true
;;

(* Get the compiler name provided by a package, if any.
   Only vendor packages can declare compiler providers. *)
let compiler entry =
  match entry.source with
  | Source.From_vendor { stanza; _ } -> stanza.compiler
  | Source.From_lock _ | Source.From_workspace _ -> None
;;

(* Get the toolchain name provided by a package, if any.
   Only vendor packages can declare toolchain providers. *)
let toolchain entry =
  match entry.source with
  | Source.From_vendor { stanza; _ } -> stanza.toolchain
  | Source.From_lock _ | Source.From_workspace _ -> None
;;

let of_lock_packages pkgs =
  let entries =
    Package.Name.Map.mapi pkgs ~f:(fun name pkg ->
      { name; version = pkg.Dune_pkg.Pkg.info.version; source = Source.From_lock { pkg } })
  in
  (* For lock packages, assume each package provides a library with the same name.
     This is a reasonable default; the lib-cache can provide more accurate mappings
     once packages are fetched and scanned. *)
  let lib_to_package =
    Package.Name.Map.fold pkgs ~init:String.Map.empty ~f:(fun pkg acc ->
      let name = pkg.Dune_pkg.Pkg.info.name in
      let lib_name = Package.Name.to_string name in
      String.Map.set acc lib_name name)
  in
  { entries; lib_to_package }
;;

(* Package registry merges vendor stanzas and lock files.

   Resolution order (vendor > lock):
   1. First, scan all (vendor ...) stanzas → add as From_vendor
   2. Then, load lock file packages → add as From_lock
      (skip if already provided by vendor)

   Note: Workspace packages are handled separately in pkg_rules.ml to avoid
   dependency cycles. When resolving dependencies, pkg_rules checks if a package
   name matches a workspace package and treats it as a From_workspace dependency. *)
let of_ctx =
  let impl_workspace ctx =
    let open Memo.O in
    (* Step 1: Scan all vendor stanzas - they take precedence over lock files *)
    let* vendor_stanzas = Source_tree.all_vendor_stanzas () in
    let vendor_entries, lib_to_package =
      List.fold_left
        vendor_stanzas
        ~init:(Package.Name.Map.empty, String.Map.empty)
        ~f:(fun (entries, libs) (source_dir, stanza) ->
          let subdir = Path.Source.basename source_dir in
          let pkg_name =
            (* Try to get package name from opam file, fallback to directory name *)
            match Vendor_rules.read_project_name source_dir with
            | Some name -> name
            | None -> Vendor_rules.parse_pkg_name_from_dir subdir
          in
          let name = Package.Name.of_string pkg_name in
          let version, libraries =
            (* Scan version from opam file, fall back to directory name *)
            let version =
              match Vendor_rules.find_opam_file ~pkg_name ~pkg_dir:source_dir with
              | None ->
                (* No opam file - try to extract version from directory name like "pkg.1.0.0" *)
                (match Vendor_rules.parse_name_version subdir with
                 | Some (_, v) -> Package_version.of_string v
                 | None -> Package_version.of_string "dev")
              | Some opam_file ->
                let contents = Io.read_file ~binary:true (Path.source opam_file) in
                (match OpamFile.OPAM.read_from_string contents with
                 | exception _ -> Package_version.of_string "dev"
                 | opam ->
                   (match OpamFile.OPAM.version_opt opam with
                    | Some v ->
                      Package_version.of_string (OpamPackage.Version.to_string v)
                    | None -> Package_version.of_string "dev"))
            in
            let libraries = Vendor_rules.scan_libraries source_dir ~pkg_name in
            version, libraries
          in
          let entry =
            { name; version; source = Source.From_vendor { source_dir; stanza } }
          in
          let entries = Package.Name.Map.set entries name entry in
          let libs =
            List.fold_left libraries ~init:libs ~f:(fun acc lib ->
              String.Map.set acc lib name)
          in
          entries, libs)
    in
    (* Step 2: Load lock file packages - skip those already provided by vendor *)
    let+ lock_entries, lock_lib_to_package =
      Lock_dir.lock_dir_active ctx
      >>= function
      | false -> Memo.return (Package.Name.Map.empty, String.Map.empty)
      | true ->
        let* lock_dir = Lock_dir.get_exn ctx
        and* platform = Lock_dir.Sys_vars.solver_env in
        let pkgs = Dune_pkg.Lock.packages_on_platform lock_dir ~platform in
        let entries =
          Package.Name.Map.foldi pkgs ~init:Package.Name.Map.empty ~f:(fun name pkg acc ->
            if Package.Name.Map.mem vendor_entries name
            then acc (* Vendor takes precedence *)
            else (
              let entry =
                { name
                ; version = pkg.Dune_pkg.Pkg.info.version
                ; source = Source.From_lock { pkg }
                }
              in
              Package.Name.Map.set acc name entry))
        in
        (* Build lib_to_package for lock entries.
           First use default (package provides library with same name),
           then merge with lib-cache which has accurate mappings. *)
        let libs =
          Package.Name.Map.fold entries ~init:String.Map.empty ~f:(fun entry acc ->
            let pkg_name_str = Package.Name.to_string entry.name in
            (* Default: assume package provides library with same name *)
            String.Map.set acc pkg_name_str entry.name)
        in
        (* The lib-cache (populated by dune pkg fetch) provides accurate mappings
           from library names to package names. Merge these in. *)
        let libs =
          List.fold_left
            (Vendor_rules.lib_cache_entries ())
            ~init:libs
            ~f:(fun acc (lib_name, pkg_name) ->
              let name = Package.Name.of_string pkg_name in
              (* Only add if this package is in our lock entries *)
              if Package.Name.Map.mem entries name
              then String.Map.set acc lib_name name
              else acc)
        in
        Memo.return (entries, libs)
    in
    let entries =
      Package.Name.Map.union vendor_entries lock_entries ~f:(fun _ vendor _lock ->
        Some vendor)
    in
    (* Merge lib_to_package: vendor entries take precedence *)
    let lib_to_package =
      String.Map.union lib_to_package lock_lib_to_package ~f:(fun _ vendor _lock ->
        Some vendor)
    in
    { entries; lib_to_package }
  in
  let impl_dev_tool dev_tool =
    let open Memo.O in
    let* lock_dir_opt = Dev_tool.load_lock_dir_if_exists dev_tool in
    match lock_dir_opt with
    | None ->
      Memo.return { entries = Package.Name.Map.empty; lib_to_package = String.Map.empty }
    | Some lock_dir ->
      let+ platform = Lock_dir.Sys_vars.solver_env in
      let pkgs = Dune_pkg.Lock.packages_on_platform lock_dir ~platform in
      of_lock_packages pkgs
  in
  let impl ctx =
    (* Check if this is a dev tool context *)
    match Dev_tool.of_context_name ctx with
    | Some dev_tool -> impl_dev_tool dev_tool
    | None -> impl_workspace ctx
  in
  let memo = Memo.create "package-registry" ~input:(module Context_name) impl in
  Memo.exec memo
;;
