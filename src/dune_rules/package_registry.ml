open Import

module Source = struct
  type t =
    | From_vendor of
        { source_dir : Path.Source.t
        ; stanza : Dune_lang.Vendor_stanza.t
        }
    | From_lock of { pkg : Dune_pkg.Pkg.t }
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

(* Lock files compile to the vendor registry with vendor stanzas taking precedence.

   Resolution order:
   1. First, scan all (vendor ...) stanzas → add as From_vendor
   2. Then, load lock file and for each package:
      - If already in registry from vendor stanza, skip (vendor takes precedence)
      - Otherwise add as From_lock

   This ensures vendor stanzas can override lock file packages, allowing users
   to manually vendor specific packages while still using the lock file for
   the rest of their dependencies. *)
let of_ctx =
  let impl ctx =
    let open Memo.O in
    (* Step 1: Scan all vendor stanzas - they take precedence over lock files *)
    let* vendor_stanzas = Source_tree.all_vendor_stanzas () in
    let vendor_entries, lib_to_package =
      List.fold_left
        vendor_stanzas
        ~init:(Package.Name.Map.empty, String.Map.empty)
        ~f:(fun (entries, libs) (source_dir, stanza) ->
          let pkg_name =
            (* Try to get package name from opam file, fallback to directory name *)
            let subdir = Path.Source.basename source_dir in
            match Vendor_rules.read_project_name source_dir with
            | Some name -> name
            | None ->
              (match OpamPackage.of_string_opt subdir with
               | Some pkg -> OpamPackage.Name.to_string (OpamPackage.name pkg)
               | None -> subdir)
          in
          let name = Package.Name.of_string pkg_name in
          let version, libraries =
            (* Scan version from opam file *)
            let version =
              match Vendor_rules.find_opam_file ~pkg_name ~pkg_dir:source_dir with
              | None -> Package_version.of_string "dev"
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
    let+ lock_entries =
      Lock_dir.lock_dir_active ctx
      >>= function
      | false -> Memo.return Package.Name.Map.empty
      | true ->
        let* lock_dir = Lock_dir.get_exn ctx
        and* platform = Lock_dir.Sys_vars.solver_env in
        let pkgs = Dune_pkg.Lock.packages_on_platform lock_dir ~platform in
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
        |> Memo.return
    in
    let entries =
      Package.Name.Map.union vendor_entries lock_entries ~f:(fun _ vendor _lock ->
        Some vendor)
    in
    { entries; lib_to_package }
  in
  let memo = Memo.create "package-registry" ~input:(module Context_name) impl in
  Memo.exec memo
;;
