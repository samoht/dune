open Import

(** A vendor stanza specifies a vendored subdirectory and which
    libraries/packages to expose from it. This allows selective vendoring
    where only specific libraries are visible to the build system.

    Syntax:
    {[
      (vendor fmt.0.9.0 (libraries fmt fmt.tty))
      (vendor make-pkg.1.0.0 (build opam))  ; Build using opam sandbox
      (vendor yojson.1.7.0 (libraries (yojson :as yojson_v1)))
    ]}

    The directory is relative to the location of the dune file containing
    the stanza. *)

module Build_method = struct
  (** How a vendored package should be built.

      - [Dune_native]: Built as vendored code in the main dune context.
        Libraries are directly available to the build system.
      - [Opam_sandboxed]: Built in an isolated opam-style sandbox using
        the package's opam build commands. *)
  type t =
    | Dune_native
    | Opam_sandboxed

  let decode =
    let open Decoder in
    (* Keep "opam" for backward compatibility *)
    enum [ "dune", Dune_native; "opam", Opam_sandboxed ]
  ;;

  let to_dyn = function
    | Dune_native -> Dyn.variant "Dune_native" []
    | Opam_sandboxed -> Dyn.variant "Opam_sandboxed" []
  ;;

  let to_string = function
    | Dune_native -> "dune"
    | Opam_sandboxed -> "opam"
  ;;
end

module Library_entry = struct
  (** A library entry in the libraries field, optionally with an alias *)
  type t =
    { lib_name : Lib_name.t
    ; alias : Lib_name.t option (* None = use original name, Some name = expose as name *)
    }

  let decode =
    let open Decoder in
    let simple =
      let+ lib_name = Lib_name.decode in
      { lib_name; alias = None }
    in
    let with_alias =
      enter
        (let* lib_name = Lib_name.decode in
         let* () = keyword ":as" in
         let+ alias = Lib_name.decode in
         { lib_name; alias = Some alias })
    in
    with_alias <|> simple
  ;;

  let to_dyn { lib_name; alias } =
    Dyn.record
      [ "lib_name", Lib_name.to_dyn lib_name
      ; "alias", Dyn.option Lib_name.to_dyn alias
      ]
  ;;

  let exposed_name t =
    match t.alias with
    | Some alias -> alias
    | None -> t.lib_name
  ;;
end

type t =
  { loc : Loc.t
  ; directory : Filename.t
  ; libraries : Library_entry.t list option
    (* None = all libraries, Some [] = none, Some libs = only these *)
  ; packages : Package_name.t list option
    (* None = all packages, Some [] = none, Some pkgs = only these *)
  ; build_method : Build_method.t option
    (* None = default (Dune_native for dune packages, auto-detect otherwise) *)
  }

let decode =
  let open Decoder in
  let* loc = loc in
  let* directory = filename in
  fields
  @@
  let+ libraries = field_o "libraries" (repeat Library_entry.decode)
  and+ packages = field_o "packages" (repeat Package_name.decode)
  and+ build_method = field_o "build" Build_method.decode in
  { loc; directory; libraries; packages; build_method }
;;

let to_dyn { loc = _; directory; libraries; packages; build_method } =
  Dyn.record
    [ "directory", Dyn.string directory
    ; "libraries", Dyn.option (Dyn.list Library_entry.to_dyn) libraries
    ; "packages", Dyn.option (Dyn.list Package_name.to_dyn) packages
    ; "build_method", Dyn.option Build_method.to_dyn build_method
    ]
;;

(** Check if a library should be visible based on vendor configuration *)
let library_visible t ~lib_name =
  match t.libraries with
  | None -> true (* No filter = all visible *)
  | Some libs ->
    List.exists libs ~f:(fun (entry : Library_entry.t) ->
      Lib_name.equal entry.lib_name lib_name)
;;

(** Find the exposed name for a library (may be aliased) *)
let library_exposed_name t ~lib_name =
  match t.libraries with
  | None -> Some lib_name (* No filter = use original name *)
  | Some libs ->
    List.find_map libs ~f:(fun (entry : Library_entry.t) ->
      if Lib_name.equal entry.lib_name lib_name
      then Some (Library_entry.exposed_name entry)
      else None)
;;

(** Check if a package should be visible based on vendor configuration *)
let package_visible t ~pkg_name =
  match t.packages with
  | None -> true
  | Some pkgs -> List.mem pkgs pkg_name ~equal:Package_name.equal
;;
