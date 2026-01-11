open Import

(** A vendor stanza specifies a vendored subdirectory and which
    libraries/packages to expose from it. This allows selective vendoring
    where only specific libraries are visible to the build system.

    Syntax:
    {[
      (vendor fmt.0.9.0 (libraries fmt fmt.tty))
      (vendor make-pkg.1.0.0 (mode opam))  ; Build using opam sandbox
      (vendor yojson.1.7.0 (libraries (yojson :as yojson_v1)))
      (vendor cohttp.6.0.0 (libraries :standard \ cohttp-async))  ; All except some
      (vendor ocaml.5.2.0 (toolchain native))  ; Provides native compiler
      (vendor ocaml-arm.5.2.0 (toolchain arm-linux-gnueabihf))  ; Cross-compiler
    ]}

    The [(libraries ...)] and [(packages ...)] fields use ordered set language:
    - [:standard] means all libraries/packages found in the directory
    - [\ name] excludes items from the set
    - [(name :as alias)] provides aliasing (incompatible with :standard)

    Using [:as] aliasing implies [(install false)] - aliased names are
    workspace-local and external packages cannot reference them. This can
    be overridden with an explicit [(install true)] if needed.

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
      [ "lib_name", Lib_name.to_dyn lib_name; "alias", Dyn.option Lib_name.to_dyn alias ]
  ;;

  let exposed_name t =
    match t.alias with
    | Some alias -> alias
    | None -> t.lib_name
  ;;
end

module Libraries_spec = struct
  (** Specification for which libraries to expose from a vendor stanza.
      Supports ordered set language with :standard and exclusions. *)
  type t =
    | All (** [:standard] or omitted - expose all libraries *)
    | All_except of Lib_name.t list (** [:standard \ lib1 lib2] - all except excluded *)
    | Explicit of Library_entry.t list (** Explicit list, possibly with aliasing *)

  let decode =
    let open Decoder in
    (* Parse a single element: either a library name or (name :as alias) *)
    let element =
      let simple =
        let+ lib_name = Lib_name.decode in
        `Lib { Library_entry.lib_name; alias = None }
      in
      let with_alias =
        enter
          (let* lib_name = Lib_name.decode in
           let* () = keyword ":as" in
           let+ alias = Lib_name.decode in
           `Lib { Library_entry.lib_name; alias = Some alias })
      in
      let standard =
        let+ () = keyword ":standard" in
        `Standard
      in
      let backslash =
        let+ () = keyword "\\" in
        `Backslash
      in
      backslash <|> standard <|> with_alias <|> simple
    in
    (* Parse the full specification *)
    let+ elements = repeat element in
    (* Process the parsed elements into a Libraries_spec.t *)
    let rec process ~has_standard ~libs ~excluded ~in_exclusion = function
      | [] ->
        if in_exclusion
        then All_except (List.rev excluded)
        else if has_standard
        then All
        else (
          match libs with
          | [] -> All
          | _ -> Explicit (List.rev libs))
      | `Standard :: rest ->
        if in_exclusion
        then User_error.raise [ Pp.text ":standard cannot appear after \\" ]
        else if has_standard
        then User_error.raise [ Pp.text ":standard can only appear once" ]
        else if libs <> []
        then
          User_error.raise [ Pp.text "Cannot mix :standard with explicit library names" ]
        else process ~has_standard:true ~libs ~excluded ~in_exclusion rest
      | `Backslash :: rest ->
        if not has_standard
        then
          User_error.raise
            [ Pp.text "\\ can only be used after :standard in vendor stanzas" ]
        else if in_exclusion
        then User_error.raise [ Pp.text "Cannot have multiple \\ operators" ]
        else process ~has_standard ~libs ~excluded ~in_exclusion:true rest
      | `Lib entry :: rest ->
        if in_exclusion
        then (
          (* In exclusion mode, only simple names are allowed *)
          match entry.Library_entry.alias with
          | Some _ ->
            User_error.raise [ Pp.text "Cannot use :as aliasing in exclusion list" ]
          | None ->
            process
              ~has_standard
              ~libs
              ~excluded:(entry.lib_name :: excluded)
              ~in_exclusion
              rest)
        else if has_standard
        then
          User_error.raise [ Pp.text "Cannot mix :standard with explicit library names" ]
        else process ~has_standard ~libs:(entry :: libs) ~excluded ~in_exclusion rest
    in
    process ~has_standard:false ~libs:[] ~excluded:[] ~in_exclusion:false elements
  ;;

  let to_dyn = function
    | All -> Dyn.variant "All" []
    | All_except excluded ->
      Dyn.variant "All_except" [ Dyn.list Lib_name.to_dyn excluded ]
    | Explicit entries -> Dyn.variant "Explicit" [ Dyn.list Library_entry.to_dyn entries ]
  ;;

  (** Check if spec uses aliasing *)
  let has_aliasing = function
    | All | All_except _ -> false
    | Explicit entries ->
      List.exists entries ~f:(fun (e : Library_entry.t) -> Option.is_some e.alias)
  ;;

  (** Check if a library should be visible according to this spec *)
  let library_visible t ~lib_name =
    match t with
    | All -> true
    | All_except excluded -> not (List.mem excluded lib_name ~equal:Lib_name.equal)
    | Explicit entries ->
      List.exists entries ~f:(fun (entry : Library_entry.t) ->
        Lib_name.equal entry.lib_name lib_name)
  ;;

  (** Find the exposed name for a library (may be aliased) *)
  let library_exposed_name t ~lib_name =
    match t with
    | All -> Some lib_name
    | All_except excluded ->
      if List.mem excluded lib_name ~equal:Lib_name.equal then None else Some lib_name
    | Explicit entries ->
      List.find_map entries ~f:(fun (entry : Library_entry.t) ->
        if Lib_name.equal entry.lib_name lib_name
        then Some (Library_entry.exposed_name entry)
        else None)
  ;;
end

module Packages_spec = struct
  (** Specification for which packages to expose from a vendor stanza.
      Supports ordered set language with :standard and exclusions. *)
  type t =
    | All (** [:standard] or omitted - expose all packages *)
    | All_except of Package_name.t list
    (** [:standard \ pkg1 pkg2] - all except excluded *)
    | Explicit of Package_name.t list (** Explicit list of packages *)

  let decode =
    let open Decoder in
    (* Parse a single element *)
    let element =
      let pkg =
        let+ name = Package_name.decode in
        `Pkg name
      in
      let standard =
        let+ () = keyword ":standard" in
        `Standard
      in
      let backslash =
        let+ () = keyword "\\" in
        `Backslash
      in
      backslash <|> standard <|> pkg
    in
    (* Parse the full specification *)
    let+ elements = repeat element in
    (* Process the parsed elements *)
    let rec process ~has_standard ~pkgs ~excluded ~in_exclusion = function
      | [] ->
        if in_exclusion
        then All_except (List.rev excluded)
        else if has_standard
        then All
        else (
          match pkgs with
          | [] -> All
          | _ -> Explicit (List.rev pkgs))
      | `Standard :: rest ->
        if in_exclusion
        then User_error.raise [ Pp.text ":standard cannot appear after \\" ]
        else if has_standard
        then User_error.raise [ Pp.text ":standard can only appear once" ]
        else if pkgs <> []
        then
          User_error.raise [ Pp.text "Cannot mix :standard with explicit package names" ]
        else process ~has_standard:true ~pkgs ~excluded ~in_exclusion rest
      | `Backslash :: rest ->
        if not has_standard
        then
          User_error.raise
            [ Pp.text "\\ can only be used after :standard in vendor stanzas" ]
        else if in_exclusion
        then User_error.raise [ Pp.text "Cannot have multiple \\ operators" ]
        else process ~has_standard ~pkgs ~excluded ~in_exclusion:true rest
      | `Pkg name :: rest ->
        if in_exclusion
        then process ~has_standard ~pkgs ~excluded:(name :: excluded) ~in_exclusion rest
        else if has_standard
        then
          User_error.raise [ Pp.text "Cannot mix :standard with explicit package names" ]
        else process ~has_standard ~pkgs:(name :: pkgs) ~excluded ~in_exclusion rest
    in
    process ~has_standard:false ~pkgs:[] ~excluded:[] ~in_exclusion:false elements
  ;;

  let to_dyn = function
    | All -> Dyn.variant "All" []
    | All_except excluded ->
      Dyn.variant "All_except" [ Dyn.list Package_name.to_dyn excluded ]
    | Explicit pkgs -> Dyn.variant "Explicit" [ Dyn.list Package_name.to_dyn pkgs ]
  ;;

  (** Check if a package should be visible according to this spec *)
  let package_visible t ~pkg_name =
    match t with
    | All -> true
    | All_except excluded -> not (List.mem excluded pkg_name ~equal:Package_name.equal)
    | Explicit pkgs -> List.mem pkgs pkg_name ~equal:Package_name.equal
  ;;
end

type t =
  { loc : Loc.t
  ; directory : Filename.t
  ; libraries : Libraries_spec.t (* Specification for which libraries to expose *)
  ; packages : Packages_spec.t (* Specification for which packages to expose *)
  ; build_method : Build_method.t option
    (* None = default (Dune_native for dune packages, auto-detect otherwise) *)
  ; install : bool
    (* Whether to install to shared prefix. Defaults to true.
       Set to false for packages with library remapping. *)
  ; toolchain : string option
    (* Marks this package as providing a toolchain with the given name.
       "native" = native compiler, others = cross-compilation *)
  }

let decode =
  let open Decoder in
  let* loc = loc in
  let* directory = filename in
  fields
  @@
  let+ libraries = field "libraries" Libraries_spec.decode ~default:Libraries_spec.All
  and+ packages = field "packages" Packages_spec.decode ~default:Packages_spec.All
  and+ build_method = field_o "mode" Build_method.decode
  and+ install_explicit = field_o "install" bool
  and+ toolchain = field_o "toolchain" string in
  (* :as aliasing implies (install false) - aliased names are workspace-local
     and external packages cannot reference them *)
  let install =
    match install_explicit with
    | Some v -> v
    | None -> not (Libraries_spec.has_aliasing libraries)
  in
  { loc; directory; libraries; packages; build_method; install; toolchain }
;;

let to_dyn { loc = _; directory; libraries; packages; build_method; install; toolchain } =
  Dyn.record
    [ "directory", Dyn.string directory
    ; "libraries", Libraries_spec.to_dyn libraries
    ; "packages", Packages_spec.to_dyn packages
    ; "build_method", Dyn.option Build_method.to_dyn build_method
    ; "install", Dyn.bool install
    ; "toolchain", Dyn.option Dyn.string toolchain
    ]
;;

(** Check if a library should be visible based on vendor configuration *)
let library_visible t ~lib_name = Libraries_spec.library_visible t.libraries ~lib_name

(** Find the exposed name for a library (may be aliased) *)
let library_exposed_name t ~lib_name =
  Libraries_spec.library_exposed_name t.libraries ~lib_name
;;

(** Check if a package should be visible based on vendor configuration *)
let package_visible t ~pkg_name = Packages_spec.package_visible t.packages ~pkg_name
