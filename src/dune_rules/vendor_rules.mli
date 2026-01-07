open Import

(** {1 Vendor Infrastructure}

    The vendor infrastructure is the primitive foundation for building external
    packages. Lock files are a higher-level abstraction that compiles to the
    vendor infrastructure.

    Architecture:
    {v
    Lock file → compiles to → Vendor stanzas → builds packages
                                  ↑
             Manual vendor stanzas also work directly
    v}
*)

(** {2 Library Scanning} *)

val scan_public_libraries : Path.Source.t -> string list
val scan_meta_libraries : Path.Source.t -> pkg_name:string -> string list
val scan_opam_libraries : Path.Source.t -> string list
val scan_libraries : Path.Source.t -> pkg_name:string -> string list
val read_project_name : Path.Source.t -> string option

(** Find opam file in a directory. Checks both "opam" and "<name>.opam". *)
val find_opam_file : pkg_name:string -> pkg_dir:Path.Source.t -> Path.Source.t option

(** {2 Library Cache}
    Maps library names to directory names in duniverse/. Used by dune pkg fetch. *)

val find_dir_for_library : string -> string option
val invalidate_lib_cache : unit -> unit

(** {2 Vendored Package Map}

    Maps packages to their info and provides library→package reverse lookup.
    Built by scanning the duniverse directory. *)

module Vendored_map : sig
  type package_info =
    { version : Package_version.t
    ; source_dir : Path.Source.t
    ; libraries : string list
    ; build_method : Dune_lang.Vendor_stanza.Build_method.t option
    }

  type t

  val empty : t

  val add
    :  t
    -> name:Package.Name.t
    -> version:Package_version.t
    -> source_dir:Path.Source.t
    -> libraries:string list
    -> build_method:Dune_lang.Vendor_stanza.Build_method.t option
    -> t

  val find : t -> Package.Name.t -> package_info option
  val is_installed : t -> Package.Name.t -> bool
  val version : t -> Package.Name.t -> Package_version.t option
  val source_dir : t -> Package.Name.t -> Path.Source.t option
  val package_for_library : t -> string -> Package.Name.t option
  val all_packages : t -> Package.Name.t list
  val needs_marker : t -> Package.Name.t -> bool
end

val scan_vendor_dir : Path.Source.t -> Vendored_map.t
val get_vendored_map : unit -> Vendored_map.t Memo.t

(** {2 On-demand Build Triggers}

    These functions support on-demand vendor package building.
    When a library is requested, callers can check if it comes from a
    vendor package and add a dependency on the marker file to ensure
    the package is built first. *)

(** Get the marker file for a vendor package. Returns None if the package
    doesn't exist or isn't an opam vendor package. *)
val marker_for_package
  :  context:Context_name.t
  -> Package.Name.t
  -> Path.Build.t option Memo.t

(** Given a library name, find the vendor package that provides it and return
    the marker file needed to trigger that package's build.
    Returns None if the library is not from a vendor package. *)
val marker_for_library : context:Context_name.t -> string -> Path.Build.t option Memo.t

(** {2 Package Paths}

    Package paths define the directory structure for building packages.
    Both lock file packages and vendor packages use the same path layout. *)

module Paths : sig
  type 'a t =
    { source_dir : 'a
    ; target_dir : 'a
    ; extra_sources : 'a
    ; name : Package.Name.t
    ; install_roots : 'a Install.Roots.t Lazy.t
    ; install_paths : 'a Install.Paths.t Lazy.t
    ; prefix : 'a
    }

  val map_path : 'a t -> f:('a -> 'b) -> 'b t
  val of_root : Package.Name.t -> root:'a -> relative:('a -> string -> 'a) -> 'a t
  val install_cookie : Path.t t -> Path.t
  val install_cookie' : Path.Build.t -> Path.Build.t
  val target_dir : 'a t -> 'a
  val source_dir : 'a t -> 'a
  val install_paths : 'a t -> 'a Install.Paths.t
end

(** {2 Opam Package Building}

    Build infrastructure for opam-style packages. This handles variable
    expansion (%{prefix}%, %{lib}%, %{make}%, etc.) and command execution. *)

(** Build an opam package from an opam file.
    This is the foundation that both lock file packages and vendor packages use.

    @param context Build context name
    @param pkg_name Package name
    @param pkg_version Package version
    @param source_dir Path to package source
    @param opam_file Parsed opam file
    @return Marker file and build action *)
val build_opam_package
  :  context:Context_name.t
  -> pkg_name:Package.Name.t
  -> pkg_version:Package_version.t
  -> source_dir:Path.Source.t
  -> opam_file:OpamFile.OPAM.t
  -> (Path.Build.t * Action.Full.t Action_builder.With_targets.t) Memo.t

(** Set up rules for a vendor package in the pkg context.
    Called from pkg_rules when handling _build/pkg/<ctx>/<name>.<version>/.
    Returns None if the package directory doesn't correspond to a vendor package.
    Returns the build action with targets for the cookie file. *)
val setup_vendor_package_rules
  :  context:Context_name.t
  -> pkg_dir:string
  -> Action.Full.t Action_builder.With_targets.t option Memo.t
