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

(** {2 Name/Version Parsing}

    Parse "name.version" strings (e.g. "foo.1.0.0") into components.
    These are the canonical helpers for parsing opam-style package directory names. *)

(** Parse "name.version" into (name, version) components. Returns None if
    the string doesn't match the expected format. *)
val parse_name_version : string -> (string * string) option

(** Extract the package name from a "name.version" string.
    Falls back to returning the input unchanged if parsing fails. *)
val parse_pkg_name_from_dir : string -> string

(** {2 Library Scanning} *)

val scan_public_libraries : pkg_name:string -> Path.Source.t -> string list
val scan_meta_libraries : Path.Source.t -> pkg_name:string -> string list
val scan_opam_libraries : Path.Source.t -> string list
val scan_libraries : Path.Source.t -> pkg_name:string -> string list

(** Scan libraries from any path (source or build). This is the unified version
    that works for both vendor packages and lock file packages. *)
val scan_libraries' : Path.t -> pkg_name:string -> string list

val read_project_name : Path.Source.t -> string option

(** Find opam file in a directory. Checks both "opam" and "<name>.opam". *)
val find_opam_file : pkg_name:string -> pkg_dir:Path.Source.t -> Path.Source.t option

(** {2 Library Cache}
    Maps library names to package names and directories. Used by dune pkg fetch
    to record which libraries are provided by which packages. *)

(** A lib-cache entry maps a library to its package and directory. *)
type lib_cache_entry =
  { lib_name : string
  ; pkg_name : string
  ; dirname : string
  }

(** Find the package name that provides a given library.
    Uses the lib-cache file written by dune pkg fetch. *)
val find_pkg_for_library : string -> string option

(** Find the directory that contains a given library.
    Uses the lib-cache file written by dune pkg fetch. *)
val find_dir_for_library : string -> string option

(** Get all lib-cache entries as (lib_name, pkg_name) pairs. *)
val lib_cache_entries : unit -> (string * string) list

val invalidate_lib_cache : unit -> unit
val lib_cache_path : unit -> Path.Build.t

(** Write lib-cache entries to file. *)
val write_lib_cache : lib_cache_entry list -> unit

(** {2 Package Build Directory}

    Path to a package's build directory: _build/.pkgs/<ctx>/<name>/
    Used by callers that need to compute marker paths. *)

val pkg_build_dir : context:Context_name.t -> pkg_name:Package.Name.t -> Path.Build.t

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
