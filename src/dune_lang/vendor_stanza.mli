open Import

(** A vendor stanza specifies a vendored subdirectory and which
    libraries/packages to expose from it. This allows selective vendoring
    where only specific libraries are visible to the build system.

    Syntax:
    {[
      (vendor fmt.0.9.0 (libraries fmt fmt.tty))
      (vendor make-pkg.1.0.0 (mode opam))  ; Build using opam sandbox
      (vendor yojson.1.7.0 (libraries (yojson :as yojson_v1)))
      (vendor foo.2.0.0 (libraries (foo :as bar)) (install false))
      (vendor ocaml.5.2.0 (toolchain native))  ; Provides native compiler
      (vendor ocaml-arm.5.2.0 (toolchain arm-linux-gnueabihf))  ; Cross-compiler
    ]}

    The [(install false)] option prevents the package from being installed
    to the shared prefix. Use this for packages with library remapping that
    should coexist with other versions of the same package.

    The [(toolchain ...)] option marks the package as providing a toolchain.
    native provides the native compiler, other names provide
    cross-compilation toolchains matching [(targets ...)] in dune-workspace.

    The directory is relative to the location of the dune file containing
    the stanza. This stanza works alongside [vendored_dirs] - directories
    matched by [vendored_dirs] get all libraries visible by default, but
    [vendor] stanzas can restrict which libraries are exposed from specific
    subdirectories. *)

module Build_method : sig
  (** How a vendored package should be built.

      - [Dune_native]: Built as vendored code in the main dune context.
        Libraries are directly available to the build system.
      - [Opam_sandboxed]: Built in an isolated opam-style sandbox using
        the package's opam build commands. *)
  type t =
    | Dune_native
    | Opam_sandboxed

  val decode : t Decoder.t
  val to_dyn : t -> Dyn.t
  val to_string : t -> string
end

module Library_entry : sig
  (** A library entry in the libraries field, optionally with an alias *)
  type t =
    { lib_name : Lib_name.t
    ; alias : Lib_name.t option
    }

  val decode : t Decoder.t
  val to_dyn : t -> Dyn.t

  (** Returns the exposed name (alias if present, otherwise lib_name) *)
  val exposed_name : t -> Lib_name.t
end

type t =
  { loc : Loc.t
  ; directory : Filename.t
  ; libraries : Library_entry.t list option
  ; packages : Package_name.t list option
  ; build_method : Build_method.t option
    (** How to build this vendored package. None means use default
        (Dune_native for dune packages, Opam_sandboxed for non-dune). *)
  ; install : bool
    (** Whether to install this package to the shared prefix.
        Defaults to true. Set to false for packages with library remapping
        that should coexist with other versions of the same package. *)
  ; toolchain : string option
    (** Marks this package as providing a toolchain with the given name.
        native = native compiler, others = cross-compilation.
        The name matches what [(targets ...)] references in dune-workspace. *)
  }

val decode : t Decoder.t
val to_dyn : t -> Dyn.t

(** [library_visible t ~lib_name] returns true if the library should
    be visible according to this vendor stanza. *)
val library_visible : t -> lib_name:Lib_name.t -> bool

(** [library_exposed_name t ~lib_name] returns the exposed name for the library.
    If the library has an alias, returns the alias. If not aliased, returns the
    original name. Returns [None] if the library is not in the list. *)
val library_exposed_name : t -> lib_name:Lib_name.t -> Lib_name.t option

(** [package_visible t ~pkg_name] returns true if the package should
    be visible according to this vendor stanza. *)
val package_visible : t -> pkg_name:Package_name.t -> bool
