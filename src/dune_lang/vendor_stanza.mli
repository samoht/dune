open Import

(** A vendor stanza specifies a vendored subdirectory and which
    libraries/packages to expose from it. This allows selective vendoring
    where only specific libraries are visible to the build system.

    Syntax:
    {[
      (vendor fmt.0.9.0 (libraries fmt fmt.tty))
      (vendor make-pkg.1.0.0 (sandbox opam))
    ]}

    The directory is relative to the location of the dune file containing
    the stanza. This stanza works alongside [vendored_dirs] - directories
    matched by [vendored_dirs] get all libraries visible by default, but
    [vendor] stanzas can restrict which libraries are exposed from specific
    subdirectories. *)

module Sandbox_mode : sig
  type t =
    | None
    | Opam

  val decode : t Decoder.t
  val to_dyn : t -> Dyn.t
  val to_string : t -> string
end

type t =
  { loc : Loc.t
  ; directory : Filename.t
  ; libraries : Lib_name.t list option
  ; packages : Package_name.t list option
  ; sandbox : Sandbox_mode.t option
  }

val decode : t Decoder.t
val to_dyn : t -> Dyn.t

(** [library_visible t ~lib_name] returns true if the library should
    be visible according to this vendor stanza. *)
val library_visible : t -> lib_name:Lib_name.t -> bool

(** [package_visible t ~pkg_name] returns true if the package should
    be visible according to this vendor stanza. *)
val package_visible : t -> pkg_name:Package_name.t -> bool
