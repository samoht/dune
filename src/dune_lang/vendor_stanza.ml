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
    the stanza. *)

module Sandbox_mode = struct
  type t =
    | None
    | Opam

  let decode =
    let open Decoder in
    enum [ "none", None; "opam", Opam ]
  ;;

  let to_dyn = function
    | None -> Dyn.variant "None" []
    | Opam -> Dyn.variant "Opam" []
  ;;

  let to_string = function
    | None -> "none"
    | Opam -> "opam"
  ;;
end

type t =
  { loc : Loc.t
  ; directory : Filename.t
  ; libraries : Lib_name.t list option
    (* None = all libraries, Some [] = none, Some libs = only these *)
  ; packages : Package_name.t list option
    (* None = all packages, Some [] = none, Some pkgs = only these *)
  ; sandbox : Sandbox_mode.t option
    (* None = default (no sandbox), Some Opam = build in opam sandbox *)
  }

let decode =
  let open Decoder in
  let* loc = loc in
  let* directory = filename in
  fields
  @@
  let+ libraries = field_o "libraries" (repeat Lib_name.decode)
  and+ packages = field_o "packages" (repeat Package_name.decode)
  and+ sandbox = field_o "sandbox" Sandbox_mode.decode in
  { loc; directory; libraries; packages; sandbox }
;;

let to_dyn { loc = _; directory; libraries; packages; sandbox } =
  Dyn.record
    [ "directory", Dyn.string directory
    ; "libraries", Dyn.option (Dyn.list Lib_name.to_dyn) libraries
    ; "packages", Dyn.option (Dyn.list Package_name.to_dyn) packages
    ; "sandbox", Dyn.option Sandbox_mode.to_dyn sandbox
    ]
;;

(** Check if a library should be visible based on vendor configuration *)
let library_visible t ~lib_name =
  match t.libraries with
  | None -> true (* No filter = all visible *)
  | Some libs -> List.mem libs lib_name ~equal:Lib_name.equal
;;

(** Check if a package should be visible based on vendor configuration *)
let package_visible t ~pkg_name =
  match t.packages with
  | None -> true
  | Some pkgs -> List.mem pkgs pkg_name ~equal:Package_name.equal
;;
