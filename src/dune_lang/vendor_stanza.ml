open Import

(** A vendor stanza specifies a vendored subdirectory and which
    libraries/packages to expose from it. This allows selective vendoring
    where only specific libraries are visible to the build system.

    Syntax:
    {[
      (vendor fmt.0.9.0 (libraries fmt fmt.tty))
    ]}

    The directory is relative to the location of the dune file containing
    the stanza. *)

type t =
  { loc : Loc.t
  ; directory : Filename.t
  ; libraries : Lib_name.t list option
    (* None = all libraries, Some [] = none, Some libs = only these *)
  ; packages : Package_name.t list option
    (* None = all packages, Some [] = none, Some pkgs = only these *)
  }

let decode =
  let open Decoder in
  let* loc = loc in
  let* directory = filename in
  fields
  @@
  let+ libraries = field_o "libraries" (repeat Lib_name.decode)
  and+ packages = field_o "packages" (repeat Package_name.decode) in
  { loc; directory; libraries; packages }
;;

let to_dyn { loc = _; directory; libraries; packages } =
  Dyn.record
    [ "directory", Dyn.string directory
    ; "libraries", Dyn.option (Dyn.list Lib_name.to_dyn) libraries
    ; "packages", Dyn.option (Dyn.list Package_name.to_dyn) packages
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
