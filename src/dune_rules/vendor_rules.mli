open Import

val scan_public_libraries : Path.Source.t -> string list
val scan_meta_libraries : Path.Source.t -> pkg_name:string -> string list
val scan_opam_libraries : Path.Source.t -> string list
val scan_libraries : Path.Source.t -> pkg_name:string -> string list
val read_project_name : Path.Source.t -> string option
val find_dir_for_library : string -> string option
val invalidate_lib_cache : unit -> unit
