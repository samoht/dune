open Import

val is_enabled : bool Lazy.t
val lock_dev_tool : Dune_pkg.Dev_tool.t -> unit Memo.t

(** Get the version of a dev tool from its lock file.
    Returns None if the lock file doesn't exist or doesn't contain the tool. *)
val dev_tool_version : Dune_pkg.Dev_tool.t -> string option Memo.t

(** Get the OCaml version from the project's default lock file.
    Used for compiler-dependent dev tools. *)
val project_ocaml_version : unit -> string option Memo.t
