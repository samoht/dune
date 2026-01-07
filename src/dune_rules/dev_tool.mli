open Import
include module type of Dune_pkg.Dev_tool

val context_name_prefix : string
val context_name : t -> Context_name.t
val of_context_name : Context_name.t -> t option
val is_dev_tool_context : Context_name.t -> bool
val build_dir_of_context : Context_name.t -> Path.Build.t
val build_dir : t -> Path.Build.t
val lock_dir_of_context : Context_name.t -> Path.Build.t
val lock_dir : t -> Path.Build.t
val load_lock_dir : t -> Dune_pkg.Lock.t Memo.t
val load_lock_dir_if_exists : t -> Dune_pkg.Lock.t option Memo.t
val exe_path : t -> Path.Build.t
