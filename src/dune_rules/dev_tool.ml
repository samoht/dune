open Import
include Dune_pkg.Dev_tool

let context_name_prefix = "dev-tools"

(* Each dev tool gets its own isolated context for building. *)
let context_name t =
  let tool_name = Package.Name.to_string (package_name t) in
  Context_name.of_string (sprintf "%s-%s" context_name_prefix tool_name)
;;

(* Check if a context is a dev tool context and return the dev tool if so *)
let of_context_name ctx =
  let s = Context_name.to_string ctx in
  let prefix = context_name_prefix ^ "-" in
  match String.drop_prefix s ~prefix with
  | Some tool_name -> of_package_name_opt (Package.Name.of_string tool_name)
  | None -> None
;;

(* Check if a context is a dev tool context *)
let is_dev_tool_context ctx = Option.is_some (of_context_name ctx)

(* Build context directory: _build/<ctx>/ *)
let build_dir_of_context ctx =
  Path.Build.relative Path.Build.root (Context_name.to_string ctx)
;;

let build_dir t = build_dir_of_context (context_name t)
let lock_dir_of_context ctx = Lock_dir.build_dir ctx
let lock_dir t = lock_dir_of_context (context_name t)

let load_lock_dir t =
  let path = lock_dir t |> Path.build in
  Lock_dir.load_exn path
;;

let load_lock_dir_if_exists t =
  let path = lock_dir t |> Path.build in
  Lock_dir.load_if_exists path
;;

let exe_path t =
  let ctx = context_name t in
  let install_dir = Install.Context.dir ~context:ctx in
  Path.Build.L.relative install_dir (exe_path_components_within_package t)
;;
