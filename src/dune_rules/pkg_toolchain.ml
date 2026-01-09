open Import
module Pkg = Dune_pkg.Pkg

let base_dir =
  lazy
    (let dir = Path.relative (Lazy.force Dune_util.cache_root_dir) "toolchains" in
     Log.info "Toolchains cache location" [ "dir", Dyn.string (Path.to_string dir) ];
     Path.as_outside_build_dir_exn dir)
;;

let base_dir () =
  let base_dir = Lazy.force base_dir in
  let path = Path.outside_build_dir base_dir in
  if not (Path.Untracked.exists path) then Path.mkdir_p path;
  if not (Path.Untracked.is_directory path)
  then
    User_error.raise
      [ Pp.textf "Expected %s to be a directory but it is not." (Path.to_string path) ];
  base_dir
;;

(* The relocatable-compiler package from dra27's overlay can be installed at
   any path and moved afterwards, enabling cache sharing across projects.
   See https://github.com/ocaml/RFCs/pull/53 *)
let relocatable_compiler_name = Package.Name.of_string "relocatable-compiler"
let is_relocatable_compiler name = Package.Name.equal name relocatable_compiler_name

(* Extract the base OCaml version from a relocatable-compiler version.
   relocatable-compiler versions are like "5.3.0.20241026.1" where "5.3.0" is
   the OCaml version. We extract just the OCaml version for cache key purposes. *)
let relocatable_base_version version =
  let version_str = Package_version.to_string version in
  (* Split on '.' and take first 3 components (major.minor.patch) *)
  match String.split version_str ~on:'.' with
  | major :: minor :: patch :: _ -> Some (String.concat ~sep:"." [ major; minor; patch ])
  | _ -> None
;;

(* Get a readable platform identifier from enabled_on_platforms.
   For cross-compilation, different platforms need different cache entries.
   Returns something like "-macos-arm64" or "-linux-x86_64". *)
let platform_suffix (pkg : Pkg.t) =
  match pkg.enabled_on_platforms with
  | [] -> ""
  | [ platform ] ->
    (* Extract os and arch from the single platform *)
    let os =
      Dune_pkg.Solver_env.get platform Dune_lang.Package_variable_name.os
      |> Option.map ~f:Dune_pkg.Variable_value.to_string
    in
    let arch =
      Dune_pkg.Solver_env.get platform Dune_lang.Package_variable_name.arch
      |> Option.map ~f:Dune_pkg.Variable_value.to_string
    in
    (match os, arch with
     | Some os, Some arch -> sprintf "-%s-%s" os arch
     | Some os, None -> sprintf "-%s" os
     | None, Some arch -> sprintf "-%s" arch
     | None, None -> "")
  | platforms ->
    (* Multiple platforms - use hash for uniqueness *)
    let platform_digest = Dune_digest.generic platforms in
    let hash = Dune_digest.to_string platform_digest in
    "-" ^ String.sub hash ~pos:0 ~len:(min 8 (String.length hash))
;;

let pkg_dir (pkg : Pkg.t) ~build_id =
  (* The name of this package's directory within the toolchains directory.

     For relocatable-compiler packages, we use the base OCaml version plus
     platform hash and build_id. The build_id is a recursive hash of the
     package's opam content and all its dependencies' build_ids, ensuring
     any change in the dependency graph produces a different cache entry.

     For other packages (including non-relocatable compilers), we use the
     build_id directly since it already includes platform-specific info
     from the opam file hash. *)
  let dir_name =
    let name = pkg.info.name in
    if is_relocatable_compiler name
    then (
      (* Relocatable compiler: use base OCaml version + platform + build_id *)
      let base_version =
        relocatable_base_version pkg.info.version
        |> Option.value ~default:(Package_version.to_string pkg.info.version)
      in
      let platform = platform_suffix pkg in
      sprintf
        "%s.%s%s-%s"
        (Package.Name.to_string name)
        base_version
        platform
        (Dune_digest.to_string build_id))
    else
      (* Non-relocatable: use build_id which includes opam content + deps *)
      sprintf
        "%s.%s-%s"
        (Package.Name.to_string name)
        (Package_version.to_string pkg.info.version)
        (Dune_digest.to_string build_id)
  in
  Path.Outside_build_dir.relative (base_dir ()) dir_name
;;

let installation_prefix pkg ~build_id =
  let pkg_dir = pkg_dir pkg ~build_id in
  Path.Outside_build_dir.relative pkg_dir "target"
;;

let is_compiler_and_toolchains_enabled name =
  match Config.get Compile_time.toolchains with
  | `Enabled -> Dune_pkg.Dev_tool.is_compiler_package name
  | `Disabled -> false
;;

let install_roots ~prefix =
  Install.Roots.make prefix ~relative:Path.Outside_build_dir.relative
;;

(* Check if a toolchain is already installed in the cache.
   We check for the presence of the install cookie file which indicates
   a successful installation. *)
let is_installed pkg ~build_id =
  let prefix = installation_prefix pkg ~build_id in
  let cookie_path =
    Path.outside_build_dir (Path.Outside_build_dir.relative prefix "cookie")
  in
  Path.Untracked.exists cookie_path
;;

(* Get the cache directory path for a toolchain package *)
let cache_dir pkg ~build_id = Path.outside_build_dir (pkg_dir pkg ~build_id)

(* Populate the shared install directory from global cache.
   Copies the cached target/ contents to install_dir.
   Also creates target_dir and copies the cookie there for dependency tracking.

   IMPORTANT: The action runs in the sandbox's source/ directory. We use paths
   relative to that location (../target, ../../../install/default) so the sandbox
   can properly manage the directory targets. Using absolute paths would bypass
   the sandbox and cause "target already exists" errors. *)
let populate_from_cache_action pkg ~build_id ~install_dir ~target_dir:_ =
  let cache_target = Path.outside_build_dir (installation_prefix pkg ~build_id) in
  let cache_target_str = Path.to_string cache_target in
  (* The action runs in source/, so target/ is ../target relative to that.
     install_dir needs the full path since it's outside the package directory. *)
  let target_dir_rel = "../target" in
  let install_dir_str = Path.Build.to_string install_dir in
  let cache_cookie = Filename.concat cache_target_str "cookie" in
  let target_cookie = Filename.concat target_dir_rel "cookie" in
  let cmd =
    sprintf
      "mkdir -p %s && mkdir -p %s && cp -a %s/* %s/ && cp %s %s"
      (Filename.quote target_dir_rel)
      (Filename.quote install_dir_str)
      (Filename.quote cache_target_str)
      (Filename.quote install_dir_str)
      (Filename.quote cache_cookie)
      (Filename.quote target_cookie)
  in
  Dune_lang.Action.System (String_with_vars.make_text Loc.none cmd)
;;
