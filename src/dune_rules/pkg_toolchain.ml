open Import

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
let platform_suffix (pkg : Dune_pkg.Lock.Pkg.t) =
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

let pkg_dir (pkg : Dune_pkg.Lock.Pkg.t) =
  (* The name of this package's directory within the toolchains directory.

     For relocatable-compiler packages, we use the base OCaml version plus
     a platform hash. This enables cache sharing across projects while
     ensuring different platforms (for cross-compilation) get separate entries.

     For other packages (including non-relocatable compilers), we include a
     hash of the package's fields so that modified lockfiles produce different
     cache entries. *)
  let dir_name =
    let name = pkg.info.name in
    if is_relocatable_compiler name
    then (
      (* Relocatable compiler: use base OCaml version + platform for cache sharing *)
      let base_version =
        relocatable_base_version pkg.info.version
        |> Option.value ~default:(Package_version.to_string pkg.info.version)
      in
      let platform = platform_suffix pkg in
      sprintf "%s.%s%s" (Package.Name.to_string name) base_version platform)
    else (
      (* Non-relocatable: include hash to ensure correctness *)
      (* TODO should include resolved deps *)
      let pkg_digest =
        Dune_digest.Feed.compute_digest
          Lock_dir.Pkg.digest_feed
          (Lock_dir.Pkg.remove_locs pkg)
      in
      sprintf
        "%s.%s-%s"
        (Package.Name.to_string name)
        (Package_version.to_string pkg.info.version)
        (Dune_digest.to_string pkg_digest))
  in
  Path.Outside_build_dir.relative (base_dir ()) dir_name
;;

let installation_prefix pkg =
  let pkg_dir = pkg_dir pkg in
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
let is_installed pkg =
  let prefix = installation_prefix pkg in
  let cookie_path =
    Path.outside_build_dir (Path.Outside_build_dir.relative prefix "cookie")
  in
  Path.Untracked.exists cookie_path
;;
