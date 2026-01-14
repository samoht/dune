open Import
module Package_variable = Dune_pkg.Package_variable

(** Opam variable expansion for package builds.

    This module provides the canonical implementation for expanding opam variables
    like %{prefix}%, %{lib}%, %{pkg:installed}%, etc. *)

module Variable = struct
  type value = OpamVariable.variable_contents =
    | B of bool
    | S of string
    | L of string list

  type t = Package_variable_name.t * value

  let dyn_of_value : value -> Dyn.t =
    let open Dyn in
    function
    | B b -> variant "Bool" [ bool b ]
    | S s -> variant "String" [ string s ]
    | L xs -> variant "Strings" [ list string xs ]
  ;;

  let dune_value : value -> Value.t list = function
    | B b -> [ String (Bool.to_string b) ]
    | S s -> [ String s ]
    | L s -> List.map s ~f:(fun x -> Value.String x)
  ;;

  let of_values : dir:Path.t -> Value.t list -> value =
    fun ~dir xs ->
    match List.map xs ~f:(Value.to_string ~dir) with
    | [ x ] -> S x
    | xs -> L xs
  ;;

  let to_dyn (name, value) =
    Dyn.(pair Package_variable_name.to_dyn dyn_of_value (name, value))
  ;;
end

(** Shared install directory helpers *)
module Pkg_install = struct
  let dir ~context = Install.Context.dir ~context

  let roots_build ~context =
    Install.Roots.opam_from_prefix ~relative:Path.Build.relative (dir ~context)
  ;;

  let roots ~context = roots_build ~context |> Install.Roots.map ~f:Path.build

  (* Compiler packages store their libraries in a subdirectory named "ocaml" *)
  let roots_for_package ~pkg_name ~context =
    let base_roots = roots ~context in
    match Pkg_cache.Toolchain.is_compiler_and_toolchains_enabled pkg_name with
    | false -> base_roots
    | true -> { base_roots with lib_root = Path.relative base_roots.lib_root "ocaml" }
  ;;
end

(** Convert Pform section to Install section *)
let pform_section_to_install_section (section : Pform.Var.Pkg.Section.t) : Section.t =
  match section with
  | Lib -> Lib
  | Libexec -> Libexec
  | Bin -> Bin
  | Sbin -> Sbin
  | Share -> Share
  | Etc -> Etc
  | Doc -> Doc
  | Man -> Man
  | Toplevel -> Toplevel
  | Stublibs -> Stublibs
;;

(** Map install sections to directories *)
let section_dir_of_root (roots : _ Install.Roots.t) (section : Pform.Var.Pkg.Section.t) =
  match section with
  | Lib -> roots.lib_root
  | Libexec -> roots.libexec_root
  | Bin -> roots.bin
  | Sbin -> roots.sbin
  | Share -> roots.share_root
  | Etc -> roots.etc_root
  | Doc -> roots.doc_root
  | Man -> roots.man
  | Toplevel -> Path.relative roots.lib_root "toplevel"
  | Stublibs -> Path.relative roots.lib_root "stublibs"
;;

(** Poll system variables from Lock_dir.Sys_vars *)
let sys_poll_var accessor =
  let open Memo.O in
  accessor Lock_dir.Sys_vars.poll
  |> Memo.Lazy.force
  >>| function
  | Some v -> [ Value.String v ]
  | None ->
    (* In OPAM an unset variable evaluates to false, but we
       can't represent that in a string so it evaluates to an empty string *)
    [ Value.String "" ]
;;

(** Expand package-level pform variables like %{prefix}%, %{lib}%, %{jobs}%, etc. *)
let expand_pkg ~context ~source_dir ~prefix (pform : Pform.Var.Pkg.t) =
  match pform with
  | Switch -> Memo.return [ Value.String (Context_name.to_string context) ]
  | Os Os -> sys_poll_var (fun { os; _ } -> os)
  | Os Os_version -> sys_poll_var (fun { os_version; _ } -> os_version)
  | Os Os_distribution -> sys_poll_var (fun { os_distribution; _ } -> os_distribution)
  | Os Os_family -> sys_poll_var (fun { os_family; _ } -> os_family)
  | Sys_ocaml_version -> sys_poll_var (fun { sys_ocaml_version; _ } -> sys_ocaml_version)
  | Build -> Memo.return [ Value.Dir source_dir ]
  | Prefix ->
    (* Use the package's prefix (may be toolchain cache dir or shared install dir) *)
    Memo.return [ Value.Dir prefix ]
  | User -> Memo.return [ Value.String (Unix.getlogin ()) ]
  | Jobs -> Memo.return [ Value.String (Int.to_string !Clflags.concurrency) ]
  | Arch -> sys_poll_var (fun { arch; _ } -> arch)
  | Group ->
    let group = Unix.getgid () |> Unix.getgrgid in
    Memo.return [ Value.String group.gr_name ]
  | Section_dir section ->
    (* Derive roots from prefix for consistency with %{prefix}%.
       This ensures %{lib}%, %{share}%, etc. are relative to the same prefix. *)
    let roots = Install.Roots.opam_from_prefix ~relative:Path.relative prefix in
    let dir = section_dir_of_root roots section in
    Memo.return [ Value.Dir dir ]
  | Name | Version ->
    (* Name and Version need to be handled by the caller with access to package info *)
    Code_error.raise "Name and Version should be handled by caller" []
;;

(** Resolve builtin package variables that don't come from the package itself *)
let resolve_builtin_var
      ~context
      ~package_name
      ~all_versions
      ~present
      ~scope
      ~self_build_id
      ~build_ids
      ~dep_install_paths
      variable_name
  =
  match Package_variable_name.to_string variable_name with
  | "pinned" -> Some (Memo.return @@ Ok [ Value.false_ ])
  | "preinstalled" -> Some (Memo.return @@ Ok [ Value.false_ ])
  | "native" -> Some (Memo.return @@ Ok [ Value.true_ ])
  | "enable" ->
    Some (Memo.return @@ Ok [ Value.String (if present then "enable" else "disable") ])
  | "installed" ->
    let in_lock = Package.Name.Map.mem all_versions package_name in
    Some (Memo.return @@ Ok [ Value.String (Bool.to_string in_lock) ])
  | "version" ->
    (match Package.Name.Map.find all_versions package_name with
     | Some version ->
       Some (Memo.return @@ Ok [ Value.String (Package_version.to_string version) ])
     | None -> Some (Memo.return @@ Ok [ Value.String "" ]))
  | "build-id" ->
    let build_id =
      match scope with
      | Package_variable.Scope.Self -> Some self_build_id
      | Package_variable.Scope.Package pkg -> Package.Name.Map.find build_ids pkg
    in
    let build_id_str =
      match build_id with
      | Some digest -> Dune_digest.to_string digest
      | None -> ""
    in
    Some (Memo.return @@ Ok [ Value.String build_id_str ])
  | _ ->
    (* Try section directory *)
    (match Package.Name.Map.mem build_ids package_name with
     | false -> None
     | true ->
       (match
          Pform.Var.Pkg.Section.of_string (Package_variable_name.to_string variable_name)
        with
        | None -> None
        | Some section ->
          (* Use package-specific paths if available, otherwise fall back to shared roots *)
          let dir =
            match dep_install_paths with
            | Some paths ->
              Install.Paths.get paths (pform_section_to_install_section section)
            | None ->
              let roots = Pkg_install.roots_for_package ~pkg_name:package_name ~context in
              section_dir_of_root roots section
          in
          Some (Memo.return @@ Ok [ Value.Dir dir ])))
;;

(** Apply opam's var?default semantics: if var is truthy, return default, else "" *)
let apply_default_if_true default_if_true result =
  match default_if_true with
  | None -> result
  | Some default ->
    Result.map result ~f:(fun values ->
      let is_truthy =
        match values with
        | [ Value.String "true" ] -> true
        | [ Value.String "false" ] | [ Value.String "" ] | [] -> false
        | _ -> true
      in
      if is_truthy then [ Value.String default ] else [ Value.String "" ])
;;

(** Expand a bare identifier (CIdent in opam commands).
    Returns None if the variable is not recognized. *)
let expand_ident ~context ~pkg_name ~pkg_version ~prefix ~ocamlfind_destdir var =
  let roots = Pkg_install.roots ~context in
  let pkg_name_str = Package.Name.to_string pkg_name in
  let pkg_path_var section =
    match section with
    | "lib" -> Some (Path.to_string (Path.relative ocamlfind_destdir pkg_name_str))
    | "share" -> Some (Path.to_string (Path.relative roots.share_root pkg_name_str))
    | "doc" -> Some (Path.to_string (Path.relative roots.doc_root pkg_name_str))
    | "etc" -> Some (Path.to_string (Path.relative roots.etc_root pkg_name_str))
    | _ -> None
  in
  let system_path = Global.env () |> Env_path.path in
  match var with
  | "prefix" -> Some (Path.to_string prefix)
  | "lib" -> Some (Path.to_string ocamlfind_destdir)
  | "bin" -> Some (Path.to_string roots.bin)
  | "sbin" -> Some (Path.to_string roots.sbin)
  | "share" -> Some (Path.to_string roots.share_root)
  | "doc" -> Some (Path.to_string roots.doc_root)
  | "etc" -> Some (Path.to_string roots.etc_root)
  | "man" -> Some (Path.to_string roots.man)
  | "stublibs" -> Some (Path.to_string (Path.relative roots.lib_root "stublibs"))
  | "switch" -> Some (Context_name.to_string context)
  | "name" -> Some pkg_name_str
  | "version" -> Some (Package_version.to_string pkg_version)
  | "jobs" -> Some (Int.to_string !Clflags.concurrency)
  | "make" ->
    (match Bin.which ~path:system_path "gmake" with
     | Some _ -> Some "gmake"
     | None -> Some "make")
  | "_:name" -> Some pkg_name_str
  | "_:lib" -> pkg_path_var "lib"
  | "_:share" -> pkg_path_var "share"
  | "_:doc" -> pkg_path_var "doc"
  | "_:etc" -> pkg_path_var "etc"
  | _ -> None
;;

(** String-based variable expansion for opam commands.
    Expands %{var}% and %{pkg:var}% patterns in a string. *)
let expand_string
      ~context
      ~pkg_name
      ~pkg_version
      ~all_packages
      ~prefix
      ~ocamlfind_destdir
      s
  =
  let roots = Pkg_install.roots ~context in
  let pkg_path_var pkg_str = function
    | "lib" -> Some (Path.to_string (Path.relative ocamlfind_destdir pkg_str))
    | "share" -> Some (Path.to_string (Path.relative roots.share_root pkg_str))
    | "doc" -> Some (Path.to_string (Path.relative roots.doc_root pkg_str))
    | "etc" -> Some (Path.to_string (Path.relative roots.etc_root pkg_str))
    | _ -> None
  in
  let expand_simple_var var =
    expand_ident ~context ~pkg_name ~pkg_version ~prefix ~ocamlfind_destdir var
  in
  let expand_pkg_var pkg_str var =
    let pkg = Package.Name.of_string pkg_str in
    match var with
    | "installed" -> Some (Bool.to_string (Package.Name.Map.mem all_packages pkg))
    | "enable" ->
      Some (if Package.Name.Map.mem all_packages pkg then "enable" else "disable")
    | "version" ->
      (match Package.Name.Map.find all_packages pkg with
       | Some v -> Some (Package_version.to_string v)
       | None -> Some "")
    | "lib" | "share" | "doc" | "etc" -> pkg_path_var pkg_str var
    | _ -> None
  in
  let pkg_var_re = Re.compile (Re.Perl.re {|%\{([^:}]+):([^}]+)\}%|}) in
  let simple_var_re = Re.compile (Re.Perl.re {|%\{([^:}]+)\}%|}) in
  let result =
    let s =
      Re.replace simple_var_re s ~f:(fun group ->
        let var = Re.Group.get group 1 in
        match expand_simple_var var with
        | Some value -> value
        | None -> Re.Group.get group 0)
    in
    Re.replace pkg_var_re s ~f:(fun group ->
      let pkg_str = Re.Group.get group 1 in
      let var = Re.Group.get group 2 in
      match expand_pkg_var pkg_str var with
      | Some value -> value
      | None -> Re.Group.get group 0)
  in
  Memo.return result
;;
