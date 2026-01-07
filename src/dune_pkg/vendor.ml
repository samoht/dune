open Import

(** How a vendored package should be built.

    - [Dune_native]: Built as vendored code in the main dune context.
      Libraries are directly available to the build system.
    - [Opam_sandboxed]: Built in an isolated opam-style sandbox using
      the package's opam build commands. *)
type build_method =
  | Dune_native
  | Opam_sandboxed

let build_method_to_dyn = function
  | Dune_native -> Dyn.variant "Dune_native" []
  | Opam_sandboxed -> Dyn.variant "Opam_sandboxed" []
;;

let default_dir_name = "duniverse"
let default_dir = Path.Source.of_string default_dir_name
let marker_filename = "dune"

let package_dir name version =
  let dirname =
    sprintf "%s.%s" (Package_name.to_string name) (Package_version.to_string version)
  in
  Path.Source.relative default_dir dirname
;;

(* Source comparison that ignores location information *)
module Source_key = struct
  type t =
    { url : string
    ; checksum : Checksum.t option
    }

  let of_source (source : Source.t) =
    let _, url = source.url in
    let checksum = Option.map source.checksum ~f:snd in
    { url = OpamUrl.to_string url; checksum }
  ;;

  let compare a b =
    match String.compare a.url b.url with
    | (Lt | Gt) as x -> x
    | Eq ->
      Option.compare
        (fun x y -> String.compare (Checksum.to_string x) (Checksum.to_string y))
        a.checksum
        b.checksum
  ;;

  let to_dyn { url; checksum } =
    Dyn.record [ "url", Dyn.string url; "checksum", Dyn.option Checksum.to_dyn checksum ]
  ;;
end

module Source_key_map = Map.Make (Source_key)

type source_group =
  { packages : (Package_name.t * Package_version.t) list
  ; source : Source.t
  ; primary_name : Package_name.t
  ; primary_version : Package_version.t
  }

let group_by_source pkgs =
  let groups =
    List.fold_left pkgs ~init:Source_key_map.empty ~f:(fun acc pkg ->
      let { Lock.Pkg_info.name; version; source; _ } = pkg.Lock.Pkg.info in
      match source with
      | None -> acc
      | Some source ->
        let key = Source_key.of_source source in
        let entry = name, version, source, pkg in
        Source_key_map.update acc key ~f:(function
          | None -> Some [ entry ]
          | Some entries -> Some (entry :: entries)))
  in
  Source_key_map.values groups
  |> List.map ~f:(fun entries ->
    let sorted =
      List.sort entries ~compare:(fun (n1, _, _, _) (n2, _, _, _) ->
        let s1 = Package_name.to_string n1 in
        let s2 = Package_name.to_string n2 in
        match Int.compare (String.length s1) (String.length s2) with
        | (Lt | Gt) as x -> x
        | Eq -> Package_name.compare n1 n2)
    in
    match sorted with
    | [] -> assert false
    | (primary_name, primary_version, source, _) :: _ ->
      let packages = List.map sorted ~f:(fun (n, v, _, _) -> n, v) in
      { packages; source; primary_name; primary_version })
;;

let source_group_dir group = package_dir group.primary_name group.primary_version
let source_group_packages group = group.packages

let is_dune_command = function
  | Slang.Literal sw ->
    (match String_with_vars.text_only sw with
     | Some "dune" -> true
     | _ -> false)
  | _ -> false
;;

let rec action_uses_dune (action : Action.t) =
  match action with
  | Run (cmd :: _) -> is_dune_command cmd
  | Progn actions | Concurrent actions -> List.exists actions ~f:action_uses_dune
  | Chdir (_, action)
  | Setenv (_, _, action)
  | Redirect_out (_, _, _, action)
  | Redirect_in (_, _, action)
  | Ignore (_, action)
  | No_infer action
  | With_accepted_exit_codes (_, action)
  | Withenv (_, action)
  | When (_, action) -> action_uses_dune action
  | Pipe (_, actions) -> List.exists actions ~f:action_uses_dune
  | Run []
  | Dynamic_run _
  | Echo _
  | Cat _
  | Copy _
  | Symlink _
  | Copy_and_add_line_directive _
  | System _
  | Bash _
  | Write_file _
  | Mkdir _
  | Diff _
  | Cram _
  | Patch _
  | Substitute _
  | Format_dune_file _ -> false
;;

let classify_build_method (pkg : Lock.Pkg.t) =
  let uses_dune =
    Lock.Conditional_choice.exists pkg.build_command ~f:(function
      | Lock.Build_command.Dune -> true
      | Lock.Build_command.Action action -> action_uses_dune action)
  in
  if uses_dune then Dune_native else Opam_sandboxed
;;

let classify_build_method_all pkgs = Package_name.Map.map pkgs ~f:classify_build_method

let rec extract_patches_from_action (action : Action.t) =
  match action with
  | Patch sw -> [ sw ]
  | Progn actions -> List.concat_map actions ~f:extract_patches_from_action
  | Concurrent actions -> List.concat_map actions ~f:extract_patches_from_action
  | Chdir (_, action)
  | Setenv (_, _, action)
  | Redirect_out (_, _, _, action)
  | Redirect_in (_, _, action)
  | Ignore (_, action)
  | No_infer action
  | With_accepted_exit_codes (_, action)
  | Withenv (_, action)
  | When (_, action) -> extract_patches_from_action action
  | Pipe (_, actions) -> List.concat_map actions ~f:extract_patches_from_action
  | Run _
  | Dynamic_run _
  | Echo _
  | Cat _
  | Copy _
  | Symlink _
  | Copy_and_add_line_directive _
  | System _
  | Bash _
  | Write_file _
  | Mkdir _
  | Diff _
  | Cram _
  | Substitute _
  | Format_dune_file _ -> []
;;

let get_patches (pkg : Lock.Pkg.t) ~platform =
  match Lock.Conditional_choice.choose_for_platform pkg.build_command ~platform with
  | None -> []
  | Some Lock.Build_command.Dune -> []
  | Some (Lock.Build_command.Action action) -> extract_patches_from_action action
;;
