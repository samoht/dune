open Import

type package_target =
  | Duniverse
  | Opam_sandbox

let package_target_to_dyn = function
  | Duniverse -> Dyn.variant "Duniverse" []
  | Opam_sandbox -> Dyn.variant "Opam_sandbox" []
;;

let marker_filename = ".dune-duniverse"
let duniverse_dir = Path.Source.of_string "duniverse"

let package_dir name version =
  let dirname =
    sprintf "%s.%s" (Package_name.to_string name) (Package_version.to_string version)
  in
  Path.Source.relative duniverse_dir dirname
;;

(* Check if a Slang expression is a literal "dune" *)
let is_dune_command = function
  | Slang.Literal sw ->
    (match String_with_vars.text_only sw with
     | Some "dune" -> true
     | _ -> false)
  | _ -> false
;;

(* Recursively check if an action uses dune as the build command *)
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

let classify (pkg : Lock_dir.Pkg.t) =
  (* A package goes to duniverse if it uses dune as its build system.
     We check if the build command is [Dune] or contains a dune action for any platform. *)
  let uses_dune =
    Lock_dir.Conditional_choice.exists pkg.build_command ~f:(function
      | Lock_dir.Build_command.Dune -> true
      | Lock_dir.Build_command.Action action -> action_uses_dune action)
  in
  if uses_dune then Duniverse else Opam_sandbox
;;

let classify_all pkgs = Package_name.Map.map pkgs ~f:classify
