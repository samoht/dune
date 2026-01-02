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

let classify (pkg : Lock_dir.Pkg.t) =
  (* A package goes to duniverse if it uses dune as its build system.
     We check if the build command is [Dune] for any platform. *)
  let uses_dune =
    Lock_dir.Conditional_choice.exists pkg.build_command ~f:(function
      | Lock_dir.Build_command.Dune -> true
      | Lock_dir.Build_command.Action _ -> false)
  in
  if uses_dune then Duniverse else Opam_sandbox
;;

let classify_all pkgs = Package_name.Map.map pkgs ~f:classify
