open Import

type t = OpamTypes.filtered_formula

let of_dependencies deps =
  List.map deps ~f:Package_dependency.to_opam_filtered_formula |> OpamFormula.ands
;;

let to_filtered_formula v = v
let of_filtered_formula v = v
let to_dyn = Opam_dyn.filtered_formula
let ands = OpamFormula.ands

let remove_packages (v : OpamTypes.filtered_formula) pkgs =
  OpamFormula.map_up_formula
    (function
      | Atom (name, _condition) as a ->
        if
          let name = Package_name.of_opam_package_name name in
          Package_name.Set.mem pkgs name
        then Empty
        else a
      | x -> x)
    v
;;

exception Found of Package_name.t

let any_package_name (v : OpamTypes.filtered_formula) =
  try
    OpamFormula.iter
      (fun (name, _condition) ->
         let name = Package_name.of_opam_package_name name in
         raise_notrace (Found name))
      v;
    None
  with
  | Found name -> Some name
;;

let has_entries v = v |> any_package_name |> Option.is_some

let all_package_names (v : OpamTypes.filtered_formula) =
  let names = ref Package_name.Set.empty in
  OpamFormula.iter
    (fun (name, _condition) ->
       let name = Package_name.of_opam_package_name name in
       names := Package_name.Set.add !names name)
    v;
  !names
;;

(* Check if a filter contains a reference to with-doc variable.
   These are dev tool dependencies (like odoc) that are handled separately. *)
let rec filter_is_dev_tool (filter : OpamTypes.filter) =
  match filter with
  | FIdent ([], var, None) ->
    let var_str = OpamVariable.to_string var in
    String.equal var_str "with-doc"
  | FAnd (f1, f2) | FOr (f1, f2) -> filter_is_dev_tool f1 || filter_is_dev_tool f2
  | FNot f -> filter_is_dev_tool f
  | FDefined f -> filter_is_dev_tool f
  | FUndef f -> filter_is_dev_tool f
  | _ -> false
;;

(* Check if a condition formula contains a with-doc variable anywhere *)
let rec condition_is_dev_tool (cond : OpamTypes.condition) =
  match cond with
  | Empty -> false
  | Atom (Filter f) -> filter_is_dev_tool f
  | Atom (Constraint _) -> false
  | Block c -> condition_is_dev_tool c
  | And (c1, c2) | Or (c1, c2) -> condition_is_dev_tool c1 || condition_is_dev_tool c2
;;

(* Extract package names that are NOT guarded by with-doc filters.
   Includes with-test deps (needed for dune test), excludes doc tools (odoc). *)
let runtime_package_names (v : OpamTypes.filtered_formula) =
  let names = ref Package_name.Set.empty in
  OpamFormula.iter
    (fun (name, condition) ->
       let is_dev_tool = condition_is_dev_tool condition in
       if not is_dev_tool
       then (
         let name = Package_name.of_opam_package_name name in
         names := Package_name.Set.add !names name))
    v;
  !names
;;
