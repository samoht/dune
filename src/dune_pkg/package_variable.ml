open Import

module Scope = struct
  type t =
    | Self
    | Package of Package_name.t

  let compare x y =
    match x, y with
    | Self, Self -> Eq
    | Self, Package _ -> Gt
    | Package _, Self -> Lt
    | Package x, Package y -> Package_name.compare x y
  ;;

  let to_dyn = function
    | Self -> Dyn.variant "Self" []
    | Package name -> Dyn.variant "Package" [ Package_name.to_dyn name ]
  ;;
end

module T = struct
  type t =
    { name : Package_variable_name.t
    ; scope : Scope.t
    ; default_if_true : string option
    }

  let compare t { name; scope; default_if_true } =
    match Scope.compare t.scope scope with
    | Eq ->
      (match Package_variable_name.compare t.name name with
       | Eq -> Option.compare String.compare t.default_if_true default_if_true
       | x -> x)
    | x -> x
  ;;

  let to_dyn { name; scope; default_if_true } =
    let open Dyn in
    record
      [ "name", Package_variable_name.to_dyn name
      ; "scope", Scope.to_dyn scope
      ; "default_if_true", option string default_if_true
      ]
  ;;
end

include T
module C = Comparable.Make (T)
include C

(* Parse opam syntax [var?string] into variable name and optional default.
   [installed?system] means "if installed is true, return 'system', else empty" *)
let parse_variable_with_default variable_str =
  match String.lsplit2 variable_str ~on:'?' with
  | None -> Package_variable_name.of_string variable_str, None
  | Some (name, default) -> Package_variable_name.of_string name, Some default
;;

let of_macro_invocation ~loc ({ Pform.Macro_invocation.macro; _ } as macro_invocation) =
  match macro with
  | Pkg_self ->
    let variable_str = Pform.Macro_invocation.Args.whole macro_invocation in
    let name, default_if_true = parse_variable_with_default variable_str in
    Ok { name; scope = Self; default_if_true }
  | Pkg ->
    let package_name, variable_str =
      Pform.Macro_invocation.Args.lsplit2_exn macro_invocation loc
    in
    let name, default_if_true = parse_variable_with_default variable_str in
    Ok { name; scope = Package (Package_name.of_string package_name); default_if_true }
  | _ -> Error `Unexpected_macro
;;

let to_macro_invocation { name; scope; default_if_true } =
  let var_str =
    let base = Package_variable_name.to_string name in
    match default_if_true with
    | None -> base
    | Some default -> base ^ "?" ^ default
  in
  match scope with
  | Self ->
    { Pform.Macro_invocation.macro = Pkg_self
    ; payload = Pform.Payload.of_args [ var_str ]
    }
  | Package package_name ->
    { Pform.Macro_invocation.macro = Pkg
    ; payload = Pform.Payload.of_args [ Package_name.to_string package_name; var_str ]
    }
;;

let to_pform t = Pform.Macro (to_macro_invocation t)
