open Import

module Status = struct
  type t =
    [ `Downloading
    | `Building
    ]

  let to_string = function
    | `Downloading -> "Downloading"
    | `Building -> "Building"
  ;;
end

(* Progress tracking for package builds *)
module Progress = struct
  type t =
    { mutable total : int
    ; mutable completed : int
    }

  let state = { total = 0; completed = 0 }
  let set_total n = state.total <- n

  let reset () =
    state.total <- 0;
    state.completed <- 0
  ;;

  let increment () =
    state.completed <- state.completed + 1;
    state.completed
  ;;

  let format () = if state.total > 0 then Some (state.completed, state.total) else None
end

let format_user_message ~verb ~object_ =
  let status_tag = User_message.Style.Ok in
  User_message.make
    [ User_message.aligned_message ~left:(status_tag, verb) ~right:object_ ]
;;

module Message = struct
  type t =
    { package_name : Package.Name.t
    ; package_version : Package_version.t
    ; status : Status.t
    }

  let user_message { package_name; package_version; status } =
    let pkg_str =
      sprintf
        "%s.%s"
        (Package.Name.to_string package_name)
        (Package_version.to_string package_version)
    in
    let object_ =
      match Progress.format () with
      | None -> Pp.text pkg_str
      | Some (current, total) -> Pp.textf "%s (%d/%d)" pkg_str current total
    in
    format_user_message ~verb:(Status.to_string status) ~object_
  ;;

  let display t =
    let pkg_str =
      sprintf
        "%s.%s"
        (Package.Name.to_string t.package_name)
        (Package_version.to_string t.package_version)
    in
    (match t.status with
     | `Building ->
       ignore (Progress.increment () : int);
       Dune_engine.Progress.start_target (Dune_engine.Progress.Target.package pkg_str)
     | `Downloading ->
       Dune_engine.Progress.start_target (Dune_engine.Progress.Target.fetch pkg_str));
    match !Dune_engine.Clflags.display with
    | Quiet -> ()
    | Short | Verbose -> Console.print_user_message (user_message t)
  ;;

  let encode { package_name; package_version; status } =
    Sexp.List
      [ Sexp.Atom (Package.Name.to_string package_name)
      ; Sexp.Atom (Package_version.to_string package_version)
      ; Sexp.Atom (Status.to_string status)
      ]
  ;;
end

module Spec = struct
  type ('path, 'target) t = Message.t

  let name = "progress-action"
  let version = 1
  let is_useful_to ~memoize:_ = true
  let bimap t _f _g = t
  let encode t _ _ = Message.encode t

  let action t ~ectx:_ ~eenv:_ =
    let open Fiber.O in
    let+ () = Fiber.return () in
    Message.display t
  ;;
end

module Action = Action_ext.Make (Spec)

let progress_action package_name package_version status =
  Action.action { Message.package_name; package_version; status }
;;
