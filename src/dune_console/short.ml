open Stdune

type state =
  { mutable completed : int
  ; mutable total : int
  }

let state = { completed = 0; total = 0 }

let reset_state () =
  state.completed <- 0;
  state.total <- 0
;;

(* Format: "[03/42] Building mylib" *)
let print_activity ~stage ~name ~tool =
  let prefix =
    if state.total > 0
    then (
      let width = String.length (string_of_int state.total) in
      Printf.sprintf "[%0*d/%d] " width state.completed state.total)
    else ""
  in
  let stage_str = Backend_intf.stage_to_string stage in
  let line =
    match tool with
    | None -> Printf.sprintf "%s%s %s" prefix stage_str name
    | Some t -> Printf.sprintf "%s%s %s [%s]" prefix stage_str name t
  in
  Printf.eprintf "%s\n%!" line
;;

module M : Backend_intf.S = struct
  let start () = reset_state ()
  let finish () = ()
  let print_user_message = Dumb.print_msg
  let set_status_line _ = ()
  let print_if_no_status_line _ = ()

  let reset () =
    reset_state ();
    Dumb.reset ()
  ;;

  let reset_flush_history () =
    reset_state ();
    Dumb.reset_flush_history ()
  ;;

  (* Event-driven API *)
  let set_total n = state.total <- n
  let activity_start ~stage ~name ~tool = print_activity ~stage ~name ~tool
  let activity_finish ~name:_ = state.completed <- state.completed + 1
  let activity_fail ~name:_ = state.completed <- state.completed + 1
  let activity_log ~name:_ _ = ()

  let message msg =
    Dumb.print_msg msg;
    flush stderr
  ;;

  let error msg =
    Dumb.print_msg msg;
    flush stderr
  ;;
end

let backend = (module M : Backend_intf.S)
