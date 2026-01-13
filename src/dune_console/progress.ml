open Stdune

let get_terminal_width () =
  match Sys.getenv_opt "COLUMNS" with
  | Some s ->
    (match int_of_string_opt s with
     | Some n when n > 10 -> n
     | _ -> 80)
  | None -> 80
;;

let truncate_to_width s max_width =
  let len = String.length s in
  if len <= max_width
  then s
  else if max_width <= 3
  then String.sub s ~pos:0 ~len:max_width
  else String.sub s ~pos:0 ~len:(max_width - 3) ^ "..."
;;

type activity =
  { name : string
  ; stage : Backend_intf.stage
  ; tool : string option
  }

type state =
  { mutable active : activity list
  ; mutable completed : int
  ; mutable total : int
  ; mutable failed : int
  }

let state = { active = []; completed = 0; total = 0; failed = 0 }

let reset_state () =
  state.active <- [];
  state.completed <- 0;
  state.total <- 0;
  state.failed <- 0
;;

(* Format: "[x/y] Fetching Foo [curl], Building Bar [opam]" *)
let format_status_line max_width =
  if List.is_empty state.active && state.total = 0
  then ""
  else (
    let prefix =
      if state.total > 0
      then (
        let width = String.length (string_of_int state.total) in
        Printf.sprintf "[%0*d/%d] " width state.completed state.total)
      else ""
    in
    let prefix_len = String.length prefix in
    let available = max_width - prefix_len in
    let format_activity { name; stage; tool } =
      let stage_str = Backend_intf.stage_to_string stage in
      match tool with
      | None -> Printf.sprintf "%s %s" stage_str name
      | Some t -> Printf.sprintf "%s %s [%s]" stage_str name t
    in
    let rec fit_activities acc len = function
      | [] -> List.rev acc, 0
      | act :: rest ->
        let s = format_activity act in
        let s_len = String.length s in
        let sep_len = if List.is_empty acc then 0 else 2 in
        let new_len = len + sep_len + s_len in
        if new_len > available && not (List.is_empty acc)
        then List.rev acc, List.length rest + 1
        else fit_activities (s :: acc) new_len rest
    in
    let shown, remaining = fit_activities [] 0 state.active in
    let activities_str =
      if List.is_empty shown
      then if remaining > 0 then Printf.sprintf "+%d targets" remaining else "Waiting..."
      else (
        let base = String.concat ~sep:", " shown in
        if remaining > 0 then Printf.sprintf "%s, +%d more" base remaining else base)
    in
    prefix ^ activities_str)
;;

module No_flush = struct
  let status_line = ref Pp.nop
  let status_line_len = ref 0

  let hide_status_line () =
    if !status_line_len > 0
    then (
      Printf.eprintf "\r%*s\r" !status_line_len "";
      flush stderr)
  ;;

  let show_status_line () =
    if !status_line_len > 0
    then (
      Ansi_color.prerr !status_line;
      flush stderr)
  ;;

  let update_status_line () =
    let term_width = get_terminal_width () in
    let line_str = format_status_line term_width in
    let line_str = truncate_to_width line_str term_width in
    let line_len = String.length line_str in
    let old_str = Format.asprintf "%a" Pp.to_fmt !status_line in
    if not (String.equal line_str old_str)
    then (
      hide_status_line ();
      status_line := Pp.verbatim line_str;
      status_line_len := line_len;
      show_status_line ())
  ;;

  let start () = reset_state ()
  let finish () = hide_status_line ()

  let set_status_line = function
    | None ->
      hide_status_line ();
      status_line := Pp.nop;
      status_line_len := 0
    | Some line ->
      let line = Pp.map_tags line ~f:User_message.Print_config.default in
      let line_str = Format.asprintf "%a" Pp.to_fmt line in
      let term_width = get_terminal_width () in
      let line_str = truncate_to_width line_str term_width in
      let line_len = String.length line_str in
      let old_str = Format.asprintf "%a" Pp.to_fmt !status_line in
      let old_str = truncate_to_width old_str term_width in
      if not (String.equal line_str old_str)
      then (
        hide_status_line ();
        status_line := Pp.verbatim line_str;
        status_line_len := line_len;
        show_status_line ())
  ;;

  let print_if_no_status_line _msg = ()

  let print_user_message msg =
    hide_status_line ();
    Dumb.No_flush.print_user_message msg;
    show_status_line ()
  ;;

  let reset () =
    reset_state ();
    Dumb.reset ()
  ;;

  let reset_flush_history () =
    reset_state ();
    Dumb.reset_flush_history ()
  ;;

  (* Event-driven API *)
  let set_total n =
    state.total <- n;
    update_status_line ()
  ;;

  let activity_start ~stage ~name ~tool =
    state.active <- { name; stage; tool } :: state.active;
    update_status_line ()
  ;;

  let activity_finish ~name =
    state.active <- List.filter state.active ~f:(fun a -> not (String.equal a.name name));
    state.completed <- state.completed + 1;
    update_status_line ()
  ;;

  let activity_fail ~name =
    state.active <- List.filter state.active ~f:(fun a -> not (String.equal a.name name));
    state.failed <- state.failed + 1;
    update_status_line ()
  ;;

  let activity_log ~name:_ _msg = ()

  let message msg =
    hide_status_line ();
    Dumb.No_flush.print_user_message msg;
    show_status_line ()
  ;;

  let error msg =
    hide_status_line ();
    Dumb.No_flush.print_user_message msg;
    show_status_line ()
  ;;

  let info msg =
    hide_status_line ();
    Printf.eprintf "%s\n%!" msg;
    show_status_line ()
  ;;

  let verbose _ = ()
end

let no_flush = (module No_flush : Backend_intf.S)
let flush = Combinators.flush no_flush
