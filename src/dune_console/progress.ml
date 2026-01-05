open Stdune

(* Get terminal width from $COLUMNS or fallback to 80 *)
let get_terminal_width () =
  match Sys.getenv_opt "COLUMNS" with
  | Some s ->
    (match int_of_string_opt s with
     | Some n when n > 10 -> n
     | _ -> 80)
  | None -> 80
;;

(* Truncate a string to fit within max_width, adding "..." if truncated *)
let truncate_to_width s max_width =
  let len = String.length s in
  if len <= max_width
  then s
  else if max_width <= 3
  then String.sub s ~pos:0 ~len:max_width
  else String.sub s ~pos:0 ~len:(max_width - 3) ^ "..."
;;

module No_flush = struct
  let status_line = ref Pp.nop
  let start () = ()
  let status_line_len = ref 0

  let hide_status_line () =
    if !status_line_len > 0
    then (
      Printf.eprintf "\r%*s\r" !status_line_len "";
      (* Flush stderr to ensure the hide sequence is written before any subsequent
         output. This is important because show_status_line uses Format.err_formatter
         which has a separate buffer from Printf.eprintf. *)
      flush stderr)
  ;;

  let show_status_line () =
    if !status_line_len > 0
    then (
      Ansi_color.prerr !status_line;
      flush stderr)
  ;;

  let set_status_line = function
    | None ->
      hide_status_line ();
      status_line := Pp.nop;
      status_line_len := 0
    | Some line ->
      let line = Pp.map_tags line ~f:User_message.Print_config.default in
      let line_str = Format.asprintf "%a" Pp.to_fmt line in
      (* Truncate to terminal width to avoid line wrapping issues *)
      let term_width = get_terminal_width () in
      let line_str = truncate_to_width line_str term_width in
      let line_len = String.length line_str in
      (* Only update if the content has changed to avoid flickering *)
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

  let reset () = Dumb.reset ()
  let finish () = set_status_line None
  let reset_flush_history () = Dumb.reset_flush_history ()
end

let no_flush = (module No_flush : Backend_intf.S)
let flush = Combinators.flush no_flush
