open Stdune

let print_msg msg =
  Option.iter msg.User_message.loc ~f:(fun loc ->
    Loc.render Format.err_formatter (Loc.pp loc));
  User_message.prerr { msg with loc = None }
;;

module No_flush : Backend_intf.S = struct
  let start () = ()
  let finish () = ()
  let print_user_message = print_msg
  let set_status_line _ = ()

  let print_if_no_status_line msg =
    Ansi_color.prerr
      (Pp.seq (Pp.map_tags msg ~f:User_message.Print_config.default) Pp.cut)
  ;;

  let reset () = prerr_string "\x1b[H\x1b[2J"
  let reset_flush_history () = prerr_string "\x1b[1;1H\x1b[2J\x1b[3J"

  (* Event-driven API - dumb backend ignores activity tracking *)
  let set_total _ = ()
  let activity_start ~stage:_ ~name:_ ~tool:_ = ()
  let activity_finish ~name:_ = ()
  let activity_fail ~name:_ = ()
  let activity_log ~name:_ _ = ()
  let message = print_msg
  let error = print_msg
end

let flush = Combinators.flush (module No_flush)

include (val flush)
