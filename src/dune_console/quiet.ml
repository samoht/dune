module M : Backend_intf.S = struct
  let start () = ()
  let finish () = ()
  let print_user_message _ = ()
  let set_status_line _ = ()
  let print_if_no_status_line _ = ()
  let reset () = ()
  let reset_flush_history () = ()

  (* Event-driven API - quiet backend ignores everything except errors *)
  let set_total _ = ()
  let activity_start ~stage:_ ~name:_ ~tool:_ = ()
  let activity_finish ~name:_ = ()
  let activity_fail ~name:_ = ()
  let activity_log ~name:_ _ = ()
  let message _ = ()

  let error msg =
    Dumb.print_msg msg;
    flush stderr
  ;;

  let info _ = ()
  let verbose _ = ()
end

let backend = (module M : Backend_intf.S)
