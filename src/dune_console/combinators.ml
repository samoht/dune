let flush (module Backend : Backend_intf.S) : Backend_intf.t =
  (module struct
    include Backend

    let print_if_no_status_line msg =
      print_if_no_status_line msg;
      flush stderr
    ;;

    let print_user_message msg =
      print_user_message msg;
      flush stderr
    ;;

    let reset () =
      reset ();
      flush stderr
    ;;

    let reset_flush_history () =
      reset_flush_history ();
      flush stderr
    ;;

    let message msg =
      message msg;
      flush stderr
    ;;

    let error msg =
      error msg;
      flush stderr
    ;;

    let warning msg =
      warning msg;
      flush stderr
    ;;

    let info msg =
      info msg;
      flush stderr
    ;;

    let verbose msg =
      verbose msg;
      flush stderr
    ;;
  end : Backend_intf.S)
;;

let compose (module A : Backend_intf.S) (module B : Backend_intf.S)
  : (module Backend_intf.S)
  =
  (module struct
    let start () =
      A.start ();
      B.start ()
    ;;

    let print_user_message msg =
      A.print_user_message msg;
      B.print_user_message msg
    ;;

    let set_status_line x =
      A.set_status_line x;
      B.set_status_line x
    ;;

    let finish () =
      A.finish ();
      B.finish ()
    ;;

    let print_if_no_status_line msg =
      A.print_if_no_status_line msg;
      B.print_if_no_status_line msg
    ;;

    let reset () =
      A.reset ();
      B.reset ()
    ;;

    let reset_flush_history () =
      A.reset_flush_history ();
      B.reset_flush_history ()
    ;;

    let set_total n =
      A.set_total n;
      B.set_total n
    ;;

    let activity_start ~stage ~name ~tool =
      A.activity_start ~stage ~name ~tool;
      B.activity_start ~stage ~name ~tool
    ;;

    let activity_finish ~name =
      A.activity_finish ~name;
      B.activity_finish ~name
    ;;

    let activity_fail ~name =
      A.activity_fail ~name;
      B.activity_fail ~name
    ;;

    let activity_log ~name msg =
      A.activity_log ~name msg;
      B.activity_log ~name msg
    ;;

    let message msg =
      A.message msg;
      B.message msg
    ;;

    let error msg =
      A.error msg;
      B.error msg
    ;;

    let warning msg =
      A.warning msg;
      B.warning msg
    ;;

    let info msg =
      A.info msg;
      B.info msg
    ;;

    let verbose msg =
      A.verbose msg;
      B.verbose msg
    ;;
  end : Backend_intf.S)
;;
