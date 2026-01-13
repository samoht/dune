(* Format:
   [mylib] Building
   [mylib] ocamlfind ocamlopt -c foo.ml
   [mylib] Done
*)

let print_line ~name msg = Printf.eprintf "[%s] %s\n%!" name msg

module M : Backend_intf.S = struct
  let start () = ()
  let finish () = ()
  let print_user_message = Dumb.print_msg
  let set_status_line _ = ()
  let print_if_no_status_line _ = ()
  let reset () = Dumb.reset ()
  let reset_flush_history () = Dumb.reset_flush_history ()

  (* Event-driven API *)
  let set_total _ = ()

  let activity_start ~stage ~name ~tool =
    let stage_str = Backend_intf.stage_to_string stage in
    let msg =
      match tool with
      | None -> stage_str
      | Some t -> Printf.sprintf "%s [%s]" stage_str t
    in
    print_line ~name msg
  ;;

  let activity_finish ~name = print_line ~name "Done"
  let activity_fail ~name = print_line ~name "Failed"
  let activity_log ~name msg = print_line ~name msg

  let message msg =
    Dumb.print_msg msg;
    flush stderr
  ;;

  let error msg =
    Dumb.print_msg msg;
    flush stderr
  ;;

  let info msg = Printf.eprintf "%s\n%!" msg
  let verbose msg = Printf.eprintf "%s\n%!" msg
end

let backend = (module M : Backend_intf.S)
