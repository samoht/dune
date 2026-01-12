open Stdune

(** Helper to print a user message to stderr *)
val print_msg : User_message.t -> unit

(** Output to a terminal without any capbilities and without flushing *)
module No_flush : Backend_intf.S

(** Output to a terminal without any capbilities with flushing *)
include Backend_intf.S
