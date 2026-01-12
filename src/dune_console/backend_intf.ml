open Stdune

type stage =
  | Fetch
  | Build
  | Scan
  | Save
  | Vendor

let stage_to_string = function
  | Fetch -> "Fetching"
  | Build -> "Building"
  | Scan -> "Scanning"
  | Save -> "Saving"
  | Vendor -> "Vendoring"
;;

module type S = sig
  val start : unit -> unit
  val print_user_message : User_message.t -> unit
  val set_status_line : User_message.Style.t Pp.t option -> unit
  val print_if_no_status_line : User_message.Style.t Pp.t -> unit
  val reset : unit -> unit
  val reset_flush_history : unit -> unit
  val finish : unit -> unit
  val set_total : int -> unit
  val activity_start : stage:stage -> name:string -> tool:string option -> unit
  val activity_finish : name:string -> unit
  val activity_fail : name:string -> unit
  val activity_log : name:string -> string -> unit
  val message : User_message.t -> unit
  val error : User_message.t -> unit
end

type t = (module S)
