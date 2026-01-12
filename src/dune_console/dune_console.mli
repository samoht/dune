open Stdune

(** Manages the console *)

(** The console is a system than can report messages and a status to the user.
    It is usually the terminal the application is connected to, however it could
    be something else. This module allow to set a global backend for the
    application as well as composing backends.

    Design principle: One API, backend decides presentation.
    Callers say "this event happened" - backend decides how to show it. *)

type stage = Backend_intf.stage =
  | Fetch
  | Build
  | Scan
  | Save
  | Vendor

module type Backend = Backend_intf.S

(** [separate_messages b] changes the behavior of [print_user_message], so that
    it separates messages with a blank line when [b = true]. *)
val separate_messages : bool -> unit

module Backend : sig
  type t = (module Backend)

  val set : t -> unit

  (** [compose a b] produce a backend that sends message to both [a] and [b]
      backends. *)
  val compose : t -> t -> t

  (** [flush t] returns a backend that will flush [stderr] after every write *)
  val flush : t -> t

  (** Dumb backend - no status line, just prints messages. *)
  val dumb : t

  (** Progress backend - displays status line in terminal. *)
  val progress : t

  (** Progress backend without flushing (for use by threaded console). *)
  val progress_no_flush : t

  (** Quiet backend - errors only. *)
  val quiet : t

  (** Short backend - one line per event with progress counter. *)
  val short : t

  (** Verbose backend - one line per event plus log output. *)
  val verbose : t
end

(** Format and print a user message to the console. *)
val print_user_message : User_message.t -> unit

(** Reset the log output and (try) to remove the history. *)
val reset_flush_history : unit -> unit

(** Reset the log output. *)
val reset : unit -> unit

(** Finish outputting to the console. *)
val finish : unit -> unit

(** [print paragraphs] is a short-hand for:

    {[
      print_user_message (User_message.make paragraphs)
    ]} *)
val print : User_message.Style.t Pp.t list -> unit

(** [printf fmt] is a convenient function for debugging. It formats a string and
    then print it raw followed by a newline. It is the same as:

    {[
      print [Pp.verbatim (sprintf fmt ...)]
    ]}

    For properly formatted output you should use [print]. *)
val printf : ('a, unit, string, unit) format4 -> 'a

module Status_line : sig
  (** Status line management *)

  (** The current status line. *)
  type t =
    | Live of (unit -> User_message.Style.t Pp.t)
    (** A "live" value that's updated continuously, such as a progress
        indicator. This message is not shown when a "dumb" terminal backend
        is in use. *)
    | Constant of User_message.Style.t Pp.t
    (** A fixed value. Unlike with [Live], this text is printed even if a
        dumb console backend is in use. *)

  val set : t -> unit

  (** Clear the current status line. *)
  val clear : unit -> unit

  type overlay

  (** Add an overlay on top of the current status line. [set] and [clear] remove
      any active overlay. *)
  val add_overlay : t -> overlay

  (** Remove an overlay if it is still active. Do nothing otherwise. *)
  val remove_overlay : overlay -> unit

  (** [with_overlay t ~f] is the same as:

      {[
        let id = add_overlay t in
        Exn.protect f ~finally:(fun () -> remove_overlay id)
      ]} *)
  val with_overlay : t -> f:(unit -> 'a) -> 'a

  val refresh : unit -> unit
end

(** {1 Event-driven API} *)

(** Set total count (libs + pkgs + exes - coarse grained). *)
val set_total : int -> unit

(** Mark an activity as started. *)
val start : stage:stage -> name:string -> tool:string option -> unit

(** Mark an activity as finished. *)
val finish_activity : name:string -> unit

(** Mark an activity as failed. *)
val fail : name:string -> unit

(** Log output for an activity - shown in verbose mode. *)
val log : name:string -> string -> unit

(** Messages - always shown (except in quiet mode). *)
val message : User_message.t -> unit

(** Errors - always shown. *)
val error : User_message.t -> unit
