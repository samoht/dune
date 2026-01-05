(** Unified progress tracking for builds and package management.

    This module tracks progress at library/package granularity rather than
    rules, providing a stable count that matches how users think about builds.

    Format: "Building: target1, target2 (3/15, 8j)"
    - Comma-separated target names (libraries, packages)
    - 3/15 = completed out of total
    - 8j = concurrent jobs

    When fetching, shows: "Fetching: pkg | Building: lib (3/15, 8j)" *)

module Target : sig
  type t

  val create : name:string -> is_fetch:bool -> t
  val library : string -> t
  val package : string -> t
  val fetch : string -> t
end

(** Reset all progress state *)
val reset : unit -> unit

(** Set total number of libraries/packages (should be known upfront) *)
val set_total : int -> unit

(** Set current number of concurrent jobs *)
val set_jobs : int -> unit

(** Increment cached count (for summary) *)
val incr_cached : unit -> unit

(** Mark a target as actively being processed *)
val start_target : Target.t -> unit

(** Mark a target as completed *)
val finish_target : name:string -> unit

(** Mark a target as failed *)
val fail_target : name:string -> unit

(** Render progress respecting terminal width.
    Groups fetching targets separately if any. *)
val pp : max_width:int -> unit Pp.t

(** Render summary after completion. *)
val summary_pp : elapsed_secs:float -> unit Pp.t
