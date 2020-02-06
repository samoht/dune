(** Mirage runes *)

open! Stdune

val gen_rules :
  sctx:Super_context.t -> dir:Path.Build.t -> Dune_file.Mirage.t -> unit

val init : dir:Path.Build.t -> Super_context.t -> unit
