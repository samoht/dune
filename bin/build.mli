open Import

val run_build_system
  :  common:Common.t
  -> auto_fetch:bool
  -> request:(Dune_rules.Main.build_system -> unit Action_builder.t)
  -> (unit, [ `Already_reported ]) result Fiber.t

val build : unit Cmd.t

val run_build_command
  :  common:Common.t
  -> config:Dune_config.t
  -> auto_fetch:bool
  -> request:(Dune_rules.Main.build_system -> unit Action_builder.t)
  -> unit
