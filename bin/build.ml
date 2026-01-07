open Import

let with_metrics ~common f =
  let start_time = Time.now () in
  Fiber.finalize f ~finally:(fun () ->
    let duration = Time.diff (Time.now ()) start_time in
    if Common.print_metrics common
    then (
      let gc_stat = Gc.quick_stat () in
      (* We reset Memo counters below, unconditionally. *)
      let memo_counters_report = Memo.Metrics.report ~reset_after_reporting:false in
      Console.print_user_message
        (User_message.make
           ([ Pp.textf "%s" memo_counters_report
            ; Pp.textf
                "(%.2fs total, %.1fM heap words)"
                (Time.Span.to_secs duration)
                (float_of_int gc_stat.heap_words /. 1_000_000.)
            ; Pp.text "Timers:"
            ]
            @ List.map
                ~f:(fun (timer, { Metrics.Timer.Measure.cumulative_time; count }) ->
                  Pp.textf
                    "%s - time spent = %.2fs, count = %d"
                    timer
                    (Time.Span.to_secs cumulative_time)
                    count)
                (String.Map.to_list (Metrics.Timer.aggregated_timers ())))));
    Memo.Metrics.reset ();
    Fiber.return ())
;;

let run_build_system ~common ~auto_fetch ~request =
  let run ~(toplevel : unit Memo.Lazy.t) =
    with_metrics ~common (fun () -> build (fun () -> Memo.Lazy.force toplevel))
  in
  let open Fiber.O in
  Fiber.finalize
    (fun () ->
       Cached_digest.invalidate_cached_timestamps ();
       Console.Status_line.set
         (Live
            (fun () ->
              Pp.map_tags (Dune_engine.Progress.pp ~max_width:80) ~f:(fun () ->
                User_message.Style.Details)));
       let* () =
         if not auto_fetch
         then Fiber.return ()
         else (
           let lock_dir_path = Dune_rules.Lock_dir.default_source_path in
           if Path.exists (Path.source lock_dir_path)
           then
             let* solver_env = Pkg.Pkg_common.poll_solver_env_from_current_system () in
             Pkg.Fetch.auto_fetch_missing ~lock_dir_path ~solver_env ()
           else Fiber.return ())
       in
       let* setup = Import.Main.setup () in
       let request =
         Action_builder.bind (Action_builder.of_memo setup) ~f:(fun setup ->
           request setup)
       in
       (* CR-someday cmoseley: Can we avoid creating a new lazy memo node every
         time the build system is rerun? *)
       (* This top-level node is used for traversing the whole Memo graph. *)
       let toplevel_cell, toplevel =
         Memo.Lazy.Expert.create ~name:"toplevel" (fun () ->
           let open Memo.O in
           let+ (), (_ : Dep.Fact.t Dep.Map.t) =
             Action_builder.evaluate_and_collect_facts request
           in
           ())
       in
       let* res = run ~toplevel in
       let+ () =
         match Common.dump_memo_graph_file common with
         | None -> Fiber.return ()
         | Some file ->
           let path = Path.external_ file in
           let+ graph =
             Memo.dump_cached_graph
               ~time_nodes:(Common.dump_memo_graph_with_timing common)
               toplevel_cell
           in
           Graph.serialize graph ~path ~format:(Common.dump_memo_graph_format common)
         (* CR-someday cmoseley: It would be nice to use Persistent to dump a
           copy of the graph's internal representation here, so it could be used
           without needing to re-run the build*)
       in
       res)
    ~finally:(fun () ->
      Hooks.End_of_build.run ();
      Fiber.return ())
;;

let poll_handling_rpc_build_requests ~(common : Common.t) =
  let open Fiber.O in
  let rpc =
    match Common.rpc common with
    | `Allow server -> server
    | `Forbid_builds -> Code_error.raise "rpc server must be allowed in passive mode" []
  in
  Dune_engine.Scheduler.Run.poll_passive
    ~get_build_request:
      (let+ { kind; outcome } = Dune_rpc_impl.Server.pending_action rpc in
       let request setup =
         let root = Common.root common in
         match kind with
         | Build targets -> Target.interpret_targets (Common.root common) setup targets
         | Runtest test_paths ->
           Runtest_common.make_request
             ~scontexts:setup.scontexts
             ~to_cwd:root.to_cwd
             ~test_paths
       in
       run_build_system ~common ~auto_fetch:true ~request, outcome)
;;

let run_build_command_poll_eager
      ~(common : Common.t)
      ~config
      ~auto_fetch
      ~auto_lock:_
      ~request
  : unit
  =
  Scheduler.go_with_rpc_server_and_console_status_reporting ~common ~config (fun () ->
    let open Fiber.O in
    let+ () =
      Dune_engine.Scheduler.Run.poll (run_build_system ~common ~auto_fetch ~request)
    and+ () = poll_handling_rpc_build_requests ~common in
    ())
;;

let run_build_command_poll_passive ~common ~config ~auto_fetch:_ ~auto_lock:_ ~request:_
  : unit
  =
  Scheduler.go_with_rpc_server_and_console_status_reporting ~common ~config (fun () ->
    poll_handling_rpc_build_requests ~common)
;;

let run_build_command_once ~(common : Common.t) ~config ~auto_fetch ~auto_lock:_ ~request =
  let open Fiber.O in
  let once () =
    let+ res = run_build_system ~common ~auto_fetch ~request in
    match res with
    | Error `Already_reported -> raise Dune_util.Report_error.Already_reported
    | Ok () -> ()
  in
  Scheduler.go_with_rpc_server ~common ~config once
;;

let run_build_command ~(common : Common.t) ~config ~auto_fetch ~auto_lock ~request =
  (match Common.watch common with
   | Yes Eager -> run_build_command_poll_eager
   | Yes Passive -> run_build_command_poll_passive
   | No -> run_build_command_once)
    ~common
    ~config
    ~auto_fetch
    ~auto_lock
    ~request
;;

let build =
  let doc = "Build the given targets, or the default ones if none are given." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Targets starting with a $(b,@) are interpreted as aliases.|}
    ; `Blocks Common.help_secs
    ; Common.examples
        [ "Build all targets in the current source tree", "dune build"
        ; "Build targets in the `./foo/bar' directory", "dune build ./foo/bar"
        ; ( "Build the minimal set of targets required for tooling such as Merlin \
             (useful for quickly detecting errors)"
          , "dune build @check" )
        ; "Run all code formatting tools in-place", "dune build --auto-promote @fmt"
        ]
    ]
  in
  (* CR-someday Alizter: document this option *)
  let name_ = Arg.info [] ~docv:"TARGET" ~doc:None in
  let term =
    let+ builder = Common.Builder.term
    and+ targets = Arg.(value & pos_all dep [] name_)
    and+ aliases_rec =
      Arg.(
        value
        & opt_all Dep.alias_rec_arg []
        & info
            [ "alias-rec" ]
            ~docv:"ALIAS"
            ~doc:
              (Some
                 "Build the alias $(docv) in its parent directory and all \
                  subdirectories. Equivalent to the build target $(b,@)$(docv). Example: \
                  $(b,--alias-rec dir/foo) builds the $(b,foo) alias in $(b,dir/) and \
                  all its subdirectories. Repeatable."))
    and+ aliases =
      Arg.(
        value
        & opt_all Dep.alias_arg []
        & info
            [ "alias" ]
            ~docv:"ALIAS"
            ~doc:
              (Some
                 "Build $(docv) in its parent directory only. Equivalent to the build \
                  target $(b,@@)$(docv). Example: $(b,--alias dir/foo) builds the \
                  $(b,foo) alias in $(b,dir/) only. Repeatable."))
    and+ auto_fetch_opt =
      let toggle = [ "enabled", true; "disabled", false ] in
      let doc =
        Printf.sprintf
          "Enable or disable automatic fetching of missing dune packages to duniverse \
           (%s)."
          (Arg.doc_alts_enum toggle)
      in
      Arg.(
        value
        & opt (some (enum toggle)) None
        & info
            [ "auto-fetch" ]
            ~env:(Cmd.Env.info ~doc Common.auto_fetch_env)
            ~doc:(Some doc))
    and+ auto_lock_opt =
      let modes = Dune_config.Auto_lock.all in
      let doc =
        Printf.sprintf
          "Control automatic locking behavior (%s). $(b,auto): use workspace config, \
           then check for lock file (default). $(b,disabled): ignore lock file, use \
           system packages only. $(b,enabled): enable package management, auto-lock if \
           missing. $(b,always): always re-solve before building."
          (Arg.doc_alts_enum modes)
      in
      Arg.(
        value
        & opt (some (enum modes)) None
        & info
            [ "auto-lock" ]
            ~env:(Cmd.Env.info ~doc Common.auto_lock_env)
            ~doc:(Some doc))
    in
    let targets = List.concat [ targets; aliases; aliases_rec ] in
    let targets =
      match targets with
      | [] -> [ Common.Builder.default_target builder ]
      | _ :: _ -> targets
    in
    let common, config = Common.init builder in
    (* Here we need to find out whether another instance of dune already holds
       the global build lock, as this will determine whether the current
       instance of dune will perform the build itself or send a build request
       to the RPC server in an already-running dune process. The method of
       checking whether another dune instance holds the lock is to simply try
       to take the lock. If taking the lock succeeds then the current process
       will perform the build itself, and future attempts by this process to
       take the lock are guaranteed to succeed. If taking the lock fails then
       we know that another instance of dune must have it, and the current
       process will send a build RPC request to that dune instance. Checking
       the status of the lock by taking prevents a race condition where the
       state of the lock could otherwise change between checking it and taking
       it. *)
    match Dune_util.Global_lock.lock ~timeout:None with
    | Error lock_held_by ->
      (* This case is reached if dune detects that another instance of dune
         is already running. Rather than performing the build itself, the
         current instance of dune will instruct the already-running instance to
         perform the build by sending an RPC message. As only one RPC server
         can run at a time we need to use a fiber scheduler which does not run
         an RPC server in the background to schedule the fiber which will
         perform the RPC call.
      *)
      let targets = Rpc.Rpc_common.prepare_targets targets in
      Scheduler.go_without_rpc_server ~common ~config (fun () ->
        let open Fiber.O in
        Rpc.Rpc_common.fire_request
          ~name:"build"
          ~wait:true
          ~lock_held_by
          builder
          Dune_rpc_impl.Decl.build
          targets
        >>| Rpc.Rpc_common.wrap_build_outcome_exn ~print_on_success:true)
    | Ok () ->
      let request setup = Target.interpret_targets (Common.root common) setup targets in
      let auto_fetch = Option.value auto_fetch_opt ~default:config.auto_fetch in
      let auto_lock = Option.value auto_lock_opt ~default:config.auto_lock in
      (* Override Clflags.auto_lock if CLI option was provided *)
      Option.iter auto_lock_opt ~f:(fun v -> Dune_rules.Clflags.auto_lock := v);
      Log.info
        "Build auto_lock"
        [ "auto_lock", Dune_config.Auto_lock.to_dyn !Dune_rules.Clflags.auto_lock ];
      run_build_command ~common ~config ~auto_fetch ~auto_lock ~request
  in
  Cmd.v (Cmd.info "build" ~doc ~man ~envs:Common.envs) term
;;
