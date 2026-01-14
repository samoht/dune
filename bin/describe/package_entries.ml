open Import

let term =
  let+ builder = Common.Builder.term
  and+ context_name = Common.context_arg ~doc:(Some "Build context to use.")
  and+ format = Describe_format.arg in
  let common, config = Common.init builder in
  Scheduler.go_with_rpc_server ~common ~config
  @@ fun () ->
  let open Fiber.O in
  let* setup = Import.Main.setup () in
  let* setup = Memo.run setup in
  let super_context = Import.Main.find_scontext_exn setup ~name:context_name in
  build_exn
  @@ fun () ->
  let open Memo.O in
  let* packages = Dune_rules.Dune_load.packages () in
  let+ entries =
    Package.Name.Map.to_list packages
    |> Memo.parallel_map ~f:(fun (name, _pkg) ->
      let+ entries = Dune_rules.Install_rules.stanzas_to_entries super_context name in
      name, entries)
  in
  Package.Name.Map.of_list_exn entries
  |> Package.Name.Map.to_dyn (Dyn.list Install.Entry.Sourced.Unexpanded.to_dyn)
  |> Describe_format.print_dyn format
;;

let command =
  let doc = "prints information about the entries per package." in
  let info = Cmd.info ~doc "package-entries" in
  Cmd.v info term
;;
