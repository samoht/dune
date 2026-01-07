open Import

type checked =
  | In_build_dir of (Context.t * Path.Source.t)
  | In_pkg_dir of (Context.t * Path.Source.t)
  | In_install_dir of (Context.t * Path.Source.t)
  | In_source_dir of Path.Source.t
  | External of Path.External.t

let is_pkg_context ctx =
  Context_name.equal ctx Dune_rules.Private_context.t.name
  || Context_name.equal ctx Dune_rules.Pkg_rules.context.name
  || Context_name.equal ctx Dune_rules.Lock_dir.context.name
  || Context_name.equal ctx Dune_rules.Fetch_rules.context.name
;;

let check_path contexts =
  let contexts =
    Dune_engine.Context_name.Map.of_list_map_exn contexts ~f:(fun c -> Context.name c, c)
  in
  fun path ->
    let internal_path () =
      User_error.raise
        [ Pp.textf "This path is internal to dune: %s" (Path.to_string_maybe_quoted path)
        ]
    in
    let context_exn ctx =
      match Dune_engine.Context_name.Map.find contexts ctx with
      | Some context -> context
      | None ->
        User_error.raise
          [ Pp.textf
              "%s refers to unknown build context: %s"
              (Path.to_string_maybe_quoted path)
              (Dune_engine.Context_name.to_string ctx)
          ]
          ~hints:
            (User_message.did_you_mean
               (Dune_engine.Context_name.to_string ctx)
               ~candidates:
                 (Dune_engine.Context_name.Map.keys contexts
                  |> List.map ~f:Dune_engine.Context_name.to_string))
    in
    match path with
    | External e -> External e
    | In_source_tree s -> In_source_dir s
    | In_build_dir path ->
      (match Dune_engine.Dpath.analyse_target path with
       | Other _ -> internal_path ()
       | Alias (_, _) -> internal_path ()
       | Anonymous_action _ -> internal_path ()
       | Regular (name, src) ->
         (match Install.Context.analyze_path name src with
          | Invalid -> internal_path ()
          | Install (ctx, path) -> In_install_dir (context_exn ctx, path)
          | Normal (ctx, src_path) ->
            if is_pkg_context ctx
            then (
              (* For pkg contexts like _build/pkg/<ctx>/..., extract the actual context
                 from the first component of src_path *)
              match Path.Source.split_first_component src_path with
              | Some (ctx_name, rest) ->
                let build_ctx = Context_name.of_string ctx_name in
                In_pkg_dir (context_exn build_ctx, Path.Source.of_local rest)
              | None -> internal_path ())
            else In_build_dir (context_exn ctx, src_path)))
;;
