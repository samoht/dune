open Import
module Package_version = Dune_pkg.Package_version

(* Platform detection for install commands *)
module Platform = struct
  type package_manager =
    | Apt
    | Brew
    | Dnf
    | Pacman
    | Apk
    | Unknown

  let detect () =
    (* Check for common package managers using 'which' command *)
    let has_cmd cmd =
      let exit_code = Sys.command (sprintf "which %s >/dev/null 2>&1" cmd) in
      exit_code = 0
    in
    if has_cmd "brew"
    then Brew
    else if has_cmd "apt"
    then Apt
    else if has_cmd "dnf"
    then Dnf
    else if has_cmd "pacman"
    then Pacman
    else if has_cmd "apk"
    then Apk
    else Unknown
  ;;

  let install_command pm packages =
    match pm with
    | Apt -> sprintf "apt install %s" (String.concat ~sep:" " packages)
    | Brew -> sprintf "brew install %s" (String.concat ~sep:" " packages)
    | Dnf -> sprintf "dnf install %s" (String.concat ~sep:" " packages)
    | Pacman -> sprintf "pacman -S %s" (String.concat ~sep:" " packages)
    | Apk -> sprintf "apk add %s" (String.concat ~sep:" " packages)
    | Unknown -> sprintf "# install: %s" (String.concat ~sep:" " packages)
  ;;
end

type output_format =
  | Default
  | Short

let print_depexts ~format context_name =
  let open Fiber.O in
  let+ depexts_with_origins =
    build_exn (fun () ->
      Dune_rules.Pkg_rules.all_filtered_depexts_with_origins context_name)
  in
  match format with
  | Short ->
    (* Just print package names, one per line (for scripting) *)
    let unique_depexts =
      List.map depexts_with_origins ~f:(fun (depext, _, _) -> depext)
      |> List.sort_uniq ~compare:String.compare
    in
    Console.print [ Pp.concat_map ~sep:Pp.newline ~f:Pp.verbatim unique_depexts ]
  | Default ->
    if List.is_empty depexts_with_origins
    then Console.print [ Pp.text "No system dependencies required." ]
    else (
      (* Group by depext to combine origins *)
      let by_depext =
        List.fold_left
          depexts_with_origins
          ~init:String.Map.empty
          ~f:(fun acc (depext, pkg_name, pkg_version) ->
            let origin =
              sprintf
                "%s.%s"
                (Package_name.to_string pkg_name)
                (Package_version.to_string pkg_version)
            in
            String.Map.update acc depext ~f:(function
              | None -> Some [ origin ]
              | Some origins -> Some (origin :: origins)))
      in
      (* Print depexts with origins *)
      let sorted_depexts =
        String.Map.to_list by_depext
        |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
      in
      let depext_lines =
        List.map sorted_depexts ~f:(fun (depext, origins) ->
          let origins_str =
            String.concat ~sep:", " (List.sort ~compare:String.compare origins)
          in
          Pp.textf "%-16s (%s)" depext origins_str)
      in
      Console.print (depext_lines @ [ Pp.nop ]);
      (* Detect platform and show install command *)
      let pm = Platform.detect () in
      let packages = List.map sorted_depexts ~f:fst in
      let install_cmd = Platform.install_command pm packages in
      Console.print [ Pp.verbatim install_cmd ])
;;

let term =
  let+ builder = Common.Builder.term
  and+ context_name = Common.context_arg ~doc:(Some "Build context to use.")
  and+ short =
    Arg.(
      value
      & flag
      & info
          [ "short" ]
          ~doc:(Some "Print only package names, one per line (for scripting)."))
  in
  let format = if short then Short else Default in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler.go_with_rpc_server ~common ~config (fun () ->
    print_depexts ~format context_name)
;;

let info =
  let doc = "Print external system dependencies" in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        "Print system packages required by the project's dependencies. Shows which \
         package requires each dependency and suggests installation commands for your \
         platform."
    ; `S "OPTIONS"
    ; `S "EXAMPLES"
    ; `Pre "  $ dune show depexts"
    ; `Pre "  libssl-dev      (tls.0.17.0)"
    ; `Pre "  "
    ; `Pre "  apt install libssl-dev"
    ; `Noblank
    ; `Pre "  "
    ; `Pre "  $ dune show depexts --short | xargs apt install"
    ]
  in
  Cmd.info "depexts" ~doc ~man
;;

let command = Cmd.v info term
