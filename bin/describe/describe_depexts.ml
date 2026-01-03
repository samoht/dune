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
    | Nix
    | Unknown

  let all = [ "apt"; "brew"; "dnf"; "pacman"; "apk"; "nix" ]

  let of_string = function
    | "apt" -> Some Apt
    | "brew" -> Some Brew
    | "dnf" -> Some Dnf
    | "pacman" -> Some Pacman
    | "apk" -> Some Apk
    | "nix" -> Some Nix
    | _ -> None
  ;;

  let to_string = function
    | Apt -> "apt"
    | Brew -> "brew"
    | Dnf -> "dnf"
    | Pacman -> "pacman"
    | Apk -> "apk"
    | Nix -> "nix"
    | Unknown -> "unknown"
  ;;

  let detect () =
    let path = Env_path.path Env.initial in
    let has_cmd cmd = Option.is_some (Bin.which ~path cmd) in
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
    | Nix -> sprintf "nix-shell -p %s" (String.concat ~sep:" " packages)
    | Unknown -> sprintf "# install: %s" (String.concat ~sep:" " packages)
  ;;
end

type output_format =
  | Default
  | Short
  | Json

(* JSON output helpers *)
let json_string s =
  (* Escape special characters for JSON strings *)
  let buf = Buffer.create (String.length s) in
  String.iter s ~f:(fun c ->
    match c with
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c -> Buffer.add_char buf c);
  sprintf "\"%s\"" (Buffer.contents buf)
;;

let json_array items = sprintf "[%s]" (String.concat ~sep:", " items)

let json_object fields =
  sprintf
    "{%s}"
    (String.concat ~sep:", " (List.map fields ~f:(fun (k, v) -> sprintf "%s: %s" k v)))
;;

let print_depexts ~format ~pm context_name =
  let open Fiber.O in
  let+ depexts_with_origins =
    build_exn (fun () ->
      Dune_rules.Pkg_rules.all_filtered_depexts_with_origins context_name)
  in
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
  let sorted_depexts =
    String.Map.to_list by_depext
    |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  in
  match format with
  | Short ->
    (* Just print package names, one per line (for scripting) *)
    let unique_depexts = List.map sorted_depexts ~f:fst in
    Console.print [ Pp.concat_map ~sep:Pp.newline ~f:Pp.verbatim unique_depexts ]
  | Json ->
    let pm = Option.value pm ~default:(Platform.detect ()) in
    let packages = List.map sorted_depexts ~f:fst in
    let depexts_json =
      List.map sorted_depexts ~f:(fun (name, origins) ->
        json_object
          [ json_string "name", json_string name
          ; ( json_string "required_by"
            , json_array
                (List.map (List.sort ~compare:String.compare origins) ~f:json_string) )
          ])
    in
    let install_json =
      let cmd, args =
        match pm with
        | Platform.Apt -> "apt", "install" :: packages
        | Brew -> "brew", "install" :: packages
        | Dnf -> "dnf", "install" :: packages
        | Pacman -> "pacman", "-S" :: packages
        | Apk -> "apk", "add" :: packages
        | Nix -> "nix-shell", "-p" :: packages
        | Unknown -> "#", "install:" :: packages
      in
      json_object
        [ json_string "command", json_string cmd
        ; json_string "args", json_array (List.map args ~f:json_string)
        ]
    in
    let json =
      json_object
        [ json_string "pm", json_string (Platform.to_string pm)
        ; json_string "depexts", json_array depexts_json
        ; json_string "install", install_json
        ]
    in
    Console.print [ Pp.verbatim json ]
  | Default ->
    if List.is_empty depexts_with_origins
    then Console.print [ Pp.text "No system dependencies required." ]
    else (
      let depext_lines =
        List.map sorted_depexts ~f:(fun (depext, origins) ->
          let origins_str =
            String.concat ~sep:", " (List.sort ~compare:String.compare origins)
          in
          Pp.textf "%-16s (%s)" depext origins_str)
      in
      Console.print (depext_lines @ [ Pp.nop ]);
      (* Use specified or auto-detected package manager *)
      let pm = Option.value pm ~default:(Platform.detect ()) in
      let packages = List.map sorted_depexts ~f:fst in
      let install_cmd = Platform.install_command pm packages in
      Console.print [ Pp.verbatim install_cmd ])
;;

let pm_conv =
  let parse s =
    match Platform.of_string s with
    | Some pm -> Ok pm
    | None ->
      Error
        (`Msg
            (sprintf
               "Unknown package manager %S. Valid options: %s"
               s
               (String.concat ~sep:", " Platform.all)))
  in
  let print ppf pm = Format.pp_print_string ppf (Platform.to_string pm) in
  Arg.conv (parse, print)
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
  and+ json =
    Arg.(
      value
      & flag
      & info [ "json" ] ~doc:(Some "Output as JSON (for tooling integration)."))
  and+ pm =
    Arg.(
      value
      & opt (some pm_conv) None
      & info
          [ "pm" ]
          ~docv:"MANAGER"
          ~doc:
            (Some
               (sprintf
                  "Use specified package manager (%s). Default: auto-detect."
                  (String.concat ~sep:", " Platform.all))))
  in
  let format = if short then Short else if json then Json else Default in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler.go_with_rpc_server ~common ~config (fun () ->
    print_depexts ~format ~pm context_name)
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
    ; `Noblank
    ; `Pre "  "
    ; `Pre "  $ dune show depexts --pm=brew"
    ; `Pre "  libssl-dev      (tls.0.17.0)"
    ; `Pre "  "
    ; `Pre "  brew install libssl-dev"
    ]
  in
  Cmd.info "depexts" ~doc ~man
;;

let command = Cmd.v info term
