open Import
module Display = Dune_engine.Display

(* Platform detection for install command hints *)
module Platform = struct
  type package_manager =
    | Apt
    | Brew
    | Dnf
    | Pacman
    | Apk
    | Unknown

  let of_string = function
    | "apt" -> Some Apt
    | "brew" -> Some Brew
    | "dnf" -> Some Dnf
    | "pacman" -> Some Pacman
    | "apk" -> Some Apk
    | "unknown" -> Some Unknown
    | _ -> None
  ;;

  let detect () =
    (* Allow overriding platform detection via environment variable for testing *)
    match Sys.getenv_opt "DUNE_PKG_PLATFORM" with
    | Some s ->
      (match of_string s with
       | Some pm -> pm
       | None -> Unknown)
    | None ->
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
    | Unknown -> sprintf "# install: %s" (String.concat ~sep:" " packages)
  ;;
end

let depexts_hint = function
  | [] -> None
  | depexts ->
    let pm = Platform.detect () in
    let install_cmd = Platform.install_command pm depexts in
    [ Pp.textf "Missing system dependencies: %s" (String.concat ~sep:", " depexts)
    ; Pp.nop
    ; Pp.textf "To install:"
    ; Pp.verbatim (sprintf "  %s" install_cmd)
    ]
    |> Pp.concat_map ~sep:Pp.cut ~f:(fun pp -> Pp.box pp)
    |> Option.some
;;

module Output : sig
  type error

  val io : error -> Process.Io.output Process.Io.t

  val with_error
    :  accepted_exit_codes:int Predicate.t
    -> pkg:Dune_pkg.Package_name.t * Loc.t
    -> depexts:string list
    -> display:Display.t
    -> (error -> 'a)
    -> 'a

  val prerr : rc:int -> error -> unit
end = struct
  type error =
    { pkg : Dune_pkg.Package_name.t * Loc.t
    ; depexts : string list
    ; filename : Dpath.t
    ; io : Process.Io.output Process.Io.t
    ; accepted_exit_codes : int Predicate.t
    ; display : Display.t
    }

  let io t = t.io

  let with_error ~accepted_exit_codes ~pkg ~depexts ~display f =
    let filename = Temp.create File ~prefix:"dune-pkg" ~suffix:"stderr" in
    let io = Process.Io.(file filename Out) in
    let t = { filename; io; accepted_exit_codes; display; pkg; depexts } in
    let result = f t in
    Temp.destroy File filename;
    result
  ;;

  let to_paragraphs t error =
    let pp_pkg = Pp.textf "Logs for package %s" (Package.Name.to_string (fst t.pkg)) in
    [ pp_pkg; Pp.verbatim error ]
  ;;

  let prerr ~rc error =
    let hints =
      lazy
        (match depexts_hint error.depexts with
         | None -> []
         | Some h -> [ h ])
    in
    let loc = snd error.pkg in
    match Predicate.test error.accepted_exit_codes rc, error.display with
    | false, _ ->
      let paragraphs = Stdune.Io.read_file error.filename |> to_paragraphs error in
      User_warning.emit ~hints:(Lazy.force hints) ~loc ~is_error:true paragraphs
    | true, Display.Verbose ->
      let content = Stdune.Io.read_file error.filename in
      if not (String.is_empty content)
      then (
        let paragraphs = to_paragraphs error content in
        User_warning.emit ~hints:(Lazy.force hints) ~loc paragraphs)
    | true, _ -> ()
  ;;
end

module Spec = struct
  type 'path chunk =
    | String of string
    | Path of 'path

  type 'path arg = 'path chunk Array.Immutable.t

  type ('path, 'target) t =
    { prog : ('path, Action.Prog.Not_found.t) result
    ; args : 'path arg Array.Immutable.t
    ; prefix : 'path
    ; ocamlfind_destdir : 'path
    ; pkg : Dune_pkg.Package_name.t * Loc.t
    ; depexts : string list
    }

  let name = "run-with-path"
  let version = 4

  let map_arg arg ~f =
    Array.Immutable.map arg ~f:(function
      | String _ as s -> s
      | Path p -> Path (f p))
  ;;

  let bimap t f _g =
    { t with
      args = Array.Immutable.map t.args ~f:(map_arg ~f)
    ; prefix = f t.prefix
    ; ocamlfind_destdir = f t.ocamlfind_destdir
    ; prog = Result.map t.prog ~f
    }
  ;;

  let is_useful_to ~memoize:_ = true

  (* For cache key computation, use canonical placeholders for prefix paths.
     This enables cache sharing across projects since BUILD_PATH_PREFIX_MAP
     ensures outputs are relocatable. The actual paths are used at execution time. *)
  let encode
        { prog; args; prefix = _; ocamlfind_destdir = _; pkg = _; depexts = _ }
        path
        _
    : Sexp.t
    =
    let prog : Sexp.t =
      match prog with
      | Ok p -> path p
      | Error e -> Atom e.program
    in
    let args =
      Array.Immutable.to_list_map args ~f:(fun x ->
        Sexp.List
          (Array.Immutable.to_list_map x ~f:(function
             | String s -> Sexp.Atom s
             | Path p -> path p)))
    in
    (* Use canonical placeholders - actual paths don't affect build output
       because BUILD_PATH_PREFIX_MAP makes outputs relocatable *)
    List [ List ([ prog ] @ args); Atom "/PREFIX"; Atom "/OCAMLFIND_DESTDIR" ]
  ;;

  let action
        { prog; args; prefix; ocamlfind_destdir; pkg; depexts }
        ~(ectx : Action.context)
        ~(eenv : Action.env)
    =
    let open Fiber.O in
    let display = !Clflags.display in
    match prog with
    | Error e -> Action.Prog.Not_found.raise e
    | Ok prog ->
      let args =
        Array.Immutable.to_list_map args ~f:(fun arg ->
          Array.Immutable.to_list_map arg ~f:(function
            | String s -> s
            | Path p -> Path.to_absolute_filename p)
          |> String.concat ~sep:"")
      in
      let metadata = Process.create_metadata ~purpose:ectx.metadata.purpose () in
      let dune_folder =
        let bin_folder = Temp.create Dir ~prefix:"dune" ~suffix:"self-in-path" in
        let src = Path.of_string Sys.executable_name in
        let dst = Path.relative bin_folder "dune" in
        Io.portable_symlink ~src ~dst;
        Path.to_string bin_folder
      in
      let prefix_value = Path.to_absolute_filename prefix in
      let ocamlfind_destdir_value = Path.to_absolute_filename ocamlfind_destdir in
      let env =
        eenv.env
        (* OPAM_SWITCH_PREFIX is the standard opam env var for the switch prefix *)
        |> Env.add ~var:"OPAM_SWITCH_PREFIX" ~value:prefix_value
        (* PREFIX is also set for compatibility with traditional Makefiles *)
        |> Env.add ~var:"PREFIX" ~value:prefix_value
        |> Env.add ~var:"OCAMLFIND_DESTDIR" ~value:ocamlfind_destdir_value
        |> Env.update ~var:"PATH" ~f:(function
          | None -> Some dune_folder
          | Some path -> Some (sprintf "%s:%s" dune_folder path))
      in
      (* BUILD_PATH_PREFIX_MAP for reproducible/relocatable builds.
         Maps absolute PREFIX and OCAMLFIND_DESTDIR to canonical paths so
         tools that support this variable produce relocatable output. *)
      let env =
        Dune_util.Build_path_prefix_map.extend_build_path_prefix_map
          env
          `New_rules_have_precedence
          [ Some { source = prefix_value; target = "/OPAMROOT" }
          ; Some { source = ocamlfind_destdir_value; target = "/OPAMROOT/lib" }
          ]
      in
      Output.with_error
        ~accepted_exit_codes:eenv.exit_codes
        ~pkg
        ~depexts
        ~display
        (fun error ->
           let stdout_to =
             match !Clflags.debug_package_logs, display with
             | true, _ | false, Display.Verbose -> eenv.stdout_to
             | _ -> Process.Io.(null Out)
           in
           let* _, rc =
             Process.run
               Return
               prog
               args
               ~display
               ~metadata
               ~stdout_to
               ~stderr_to:(Output.io error)
               ~stdin_from:eenv.stdin_from
               ~dir:eenv.working_dir
               ~env
           in
           Output.prerr ~rc error;
           Fiber.return ())
  ;;
end

module A = Action_ext.Make (Spec)

let action ~pkg ~depexts prog args ~prefix ~ocamlfind_destdir =
  A.action { Spec.prog; args; prefix; ocamlfind_destdir; pkg; depexts }
;;

(* System_spec: like Spec but for shell commands (system actions).
   Separates cache key from execution-time prefix values for cross-project caching. *)
module System_spec = struct
  type ('path, 'target) t =
    { cmd : string
    ; prefix : 'path
    ; ocamlfind_destdir : 'path
    }

  let name = "system-with-path"
  let version = 1

  let bimap t f _g =
    { t with prefix = f t.prefix; ocamlfind_destdir = f t.ocamlfind_destdir }
  ;;

  let is_useful_to ~memoize:_ = true

  (* For cache key computation, use canonical placeholders for prefix paths.
     This enables cache sharing across projects since BUILD_PATH_PREFIX_MAP
     ensures outputs are relocatable. The actual paths are used at execution time. *)
  let encode { cmd; prefix = _; ocamlfind_destdir = _ } _path _ : Sexp.t =
    (* Use canonical placeholders - actual paths don't affect build output
       because BUILD_PATH_PREFIX_MAP makes outputs relocatable *)
    List [ Atom cmd; Atom "/PREFIX"; Atom "/OCAMLFIND_DESTDIR" ]
  ;;

  let action
        { cmd; prefix; ocamlfind_destdir }
        ~(ectx : Action.context)
        ~(eenv : Action.env)
    =
    let open Fiber.O in
    let prefix_value = Path.to_absolute_filename prefix in
    let ocamlfind_destdir_value = Path.to_absolute_filename ocamlfind_destdir in
    (* Set up BUILD_PATH_PREFIX_MAP for relocatable builds *)
    let env =
      Dune_util.Build_path_prefix_map.extend_build_path_prefix_map
        eenv.env
        `New_rules_have_precedence
        [ Some { source = prefix_value; target = "/OPAMROOT" }
        ; Some { source = ocamlfind_destdir_value; target = "/OPAMROOT/lib" }
        ]
    in
    let prog, arg =
      Env_path.system_shell_exn ~needed_to:"interpret (system ...) actions"
    in
    let display = !Clflags.display in
    Process.run
      (Accept eenv.exit_codes)
      prog
      [ arg; cmd ]
      ~display
      ~metadata:ectx.metadata
      ~stdout_to:eenv.stdout_to
      ~stderr_to:eenv.stderr_to
      ~stdin_from:eenv.stdin_from
      ~dir:eenv.working_dir
      ~env
    >>| function
    | Error _ -> ()
    | Ok _ -> ()
  ;;
end

module System_A = Action_ext.Make (System_spec)

let system_action ~cmd ~prefix ~ocamlfind_destdir =
  System_A.action { System_spec.cmd; prefix; ocamlfind_destdir }
;;
