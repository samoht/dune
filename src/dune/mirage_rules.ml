open Build.O
open Import
module SC = Super_context
module CC = Compilation_context

(* let mirage sctx ~dir = SC.resolve_program sctx ~dir "mirage" ~loc:None
   ~hint:"try: opam install mirage"

   let atom ~loc str = Dune_lang.Ast.Atom (loc, Dune_lang.Atom.of_string str)

   let list ~loc elts = Dune_lang.Ast.List (loc, elts)

   let executable ~modes ~loc ~name ~modules ~libraries = let atom = atom ~loc
   and list = list ~loc in list [ atom "executable" ; list [ atom "name"; atom
   name ] ; list (atom "modules" :: List.map ~f:atom modules) ; list (atom
   "libraries" :: List.map ~f:atom libraries) ; list (atom "modes" :: List.map
   ~f:atom modes) ]

   let rule ~loc ~targets ~run = let atom = atom ~loc and list = list ~loc in
   list [ atom "rule" ; list (atom "targets" :: List.map ~f:atom targets) ; list
   [ atom "action"; list (atom "run" :: List.map ~f:atom run) ] ]

   let alias ~loc ~name ~deps = let atom = atom ~loc and list = list ~loc in
   list [ atom "alias" ; list [ atom "name"; atom name ] ; list (atom "deps" ::
   List.map ~f:atom deps) ] *)

let config_exe ~dir = Ok (Path.relative (Path.build dir) "config.exe")

(* generate config.exe *)
let gen_config_exe ~sctx ~dir (stanza : Dune_file.Mirage.t) =
  let program =
    { Exe.Program.name = stanza.config
    ; main_module_name = Module_name.of_string stanza.config
    ; loc = stanza.loc
    }
  in
  let scope = Super_context.find_scope_by_dir sctx dir in
  let obj_dir = Obj_dir.make_exe ~dir ~name:".mirage" in
  let modules =
    let impl =
      Module.File.make Dialect.ocaml
        (Path.relative (Path.build dir) (stanza.config ^ ".ml"))
    in
    Modules.singleton_exe
      (Module.of_source ~visibility:Visibility.Private ~kind:Module.Kind.Impl
         (Module.Source.make ~impl program.main_module_name))
  in
  let requires =
    let loc = stanza.loc in
    let db = Scope.libs scope in
    let open Result.O in
    (loc, Lib_name.of_string_exn ~loc:(Some loc) "mirage")
    |> Lib.DB.resolve db
    >>| (fun libs -> [ libs ])
    >>= Lib.closure ~linking:true
  in
  let flags =
    let project = Scope.project scope in
    let dune_version = Dune_project.dune_version project in
    Ocaml_flags.default ~dune_version ~profile:(Super_context.profile sctx)
  in
  let cctx =
    let expander = Super_context.expander sctx ~dir in
    Compilation_context.create () ~super_context:sctx ~expander ~scope ~obj_dir
      ~modules ~opaque:false
      ~requires_link:(lazy requires)
      ~requires_compile:requires ~flags ~js_of_ocaml:None ~dynlink:false
      ~package:None
  in
  Exe.build_and_link ~program ~linkages:[ Exe.Linkage.native ] ~promote:None
    cctx

let obj_dir dir = Path.Build.relative dir ".mirage"

let config_libs dir name = Path.Build.relative (obj_dir dir) (name ^ ".libs")

let config_pkgs dir name = Path.Build.relative (obj_dir dir) (name ^ ".packages")

(* generate <context>.libs files *)
let gen_query_files ~sctx ~dir (stanza : Dune_file.Mirage.t) =
  List.iter
    ~f:(fun (loc, name, args) ->
      let args =
        List.map ~f:(fun (_, k, v) -> Printf.sprintf "--%s=%s" k v) args
      in
      let add_rule kind stdout_to =
        SC.add_rule sctx ~dir ~loc
          (Command.run (config_exe ~dir) ~dir:(Path.build dir)
             [ As ("query" :: kind :: args) ]
             ~stdout_to)
      in
      add_rule "libraries" (config_libs dir name);
      add_rule "packages" (config_pkgs dir name);
      let main = Path.Build.relative dir "main.ml" in
      SC.add_rule sctx ~dir ~loc
        (Command.run (config_exe ~dir) ~dir:(Path.build dir)
           [ A "config"; Hidden_targets [ main ]; As args ]))
    stanza.contexts

(* generate <context>.exe *)
let gen_context_exe ~sctx ~dir (stanza : Dune_file.Mirage.t) =
  let scope = SC.find_scope_by_dir sctx dir in
  let db = Scope.libs scope in
  let dune_version = Dune_project.dune_version (Scope.project scope) in
  List.iter
    ~f:(fun (loc, name, _args) ->
      let dyn_flags =
        let+ libs = Build.lines_of (Path.build (config_libs dir name)) in
        let exes = [ (loc, name) ] in
        let deps =
          List.map
            ~f:(fun l ->
              Lib_dep.direct (loc, Lib_name.of_string_exn ~loc:(Some loc) l))
            libs
        in
        let compile =
          Lib.DB.resolve_user_written_deps_for_exes db exes deps ~pps:[]
            ~optional:false ~variants:None ~dune_version
        in
        let lib_config = Context.lib_config (SC.context sctx) in
        let mode = Link_mode.Byte in
        let link = Lazy.force (Lib.Compile.requires_link compile) in
        let link = Result.ok_exn link in
        Lib.L.compile_and_link_flags ~compile:link ~link ~mode ~lib_config
      in
      let compiler = (SC.context sctx).ocamlc in
      let target = Path.Build.relative dir (name ^ ".exe") in
      let args =
        let open Command.Args in
        [ Dep (Path.relative (Path.build dir) "main.ml")
        ; A "-o"
        ; Target target
        ; Dyn dyn_flags
        ]
      in
      let action = Command.run ~dir:(Path.build dir) (Ok compiler) args in
      SC.add_rule ~dir sctx action)
    stanza.contexts

let gen_depends_alias ~sctx ~dir (stanza : Dune_file.Mirage.t) =
  let duniverse = SC.resolve_program sctx ~loc:None ~dir "duniverse" in
  let alias = Alias.make ~dir (Alias.Name.of_string "depends") in
  List.iter
    ~f:(fun (loc, name, _args) ->
      let pkgs = Build.lines_of (Path.build (config_libs dir name)) in
      let init =
        Command.run ~dir:(Path.build dir) duniverse
          [ A "init"; A "--pull-mode=source"; Command.Args.dyn pkgs ]
      in
      let pull =
        Command.run ~dir:(Path.build dir) duniverse [ A "pull"; A "--no-cache" ]
      in
      let action = Build.progn [ init; pull ] in
      SC.add_alias_action sctx ~dir ~loc:(Some loc) alias action ~stamp:name)
    stanza.contexts

let gen_rules ~sctx ~dir stanza =
  gen_config_exe ~sctx ~dir stanza;
  gen_query_files ~sctx ~dir stanza;
  gen_depends_alias ~sctx ~dir stanza;
  gen_context_exe ~sctx ~dir stanza
