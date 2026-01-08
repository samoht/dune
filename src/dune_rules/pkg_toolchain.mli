open Import

(** The path to the directory that will contain all toolchain
    versions. Creates the directory if it doesn't already exist.
    Set to [Dune_util.cache_home_dir/toolchains]. *)
val base_dir : unit -> Path.Outside_build_dir.t

(** Dune will download and build the ocaml-base-compiler and
    ocaml-variants packages into a user-wide directory (shared among
    projects) rather than using the usual package management mechanism to
    install such packages. Currently compiler packages can't be installed
    by dune's package management features as the compiler is not
    relocatable, and this flag allows dune to workaround this problem
    providing an experience to users that is almost identical to dune
    installing the compiler packgae.

    When this flag is disabled, users of dune package management need to
    manage their compiler installation with opam or a system package
    manager, as compilers packages that would be installed by dune will
    not work correctly. *)
val is_compiler_and_toolchains_enabled : Package.Name.t -> bool

(** Returns the path to the directory containing the given package within the
    toolchain directory. This will be something like
    [base_dir/ocaml-base-compiler.5.2.1.XXXXXXXX] where
    XXXXXXXX is the package's build_id (recursive hash of opam content + deps). *)
val installation_prefix
  :  Dune_pkg.Pkg.t
  -> build_id:Dune_digest.t
  -> Path.Outside_build_dir.t

val install_roots
  :  prefix:Path.Outside_build_dir.t
  -> Path.Outside_build_dir.t Install.Roots.t

(** Check if a toolchain is already installed in the cache. Returns true
    if the install cookie exists, indicating a successful prior installation. *)
val is_installed : Dune_pkg.Pkg.t -> build_id:Dune_digest.t -> bool

(** Get the cache directory path for a toolchain package *)
val cache_dir : Dune_pkg.Pkg.t -> build_id:Dune_digest.t -> Path.t

(** Create an action that populates the shared install directory from the global
    cache. Copies cached contents to install_dir, and creates target_dir for
    dependency tracking. *)
val populate_from_cache_action
  :  Dune_pkg.Pkg.t
  -> build_id:Dune_digest.t
  -> install_dir:Path.Build.t
  -> target_dir:Path.Build.t
  -> Dune_lang.Action.t
