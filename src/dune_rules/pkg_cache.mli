open Import

(** Unified package cache for dune's package management.

    The secondary index at ~/.cache/dune/index/ provides cross-project
    sharing of built packages.

    Cache keys:
    - Locked/Toolchain: <name>.<version>-<bid8>
    - Dev tool (compiler-independent): <name>.<version>
    - Dev tool (compiler-dependent): <name>.<version>-<ocaml_version>-<bid8>
    - Workspace/Vendored: not cached *)

(** Package types that determine caching behavior *)
module Pkg_type : sig
  type compiler_info =
    { ocaml_version : string
    ; ocaml_build_id : Dune_digest.t
    }

  type t =
    | Workspace (** Never cached - users may edit *)
    | Vendored (** Never cached - users may edit *)
    | Locked (** Cached: name.version-bid *)
    | Toolchain (** Cached: name.version-bid *)
    | Dev_tool of
        { compiler_dependent : bool
        ; compiler_info : compiler_info option
        }

  val to_dyn : t -> Dyn.t
end

(** {1 Core Cache Functions} *)

(** The secondary index directory: ~/.cache/dune/index/ *)
val index_dir : unit -> Path.Outside_build_dir.t

(** Compute the cache key for a package.
    Returns [None] for Workspace and Vendored packages (never cached).

    For dev tools, uses source_checksum if available (for sharing across projects),
    otherwise falls back to build_id (less sharing but more correct).

    @param pkg_type The type of package
    @param name Package name
    @param version Package version
    @param build_id Recursive build ID (hash of content + deps' build_ids)
    @param source_checksum Optional checksum of the package source (from opam url)
    @return [Some cache_key] or [None] if package is not cacheable *)
val cache_key
  :  pkg_type:Pkg_type.t
  -> name:Package.Name.t
  -> version:Package_version.t
  -> build_id:Dune_digest.t
  -> source_checksum:Dune_digest.t option
  -> string option

(** Check if a cached package exists in the secondary index.
    A package is cached if its symlink exists and points to a valid target. *)
val is_cached : cache_key:string -> bool

(** Get the path in the content-addressed store for a cached package.
    Returns the target of the symlink in the index. *)
val cached_content_path : cache_key:string -> Path.t option

(** Store a built package in the cache and create the secondary index symlink.

    @param cache_key The key under which to store
    @param source_dir The directory containing built artifacts
    @param content_digest Hash of the content for content-addressed storage *)
val store_in_cache
  :  cache_key:string
  -> source_dir:Path.t
  -> content_digest:Dune_digest.t
  -> unit

(** Create an action to populate build directories from cache.
    The action copies/links from the cached content to the target directories. *)
val populate_from_cache_action
  :  cache_key:string
  -> install_dir:Path.Build.t
  -> target_dir:Path.Build.t
  -> Dune_lang.Action.t

(** {1 Compatibility Modules} *)

(** Compatibility module for pkg_toolchain.mli interface.
    Provides the same interface as the existing [Pkg_toolchain] module
    for gradual migration. *)
module Toolchain : sig
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

  (** Returns the installation prefix for a toolchain package within the
      index directory. This will be something like
      [index/ocaml-base-compiler.5.2.1.XXXXXXXX/target] where
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
end

(** Dev tool cache module.
    Handles caching of dev tools like ocamlformat, odoc, etc. *)
module Dev_tool : sig
  (** Compute the cache key for a dev tool.
      Returns [Some key] or [None] if no cache key can be computed.

      Cache key format:
      - Compiler-independent tools (e.g., ocamlformat): [{name}.{version}-{checksum8}]
        Uses source_checksum from opam url for cross-project sharing.
      - Compiler-dependent tools (e.g., odoc): [{name}.{version}-{build_id8}]
        Uses the dev tool's recursive build_id which includes all deps (compiler, etc.).

      @param source_checksum The checksum from the opam url field (optional)
      @param build_id The recursive build_id from the lock file (optional) *)
  val cache_key
    :  dev_tool:Dune_pkg.Dev_tool.t
    -> version:string
    -> source_checksum:Dune_pkg.Checksum.t option
    -> build_id:Dune_digest.t option
    -> string option

  (** Get the cache directory for a dev tool *)
  val cache_dir
    :  dev_tool:Dune_pkg.Dev_tool.t
    -> version:string
    -> source_checksum:Dune_pkg.Checksum.t option
    -> build_id:Dune_digest.t option
    -> Path.Outside_build_dir.t option

  (** Get the path to a specific executable in the cache *)
  val exe_path
    :  dev_tool:Dune_pkg.Dev_tool.t
    -> version:string
    -> source_checksum:Dune_pkg.Checksum.t option
    -> build_id:Dune_digest.t option
    -> Path.Outside_build_dir.t option

  (** Check if a dev tool is already installed in the cache.
      Returns true if the executable exists. *)
  val is_installed
    :  dev_tool:Dune_pkg.Dev_tool.t
    -> version:string
    -> source_checksum:Dune_pkg.Checksum.t option
    -> build_id:Dune_digest.t option
    -> bool

  (** Get the cache directory path (as Path.t) *)
  val cache_dir_path
    :  dev_tool:Dune_pkg.Dev_tool.t
    -> version:string
    -> source_checksum:Dune_pkg.Checksum.t option
    -> build_id:Dune_digest.t option
    -> Path.t option

  (** Copy built dev tool to global cache.
      This should be called after the dev tool is successfully built.
      [source_dir] is the target/ directory containing the built tool. *)
  val populate_cache
    :  dev_tool:Dune_pkg.Dev_tool.t
    -> version:string
    -> source_checksum:Dune_pkg.Checksum.t option
    -> build_id:Dune_digest.t option
    -> source_dir:Path.t
    -> unit
end
