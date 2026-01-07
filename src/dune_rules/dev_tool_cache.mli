open Import

(** Global cache for dev tools.

    This module provides caching for dev tools to enable:
    - Fast reinstall after dune clean (cache survives clean)
    - Cross-workspace sharing (same binary used across projects)

    Cache structure: [~/.cache/dune/tools/{cache_key}/bin/{exe_name}]

    Cache keys:
    - Compiler-independent tools: [{package_name}.{version}]
    - Compiler-dependent tools: [{package_name}.{version}-{ocaml_version}] *)

(** Compute the cache key for a dev tool.
    Returns [{package_name}.{version}] for compiler-independent tools,
    or [{package_name}.{version}-{ocaml_version}] for compiler-dependent tools. *)
val cache_key
  :  dev_tool:Dune_pkg.Dev_tool.t
  -> version:string
  -> ocaml_version:string option
  -> string

(** Get the cache directory for a dev tool *)
val cache_dir
  :  dev_tool:Dune_pkg.Dev_tool.t
  -> version:string
  -> ocaml_version:string option
  -> Path.Outside_build_dir.t

(** Get the path to a specific executable in the cache *)
val exe_path
  :  dev_tool:Dune_pkg.Dev_tool.t
  -> version:string
  -> ocaml_version:string option
  -> Path.Outside_build_dir.t

(** Check if a dev tool is already installed in the cache.
    Returns true if the executable exists. *)
val is_installed
  :  dev_tool:Dune_pkg.Dev_tool.t
  -> version:string
  -> ocaml_version:string option
  -> bool

(** Get the cache directory path (as Path.t) *)
val cache_dir_path
  :  dev_tool:Dune_pkg.Dev_tool.t
  -> version:string
  -> ocaml_version:string option
  -> Path.t

(** Copy built dev tool to global cache.
    This should be called after the dev tool is successfully built.
    [source_dir] is the target/ directory containing the built tool. *)
val populate_cache
  :  dev_tool:Dune_pkg.Dev_tool.t
  -> version:string
  -> ocaml_version:string option
  -> source_dir:Path.t
  -> unit
