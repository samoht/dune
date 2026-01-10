open Import

(** Global cache for dev tools.

    This module provides caching for dev tools to enable:
    - Fast reinstall after dune clean (cache survives clean)
    - Cross-workspace sharing (same binary used across projects)

    Cache structure: [~/.cache/dune/tools/{cache_key}/bin/{exe_name}]

    Cache keys:
    - Compiler-independent tools: [{package_name}.{version}]
    - Compiler-dependent tools: [{package_name}.{version}-{ocaml_version}-{build_id_short}]

    The build_id ensures that tools are recompiled if the OCaml compiler
    was built differently (different configure options, dependencies, etc). *)

(** OCaml compiler info for cache key computation *)
type ocaml_compiler_info =
  { version : string (** e.g., "5.2.0" *)
  ; build_id : Dune_digest.t (** Hash of compiler package + dependencies *)
  }

(** Compute the cache key for a dev tool.
    Returns [{package_name}.{version}] for compiler-independent tools,
    or [{package_name}.{version}-{ocaml_version}-{build_id_short}] for
    compiler-dependent tools. *)
val cache_key
  :  dev_tool:Dune_pkg.Dev_tool.t
  -> version:string
  -> ocaml_compiler:ocaml_compiler_info option
  -> string

(** Get the cache directory for a dev tool *)
val cache_dir
  :  dev_tool:Dune_pkg.Dev_tool.t
  -> version:string
  -> ocaml_compiler:ocaml_compiler_info option
  -> Path.Outside_build_dir.t

(** Get the path to a specific executable in the cache *)
val exe_path
  :  dev_tool:Dune_pkg.Dev_tool.t
  -> version:string
  -> ocaml_compiler:ocaml_compiler_info option
  -> Path.Outside_build_dir.t

(** Check if a dev tool is already installed in the cache.
    Returns true if the executable exists. *)
val is_installed
  :  dev_tool:Dune_pkg.Dev_tool.t
  -> version:string
  -> ocaml_compiler:ocaml_compiler_info option
  -> bool

(** Get the cache directory path (as Path.t) *)
val cache_dir_path
  :  dev_tool:Dune_pkg.Dev_tool.t
  -> version:string
  -> ocaml_compiler:ocaml_compiler_info option
  -> Path.t

(** Copy built dev tool to global cache.
    This should be called after the dev tool is successfully built.
    [source_dir] is the target/ directory containing the built tool. *)
val populate_cache
  :  dev_tool:Dune_pkg.Dev_tool.t
  -> version:string
  -> ocaml_compiler:ocaml_compiler_info option
  -> source_dir:Path.t
  -> unit
