# Display Backend Redesign

**Related documents:**
- [pkg-ux-design.md](pkg-ux-design.md) - CLI UX design for `dune pkg` commands

## Design Principle

**One API, backend decides presentation.**

Callers say "this event happened" - backend decides how to show it.

## API

```ocaml
module Console : sig
  type stage = Fetch | Build | Scan | Save

  (** Set total count (libs + pkgs + exes - coarse grained) *)
  val set_total : int -> unit

  val start : stage:stage -> name:string -> unit
  val finish : name:string -> unit
  val fail : name:string -> unit

  (** Log output for an activity - shown in verbose mode *)
  val log : name:string -> string -> unit

  (** Messages - always shown *)
  val message : User_message.t -> unit
  val error : User_message.t -> unit
end
```

### Usage

```ocaml
Console.set_total 42;  (* 10 pkgs to fetch + 32 libs to build *)

(* Fetching a package *)
Console.start ~stage:Fetch ~name:"ppxlib.0.33.0";
Console.log ~name:"ppxlib.0.33.0" "curl -L https://...";
Console.finish ~name:"ppxlib.0.33.0";

(* Building a dune package - shows compiler commands *)
Console.start ~stage:Build ~name:"ppxlib.0.33.0";
Console.log ~name:"ppxlib.0.33.0" "ocamlfind ocamlopt -c ppxlib.ml";
Console.finish ~name:"ppxlib.0.33.0";

(* Building a non-dune package - shows configure/make *)
Console.start ~stage:Build ~name:"zarith.1.14";
Console.log ~name:"zarith.1.14" "./configure";
Console.log ~name:"zarith.1.14" "make";
Console.finish ~name:"zarith.1.14";

(* Building a workspace library *)
Console.start ~stage:Build ~name:"stdune";
Console.log ~name:"stdune" "ocamlfind ocamlopt -c path.ml";
Console.log ~name:"stdune" "ocamlfind ocamlopt -c io.ml";
Console.finish ~name:"stdune";

(* Building a workspace executable *)
Console.start ~stage:Build ~name:"main.exe";
Console.log ~name:"main.exe" "ocamlfind ocamlopt -o main.exe ...";
Console.finish ~name:"main.exe"
```

## Backend Behaviors

| Mode     | start/finish                              | log            | message        |
|----------|-------------------------------------------|----------------|----------------|
| Quiet    | ignored                                   | ignored        | errors only    |
| Progress | status line: `[3/42] Fetching: a, b`      | ignored        | print + clear  |
| Short    | one line: `[3/42] Building mylib`         | ignored        | print          |
| Verbose  | one line: `[mylib] Building`              | `[mylib] cmd`  | print          |

Counter `[done/total]` is coarse-grained: libs + pkgs + exes (not individual rules/commands).

### Mode Descriptions

- **Quiet**: No output except errors
- **Progress**: Single status line, updated in-place, shows all active items
- **Short**: One line per event (Cargo-style) with progress counter
- **Verbose**: One line per event + log output, prefixed by context name

### Verbose Output Example

With concurrent operations, output is interleaved. The context prefix allows
understanding which activity each line belongs to:

```
[ppxlib.0.33.0] Fetching
[zarith.1.14] Fetching
[ppxlib.0.33.0] curl -L https://github.com/ocaml-ppx/ppxlib/...
[zarith.1.14] curl -L https://github.com/ocaml/zarith/...
[ppxlib.0.33.0] Done
[ppxlib.0.33.0] Building
[zarith.1.14] Done
[zarith.1.14] Building
[zarith.1.14] ./configure
[zarith.1.14] make
[ppxlib.0.33.0] ocamlfind ocamlopt -c ppxlib.ml
[zarith.1.14] Done
[ppxlib.0.33.0] Done
[stdune] Building
[stdune] ocamlfind ocamlopt -c path.ml
[stdune] ocamlfind ocamlopt -c io.ml
[dune_console] Building
[stdune] ocamlfind ocamlopt -a -o stdune.cmxa ...
[dune_console] ocamlfind ocamlopt -c progress.ml
[stdune] Done
[dune_console] ocamlfind ocamlopt -a -o dune_console.cmxa ...
[dune_console] Done
[main.exe] Building
[main.exe] ocamlfind ocamlopt -o main.exe ...
[main.exe] Done
```

## Backend Interface

```ocaml
type stage = Fetch | Build | Scan | Save

module type S = sig
  val init : unit -> unit
  val shutdown : unit -> unit
  val set_total : int -> unit
  val message : User_message.t -> unit
  val start : stage:stage -> name:string -> unit
  val finish : name:string -> unit
  val fail : name:string -> unit
  val log : name:string -> string -> unit
end
```

## Implementation Files

- `src/dune_console/backend_intf.ml` - interface definition
- `src/dune_console/quiet.ml` - errors only
- `src/dune_console/progress.ml` - status line
- `src/dune_console/short.ml` - line-by-line
- `src/dune_console/verbose.ml` - line-by-line + log
- `src/dune_console/dune_console.ml` - main API
- `src/dune_config_file/display.ml` - maps Display.t to backend
