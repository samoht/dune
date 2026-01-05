open Stdune

(* Unified progress tracking for builds and package management. *)

module Target = struct
  type t =
    { name : string
    ; is_fetch : bool (* true if network I/O *)
    }

  let create ~name ~is_fetch = { name; is_fetch }
  let library name = { name; is_fetch = false }
  let package name = { name; is_fetch = false }
  let fetch name = { name; is_fetch = true }
end

type t =
  { mutable active : Target.t list
  ; mutable completed : int
  ; mutable total : int (* library/package count, known upfront *)
  ; mutable cached : int
  ; mutable failed : int
  ; mutable jobs : int
  }

let state = { active = []; completed = 0; total = 0; cached = 0; failed = 0; jobs = 0 }

let reset () =
  state.active <- [];
  state.completed <- 0;
  state.total <- 0;
  state.cached <- 0;
  state.failed <- 0;
  state.jobs <- 0
;;

let set_total n = state.total <- n
let set_jobs n = state.jobs <- n
let incr_cached () = state.cached <- state.cached + 1
let start_target target = state.active <- target :: state.active

let finish_target ~name =
  state.active
  <- List.filter state.active ~f:(fun (t : Target.t) -> not (String.equal t.name name));
  state.completed <- state.completed + 1
;;

let fail_target ~name =
  state.active
  <- List.filter state.active ~f:(fun (t : Target.t) -> not (String.equal t.name name));
  state.failed <- state.failed + 1
;;

(* Format progress line respecting terminal width *)
let pp ~max_width =
  let fetching, building =
    List.partition_map state.active ~f:(fun (t : Target.t) ->
      if t.is_fetch then Left t.name else Right t.name)
  in
  (* Format: "[3/15] Fetching: pkg1, pkg2 | Building: lib1 [8j]" *)
  let prefix =
    if state.total > 0 then sprintf "[%d/%d] " state.completed state.total else ""
  in
  let suffix = if state.jobs > 0 then sprintf " [%dj]" state.jobs else "" in
  let prefix_len = String.length prefix in
  let suffix_len = String.length suffix in
  let format_targets ~label targets available_width =
    if List.is_empty targets
    then None
    else (
      let label_len = String.length label in
      let remaining = available_width - label_len in
      if remaining <= 10
      then Some (sprintf "%s+%d" label (List.length targets))
      else (
        let rec fit acc len = function
          | [] -> List.rev acc, 0
          | name :: rest ->
            let name_len = String.length name in
            let sep_len = if List.is_empty acc then 0 else 2 in
            let new_len = len + sep_len + name_len in
            if new_len > remaining
            then List.rev acc, List.length rest + 1
            else fit (name :: acc) new_len rest
        in
        let shown, remaining_count = fit [] 0 targets in
        let names = String.concat ~sep:", " shown in
        if remaining_count > 0
        then Some (sprintf "%s%s, +%d more" label names remaining_count)
        else Some (sprintf "%s%s" label names)))
  in
  let available = max_width - prefix_len - suffix_len in
  let fetch_part = format_targets ~label:"Fetching: " fetching available in
  let build_part = format_targets ~label:"Building: " building available in
  let main_part =
    match fetch_part, build_part with
    | None, None -> "Waiting..."
    | Some f, None -> f
    | None, Some b -> b
    | Some f, Some b -> sprintf "%s | %s" f b
  in
  Pp.verbatim (prefix ^ main_part ^ suffix)
;;

(* Summary after completion *)
let summary_pp ~elapsed_secs =
  if state.failed > 0
  then Pp.textf "Failed after %.1fs (%d errors)" elapsed_secs state.failed
  else if state.cached > 0
  then
    Pp.textf
      "Done in %.1fs (%d cached, %d built)"
      elapsed_secs
      state.cached
      (state.completed - state.cached)
  else Pp.textf "Done in %.1fs" elapsed_secs
;;
