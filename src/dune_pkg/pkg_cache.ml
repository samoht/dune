open Import
open Fiber.O

(* Package source cache indexed by content checksum.

   Cache layout: ~/.cache/dune/pkg-sources/<first-2-chars>/<checksum>/
   Each entry contains the extracted source directory for a package. *)

let ( / ) = Path.relative
let cache_root = lazy (Lazy.force Dune_util.cache_root_dir / "pkg-sources")

let cache_path ~checksum =
  let hex = Checksum.to_string checksum in
  let two_first = String.sub hex ~pos:0 ~len:2 in
  Lazy.force cache_root / two_first / hex
;;

let temp_dir = lazy (Lazy.force cache_root / "temp")

let ensure_cache_dir () =
  let dirs = [ Lazy.force cache_root; Lazy.force temp_dir ] in
  List.iter dirs ~f:(fun path ->
    match Fpath.mkdir_p (Path.to_string path) with
    | Created | Already_exists -> ()
    | exception Unix.Unix_error (err, _, _) ->
      User_error.raise
        [ Pp.textf
            "Failed to create package cache directory %s"
            (Path.to_string_maybe_quoted path)
        ; Pp.textf "Unix error: %s" (Unix.error_message err)
        ])
;;

(* Recursively hardlink a directory tree from src to dst.
   Falls back to copy if hardlink fails (e.g., cross-filesystem). *)
let rec hardlink_tree ~src ~dst =
  match (Unix.stat (Path.to_string src)).st_kind with
  | Unix.S_DIR ->
    (match Fpath.mkdir_p (Path.to_string dst) with
     | Created | Already_exists -> ()
     | exception Unix.Unix_error (err, _, _) ->
       User_error.raise
         [ Pp.textf "Failed to create directory %s" (Path.to_string_maybe_quoted dst)
         ; Pp.textf "Unix error: %s" (Unix.error_message err)
         ]);
    Path.readdir_unsorted_with_kinds src
    |> Result.value ~default:[]
    |> List.iter ~f:(fun (name, _kind) ->
      hardlink_tree ~src:(src / name) ~dst:(dst / name))
  | Unix.S_REG ->
    (try Fpath.link (Path.to_string src) (Path.to_string dst) with
     | Unix.Unix_error (Unix.EXDEV, _, _) ->
       (* Cross-filesystem: fall back to copy *)
       Io.copy_file ~src ~dst ()
     | Unix.Unix_error (Unix.EMLINK, _, _) ->
       (* Too many links: fall back to copy *)
       Io.copy_file ~src ~dst ()
     | Unix.Unix_error (Unix.EEXIST, _, _) ->
       (* Already exists, assume it's the same content *)
       ())
  | Unix.S_LNK ->
    (* Preserve symlinks *)
    let target = Unix.readlink (Path.to_string src) in
    (try Unix.symlink target (Path.to_string dst) with
     | Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  | _ -> ()
  | exception Unix.Unix_error _ -> ()
;;

let is_cached ~checksum = Path.exists (cache_path ~checksum)

(* Populate the cache entry for a package. Returns the cache path. *)
let populate ~checksum ~f =
  ensure_cache_dir ();
  let cache_entry = cache_path ~checksum in
  if Path.exists cache_entry
  then Fiber.return (Ok cache_entry)
  else (
    (* Fetch to a temp directory, then atomically move to cache *)
    let temp = Lazy.force temp_dir / Checksum.to_string checksum in
    let+ result = f ~target:temp in
    match result with
    | Error _ as e -> e
    | Ok () ->
      (try Unix.rename (Path.to_string temp) (Path.to_string cache_entry) with
       | Unix.Unix_error (Unix.ENOTEMPTY, _, _) | Unix.Unix_error (Unix.EEXIST, _, _) ->
         (* Another process populated the cache, use that *)
         (try Fpath.rm_rf (Path.to_string temp) with
          | _ -> ()));
      Ok cache_entry)
;;

(* Get from cache or fetch, then copy/hardlink to target *)
let get_or_fetch ~checksum ~target ~fetch =
  let* result = populate ~checksum ~f:fetch in
  match result with
  | Error _ as e -> Fiber.return e
  | Ok cache_path ->
    hardlink_tree ~src:cache_path ~dst:target;
    Fiber.return (Ok ())
;;
