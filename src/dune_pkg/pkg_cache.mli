open Import

(** Package source cache indexed by content checksum.

    Stores extracted package sources in a global cache directory
    (~/.cache/dune/pkg-sources/) indexed by their checksum. Uses hardlinks
    when possible to avoid duplication. *)

(** [cache_path ~checksum] returns the path where sources for a package with
    the given [checksum] are cached. *)
val cache_path : checksum:Checksum.t -> Path.t

(** [is_cached ~checksum] returns true if a package with the given checksum
    has its sources cached. *)
val is_cached : checksum:Checksum.t -> bool

(** [populate ~checksum ~f] ensures the cache entry for [checksum] exists.
    If not present, calls [f ~target] to fetch/extract sources to [target],
    then moves the result to the cache. Returns the cache path on success. *)
val populate
  :  checksum:Checksum.t
  -> f:(target:Path.t -> (unit, 'e) result Fiber.t)
  -> (Path.t, 'e) result Fiber.t

(** [get_or_fetch ~checksum ~target ~fetch] ensures sources are at [target].
    Checks the cache first; if not present, calls [fetch ~target] to populate
    both the cache and [target]. Uses hardlinks when possible. *)
val get_or_fetch
  :  checksum:Checksum.t
  -> target:Path.t
  -> fetch:(target:Path.t -> (unit, 'e) result Fiber.t)
  -> (unit, 'e) result Fiber.t
