module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type key
  type t
  (** permutacja jako funkcja *)
  val apply : t -> key -> key
  (** permutacja identycznościowa *)
  val id : t
  (** permutacja odwrotna *)
  val invert : t -> t
  (** permutacja która tylko zamienia dwa elementy miejscami *)
  val swap : key -> key -> t
  (** złożenie permutacji (jako złożenie funkcji) *)
  val compose : t -> t -> t
  (** porównywanie permutacji *)
  val compare : t -> t -> int
end

module Make(Key : OrderedType) = struct
  module Map = Map.Make(Key)

  type key = Key.t
  type t = {
    fwd : key Map.t;
    inv : key Map.t
  }

  let apply perm key =
    try (Map.find key perm.fwd) with
    | Not_found -> key

  let id : t = { fwd = Map.empty; inv = Map.empty }

  let invert perm : t = { fwd = perm.inv; inv = perm.fwd }

  let swap k1 k2 : t = 
    if k1 = k2 then id else { 
    fwd = Map.add k1 k2 (Map.add k2 k1 id.fwd); 
    inv = Map.add k2 k1 (Map.add k1 k2 id.inv) 
    }

  let compose perm1 perm2 =
    let merge_fun m _ v1 v2 =
      match v2 with
      | None -> v1
      | Some x -> match Map.find_opt x m with
        | None -> Some x
        | Some y -> Some y
    in
    {
      fwd = Map.merge (merge_fun perm1.fwd) perm1.fwd perm2.fwd;
      inv = Map.merge (merge_fun perm2.inv) perm2.inv perm1.inv
    }

  let compare perm1 perm2 = Map.compare Key.compare perm1.fwd perm2.fwd

end