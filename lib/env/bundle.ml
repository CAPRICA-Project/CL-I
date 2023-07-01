module Name_map = Map.Make(String)


(** Synthetized environment module *)
module rec Synthetized : sig
  (** Synthetized attribute module *)
  module Attr : sig
    (** Attribute type, made of an inherited attribute and a lifetime *)
    type t = Inherited.Attr.t * Lifetime.t
    (** Wake an attribute, if possible *)
    val wake : t -> Inherited.Attr.t option
    (** Convert an attribute into a string *)
    val to_string : t -> string
  end
  (** TODO *)
  type a = (int * Constraint.t) Added.t
  type t = Attr.t Name_map.t * a
  val wake : Attr.t Name_map.t -> Inherited.t -> Inherited.t
  (** Convert a synthetized environment to an added element *)
  val to_added : t -> a
  (** Convert a synthetized environment into a string *)
  val to_string : t -> string
end = struct
  module Attr = struct
    type t = Inherited.Attr.t * Lifetime.t
    let wake = function
      | s, Lifetime.(Between (0, _) | From 0) -> Some s
      | _ -> None
    let to_string (a, l) = Inherited.Attr.to_string a ^ " " ^ Lifetime.to_string l
  end

  type a = (int * Constraint.t) Added.t
  type t = Attr.t Name_map.t * a

  let wake names ienv = Inherited.inject (Name_map.filter_map (fun _ -> Attr.wake) names) ienv
  (** Convert a synthetized environment to an added element *)
  let to_added (_, a) = a
  (** Convert a synthetized environment into a string *)
  let to_string (names, added) =
    let mappings = Name_map.fold (fun name sym acc -> (name ^ " => " ^ Attr.to_string sym)::acc)
                                 names [] in
    "[" ^ String.concat "; " (List.rev mappings) ^ "], " ^ Added.to_string added
end

and Constraint : sig
  type t = Inherited.t * Ast.rhs option * (Ast.rhs_ty option * Ast.rhs_ty list) * Ast.kind option
  val empty : t
  val of_untyped : Ast.rhs -> t
  val of_type : Ast.rhs_ty -> t
  val to_type : t -> Ast.rhs_ty
  val of_typed : Ast.rhs -> Ast.rhs_ty -> t
  val to_typed : t -> Ast.rhs * Ast.rhs_ty
  val merge_kinds : Ast.kind option -> Ast.kind option -> Ast.kind option
  val merge : (int -> t -> unit) -> t -> Synthetized.a -> Synthetized.a
  val to_string : t -> string
  val of_added : ('a * Constraint.t) Added.t -> t
  val of_singleton : Synthetized.t -> t
  val get_names : t -> Inherited.t
end = struct
  type t = Inherited.t * Ast.rhs option * (Ast.rhs_ty option * Ast.rhs_ty list) * Ast.kind option

  (** Empty constraint *)
  let empty = (Inherited.empty, None, (None, []), None)

  let of_untyped v = (Inherited.empty, Some v, (None, []), None)
  let of_type t = (Inherited.empty, None, (Some t, []), None)
  let to_type = function
    | (_, _, (Some t, []), _) -> t
    | _ -> failwith "to_type"
  let of_typed v t = (Inherited.empty, Some v, (Some t, []), None)
  let to_typed = function
    | (_, Some v, (Some t, []), _) -> (v, t)
    | _ -> failwith "to_typed"

  (** Merge two kind constraints *)
  let merge_kinds kcons kcons' = match kcons, kcons' with
  | Some k, None | None, Some k -> Some k (* One kind *)
  | Some k, Some k' when k = k' -> Some k (* Similar kinds *)
  | None, None -> None (* No kind *)
  | Some _, Some _ -> (* Different kinds *)
      raise (Exceptions.Kind_error (Mismatching, "Kinds do not match"))

  (** Merge two constraints *)
  let merge replace (names, value, (tcons:'a option*'b list), kcons) = function
    | Added.Node (i, (names', _, (_, ts' as tcons'), kcons')) ->
        (*XXX*)
        let _tcons = let (_t, _) = tcons in if Option.is_some _t then (_t, ts') else tcons' in
      (*
        let cons = (Name_map.merge merge names names', value, List.rev_append tcons tcons' (*FIXME*),
                    merge_kinds kcons kcons') in
      *)
        let cons = (Inherited.merge names names', value, _tcons, merge_kinds kcons kcons') in
        (*XXX*)
        replace i cons; Added.Node (i, cons)
    | _ -> failwith "Constraint.merge: NOT IMPLEMENTED"

  let to_string (names, _, _, _) =
    "(" ^ Inherited.to_string names ^ ")"

  let of_added a = let (_, tcons) = Added.to_node a in tcons
  let of_singleton (_, a) = of_added a
  let get_names (names, _, _, _) = names
end
