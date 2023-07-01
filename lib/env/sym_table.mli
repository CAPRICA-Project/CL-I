type t
val empty : t
val add : Inherited.Attr.t -> Constraint.t -> t -> t
val allocate : t -> t
val next : t -> int
val replace : Inherited.Attr.t -> Constraint.t -> t -> t
val find : Inherited.Attr.t -> t -> Constraint.t
val mem : Inherited.Attr.t -> t -> bool
val iter : (Inherited.Attr.t -> Constraint.t -> unit) -> t -> unit
val fold : (Inherited.Attr.t -> Constraint.t -> 'a -> 'a) -> t -> 'a -> 'a
val to_string : t -> string
val of_sym_map : Constraint.t Sym_map.t -> t
