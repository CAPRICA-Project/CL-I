(** Inherited environment module *)


module Name_map : module type of Map.Make(String)


(** Inherited attribute module *)
module Attr : sig
  (** Symbol type, among type symbol or regular symbol. Symbols of two different kinds MUST NOT
      share the same id. *)
  type t = Tsym of int | Sym of int
  (** Get the symbol id *)
  val to_int : t -> int
  (** Compare two symbols *)
  val compare : t -> t -> int
  (** Convert an attribute into a string *)
  val to_string : t -> string
end


(** Environment type. An inherited entironment is a map of attributes. *)
(*
type t
*)
type t = Attr.t Name_map.t

(** Empty inherited environment *)
val empty : t

(** Check whether an attribute exists in the environment *)
val mem : string -> t -> bool

val find : string -> t -> Attr.t

(** Merge two inherited environments *)
val merge : t -> t -> t

(** Inject a name map into an inherited environment *)
val inject : Attr.t Name_map.t -> t -> t

(** Fold over the environment *)
val fold : (string -> Attr.t -> 'a -> 'a) -> t -> 'a -> 'a

(** Convert an inherited environment into a string *)
val to_string : t -> string
