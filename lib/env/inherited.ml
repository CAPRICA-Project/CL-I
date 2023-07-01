(** Inherited environment module *)


module Name_map = Map.Make(String)


(** Inherited attribute module *)
module Attr = struct
  (** Symbol type, among type symbol or regular symbol. Symbols of two different kinds MUST NOT
      share the same id. *)
  type t = Tsym of int | Sym of int
  (** Get the symbol id *)
  let to_int = function Tsym i | Sym i -> i
  (** Compare two symbols *)
  let compare s s' = compare (to_int s) (to_int s')
  (** Convert an attribute into a string *)
  let to_string = function Tsym i -> [%string "T#%{i#Int}"] | Sym i -> [%string "#%{i#Int}"]
end


(** Environment type. An inherited entironment is a map of attributes. *)
type t = Attr.t Name_map.t

(** Empty inherited environment *)
let empty = Name_map.empty

(** Check whether an attribute exists in the environment *)
let mem = Name_map.mem

let find = Name_map.find

(** Merge two inherited environments *)
let merge = Name_map.merge Util.merge

(** Inject a name map into an inherited environment *)
let inject = merge

(** Fold over the environment *)
let fold = Name_map.fold

(** Convert an inherited environment into a string *)
let to_string env =
  let mappings = Name_map.fold (fun name sym acc -> (name ^ " => " ^ Attr.to_string sym)::acc)
                               env [] in
  "[" ^ String.concat "; " (List.rev mappings) ^ "]"
