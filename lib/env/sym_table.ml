type t = Constraint.t Sym_map.t * int
let empty = (Sym_map.empty, 0)
let add k v (map, i) = (Sym_map.add k v map, i + 1)
let allocate (map, i) = (map, i + 1)
let next (_, i) = i
let replace k v (map, i) = (Sym_map.add k v map, i)
let find k (map, _) = Sym_map.find k map
let mem k (map, _) = Sym_map.mem k map
let iter f (map, _) = Sym_map.iter f map
let fold f (map, _) = Sym_map.fold f map
let to_string (map, _) =
  let mappings = Sym_map.fold (fun sym cons acc -> (Inherited.Attr.to_string sym ^ " => " ^ Constraint.to_string cons)::acc)
                               map [] in
  "[" ^ String.concat "; " (List.rev mappings) ^ "]"
let of_sym_map map =
  let i = Sym_map.fold (fun k _ m -> max (Inherited.Attr.to_int k) m) map 0 in
  (map, i+1)
