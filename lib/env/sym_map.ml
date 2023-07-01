include Map.Make(Inherited.Attr)
let of_list l = of_seq (List.to_seq l)
