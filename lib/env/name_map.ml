include Map.Make(String)
let swap map = fold (fun x y -> Sym_map.add y x) map (Sym_map.empty)
