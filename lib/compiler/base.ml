let compile (ast, _ref_sym) =
  let (_, _, v, _) = Sem.run ast in (* (Simplify.run ast ref_sym) in *)
  print_endline "*** Sem DONE ***";
  let order = Topo.process v#symbols in
  print_endline "*** Topo DONE ***";
  let _ = Z3lib.Processor.solve_constraints order v#symbols in
  print_endline "*** Z3 DONE ***"
