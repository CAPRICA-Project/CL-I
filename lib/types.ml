type 'a sum_atom = Plus of 'a | Minus of 'a
and 'a sum = 'a sum_atom list

and 'a arg = 'a Arg.t
