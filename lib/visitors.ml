open Types

class virtual ['self] iter_ = object (self : 'self)

  method private visit_arg: 'env 'a .
    ('env -> 'a -> unit) -> 'env -> 'a arg -> unit
  = fun visit_'a env this ->
      match this with
      | Etc ->
          ()
      | Arg c0 ->
          visit_'a env c0

  method private visit_sum: 'env 'a .
    ('env -> 'a -> unit) -> 'env -> 'a sum_atom list -> unit
  = fun visit_'a env xs ->
      match xs with
      | [] ->
          ()
      | Plus c0 :: xs ->
          visit_'a env c0;
          self#visit_sum visit_'a env xs
      | Minus c0 :: xs ->
          visit_'a env c0;
          self#visit_sum visit_'a env xs
end

class virtual ['self] map_ = object (self : 'self)

  method private visit : 'env 'a_0 'a_1 .
    ('self -> 'env -> 'a_0 -> 'a_1 * 's) ->
    'env -> 'a_0 -> 'a_1
  = fun f env this -> let (node, _) = f self env this in node

  method private visit_arg: 'env 'a_0 'a_1 .
    ('env -> 'a_0 -> 'a_1) -> 'env -> 'a_0 arg -> 'a_1 arg
  = fun visit_'a env this ->
      match this with
      | Etc ->
          Etc
      | Arg c0 ->
          Arg (visit_'a env c0)

  method private visit_sum: 'env 'a_0 'a_1 .
    ('env -> 'a_0 -> 'a_1) -> 'env -> 'a_0 sum_atom list -> 'a_1 sum_atom list
  = fun visit_'a env xs ->
      match xs with
      | [] ->
          []
      | Plus c0 :: xs ->
          let r0 = visit_'a env c0 in
          let xs = self#visit_sum visit_'a env xs in
          Plus r0 :: xs
      | Minus c0 :: xs ->
          let r0 = visit_'a env c0 in
          let xs = self#visit_sum visit_'a env xs in
          Minus r0 :: xs
end

class virtual ['self] reduce_ = object (self : 'self)

  inherit ['s] VisitorsRuntime.monoid

  method private visit_arg : 'env 'a .
    ('env -> 'a -> 's) -> 'env -> 'a arg -> 's
  = fun visit_'a env this ->
      match this with
      | Etc ->
          self#zero
      | Arg c0 ->
          visit_'a env c0

  method private visit_sum : 'env 'a .
    ('env -> 'a -> 's) -> 'env -> 'a sum_atom list -> 's
  = fun visit_'a env xs ->
      match xs with
      | [] ->
          self#zero
      | Plus c0 :: xs ->
          let s0 = visit_'a env c0 in
          let sxs = self#visit_sum visit_'a env xs in
          self#plus s0 sxs
      | Minus c0 :: xs ->
          let  s0 = visit_'a env c0 in
          let sxs = self#visit_sum visit_'a env xs in
          self#plus s0 sxs
end

class virtual ['self] mapreduce_ = object (self : 'self)

  inherit ['s] VisitorsRuntime.monoid

  method private visit : 'env 'a_0 'a_1 .
    ('self -> 'env -> 'a_0 -> 'a_1 * 's) ->
    'env -> 'a_0 -> 'a_1 * 's
  = fun f -> f self

  method visit_arg : 'env 'a_0 'a_1 .
    ('env -> 'a_0 -> 'a_1 * 's) ->
    'env -> 'a_0 arg -> 'a_1 arg * 's
  = fun visit_'a env this ->
      match this with
      | Etc ->
          Etc, self#zero
      | Arg c0 ->
          let r0, s0 = visit_'a env c0 in
          Arg r0, s0

  method private visit_sum : 'env 'a_0 'a_1 .
    ('env -> 'a_0 -> 'a_1 * 's) ->
    'env -> 'a_0 sum_atom list -> 'a_1 sum_atom list * 's
  = fun visit_'a env xs ->
      match xs with
      | [] ->
          [], self#zero
      | Plus c0 :: xs ->
          let r0, s0 = visit_'a env c0 in
          let xs, sxs = self#visit_sum visit_'a env xs in
          Plus r0 :: xs, self#plus s0 sxs
      | Minus c0 :: xs ->
          let r0, s0 = visit_'a env c0 in
          let xs, sxs = self#visit_sum visit_'a env xs in
          Minus r0 :: xs, self#plus s0 sxs
end

let ($) f f' = f (fun _ -> f')

let arg :
  ('self -> 'env -> 'a_0 -> 'a_1 * 's) ->
  < visit_arg : 'env 'a_0 'a_1 .
      ('env -> 'a_0 -> 'a_1 * 's) ->
      'env -> 'a_0 arg -> 'a_1 arg * 's; .. > ->
  'env -> 'a_0 arg -> 'a_1 arg * 's
= fun f self env this -> self#visit_arg (f self) env this

let list :
  ('self -> 'env -> 'a_0 -> 'a_1 * 's) ->
  < visit_list : 'env 'a_0 'a_1 .
      ('env -> 'a_0 -> 'a_1 * 's) ->
      'env -> 'a_0 list -> 'a_1 list * 's; .. > ->
  'env -> 'a_0 list -> 'a_1 list * 's
= fun f self env this -> self#visit_list (f self) env this

let option :
  ('self -> 'env -> 'a_0 -> 'a_1 * 's) ->
  < visit_option : 'env 'a_0 'a_1 .
      ('env -> 'a_0 -> 'a_1 * 's) ->
      'env -> 'a_0 option -> 'a_1 option * 's; .. > ->
  'env -> 'a_0 option -> 'a_1 option * 's
= fun f self env this -> self#visit_option (f self) env this
