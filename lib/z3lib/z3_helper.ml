(** This module provides helper functions for Z3 *)


module String = struct
  let mk_sort = Z3.Seq.mk_string_sort
  let starts_with ctx str prefix = Z3.Seq.mk_seq_prefix ctx prefix str
  let ends_with ctx str suffix = Z3.Seq.mk_seq_suffix ctx suffix str
  let contains = Z3.Seq.mk_seq_contains
  let concat ctx str str' = Z3.Seq.mk_seq_concat ctx [str; str']
  let mk = Z3.Seq.mk_string
end

module Integer = struct
  include Z3.Arithmetic.Integer
  let mk = mk_numeral_i
end

(** Declare a sort *)
let sort = Z3.Sort.mk_uninterpreted_s
(** Declare a function *)
let func = Z3.FuncDecl.mk_func_decl_s
(** Declare a constant *)
let const = Z3.Expr.mk_const_s
(** Call a function *)
let call = Z3.Expr.mk_app
(** Build a pattern *)
let pattern_of ctx = function
  | [] -> []
  | l -> [Z3.Quantifier.mk_pattern ctx l]
(** Build a ∀ quantifier *)
let forall ctx ?(patterns=[]) constants expr =
  Z3.Quantifier.(expr_of_quantifier (mk_forall_const ctx constants expr None (pattern_of ctx patterns) [] None None))
(** Build a ∃ quantifier *)
let exists ctx ?(patterns=[]) constants expr =
  Z3.Quantifier.(expr_of_quantifier (mk_exists_const ctx constants expr None (pattern_of ctx patterns) [] None None))
(** Create an implication *)
let implies = Z3.Boolean.mk_implies
(** If-then-else *)
let ifte = Z3.Boolean.mk_ite
(** And operator *)
let and_ = Z3.Boolean.mk_and
(** Or operator *)
let or_ = Z3.Boolean.mk_or
(** Assert distinct values *)
let distinct = Z3.Boolean.mk_distinct
(** Not operator *)
let not = Z3.Boolean.mk_not
(** Assert equal values *)
let eq = Z3.Boolean.mk_eq


(** Module describing relation properties *)
module Relation_property = struct
  (** Relation types *)
  type t = Symmetric | Asymmetric | Antisymmetric | Total | Reflexive | Irreflexive | Transitive
         | Equivalence | Preorder | Total_preorder | Total_order | Strict_total_order
         | Partial_order | Strict_partial_order | Strict_weak_order
  (** Convert a primitive relation type into an integer *)
  let to_int = function
    | Symmetric -> 0
    | Antisymmetric -> 1
    | Total -> 2
    | Reflexive -> 3
    | Irreflexive -> 4
    | Transitive -> 5
    | Strict_weak_order -> 6
    | _ -> assert false
  (** Convert a primitive relation type into z3 axioms *)
  let to_z3 ctx rel sort sort' =
    let a = const ctx "__a" sort in
    let b = const ctx "__b" sort' in
    let c = const ctx "__c" sort' in
    let r a b = call ctx rel [a; b] in
    function
      | Symmetric -> forall ctx [a; b] (eq ctx (r a b) (r b a))
      | Antisymmetric -> forall ctx [a; b] (implies ctx (and_ ctx [r a b; r b a]) (eq ctx a b))
      | Total -> forall ctx [a; b] (or_ ctx [r a b; r b a; eq ctx a b])
      | Reflexive -> forall ctx [a] (r a a)
      | Irreflexive -> forall ctx [a] (not ctx (r a a))
      | Transitive -> forall ctx [a; b; c] (implies ctx (and_ ctx [r a b; r b c]) (r a c))
      | Strict_weak_order ->
          forall ctx [a; b; c] (implies ctx (not ctx (or_ ctx [r a b; r b a; r b c; r c b]))
                                            (not ctx (or_ ctx [r a c; r c a])))
      | _ -> assert false
  (** Compare two relations. Useful to store a set of relations. *)
  let compare rel rel' = compare (to_int rel) (to_int rel')
end


(** Set of relations *)
module Relation_set = Set.Make (Relation_property)


(** Module describing relations *)
module Relation = struct
  include Relation_property
  (** Convert a relation type into a string *)
  let to_string = function
    | Symmetric -> "Symmetric"
    | Asymmetric -> "Asymmetric"
    | Antisymmetric -> "Antisymmetric"
    | Total -> "Total"
    | Reflexive -> "Reflexive"
    | Irreflexive -> "Irreflexive"
    | Transitive -> "Transitive"
    | Equivalence -> "Equivalence"
    | Preorder -> "Preorder"
    | Total_preorder -> "Total_preorder"
    | Total_order -> "Total_order"
    | Strict_total_order -> "Strict_total_order"
    | Partial_order -> "Partial_order"
    | Strict_partial_order -> "Strict_partial_order"
    | Strict_weak_order -> "Strict_weak_order"
  (** Convert a string into a relation type *)
  let of_string = function
    | "Symmetric" -> Symmetric
    | "Asymmetric" -> Asymmetric
    | "Antisymmetric" -> Antisymmetric
    | "Total" -> Total
    | "Reflexive" -> Reflexive
    | "Irreflexive" -> Irreflexive
    | "Transitive" -> Transitive
    | "Equivalence" -> Equivalence
    | "Preorder" -> Preorder
    | "Total_preorder" -> Total_preorder
    | "Total_order" -> Total_order
    | "Strict_total_order" -> Strict_total_order
    | "Partial_order" -> Partial_order
    | "Strict_partial_order" -> Strict_partial_order
    | "Strict_weak_order" -> Strict_weak_order
    | _  -> assert false
  (** Expand a relation into a set of primitive relations *)
  let expand = function
    | Asymmetric -> Relation_set.of_list [Antisymmetric; Irreflexive]
    | Equivalence -> Relation_set.of_list [Transitive; Symmetric; Reflexive]
    | Preorder -> Relation_set.of_list [Transitive; Reflexive]
    | Total_preorder -> Relation_set.of_list [Transitive; Total; Reflexive]
    | Total_order -> Relation_set.of_list [Transitive; Antisymmetric; Total; Reflexive]
    | Strict_total_order -> Relation_set.of_list [Transitive; Total; Irreflexive; Antisymmetric]
    | Partial_order -> Relation_set.of_list [Transitive; Antisymmetric; Reflexive]
    | Strict_partial_order -> Relation_set.of_list [Transitive; Irreflexive; Antisymmetric]
    | Strict_weak_order ->
        Relation_set.of_list [Transitive; Irreflexive; Asymmetric; Strict_weak_order]
    | relty -> Relation_set.singleton relty
  (** Create a relation set from a list of properties *)
  let create = List.fold_left (fun s r -> Relation_set.union s (expand r)) Relation_set.empty
  (** Convert a relation into z3 assertions *)
  let to_z3 ctx rel sort sort' properties =
    Relation_set.fold (fun property assertions -> (to_z3 ctx rel sort sort' property)::assertions)
                      (create properties) []
end


(** Generative functor to create a solver instance *)
module Make () = struct
  module R = Relation
  include Z3
  module Relation = R
  module Sort = struct
    include Z3.Sort
    let to_string sort = Z3.Sort.to_string sort |> Str.(global_replace (regexp "^|\\||$") "")
  end
  let ctx = mk_context []
  let string = String.mk_sort ctx
  module String = struct
    let starts_with = String.starts_with ctx
    let starts_with_ = function str::prefix::[] -> starts_with str prefix | _ -> raise (Invalid_argument "starts_with_")
    let ends_with = String.ends_with ctx
    let ends_with_ = function str::suffix::[] -> ends_with str suffix | _ -> raise (Invalid_argument "ends_with_")
    let contains = String.contains ctx
    let contains_ = function container::containee::[] -> contains container containee | _ -> raise (Invalid_argument "contains_")
    let concat = String.concat ctx
    let concat_ = function str::str'::[] -> concat str str' | _ -> raise (Invalid_argument "concat_")
    let mk = String.mk ctx
  end
  let integer = Integer.mk_sort ctx
  module Integer = struct
    include Integer
    let mk = mk ctx
  end
  module Arithmetic = struct
    include Arithmetic
    let mk_lt = mk_lt ctx
    let mk_lt_ = function e::e'::[] -> mk_lt e e' | _ -> raise (Invalid_argument "mk_lt_")
    let mk_gt = mk_gt ctx
    let mk_gt_ = function e::e'::[] -> mk_gt e e' | _ -> raise (Invalid_argument "mk_gt_")
  end
  let sort = sort ctx
  let func = func ctx
  let const = const ctx
  let anonymous_const ?(suffix="") sort = const (Sort.to_string sort ^ suffix) sort
  let anonymous_const' = anonymous_const ~suffix:"'"
  let call = call ctx
  let forall = forall ctx
  let exists = exists ctx
  let implies = implies ctx
  let (=>) = implies
  let implies_ = function
    | hd::hd'::[] -> implies hd hd'
    | _ -> raise (Invalid_argument "implies_")
  let ifte = ifte ctx
  let ifte_ = function
    | hd::hd'::hd''::[] -> ifte hd hd' hd''
    | _ -> raise (Invalid_argument "ifte_")
  let and_ = and_ ctx
  let (&&) first other = and_ [first; other]
  let or_ = or_ ctx
  let (||) first other = or_ [first; other]
  let distinct = distinct ctx
  let (!=) first other = distinct [first; other]
  let not = not ctx
  let (!) = not
  let not_ = function expr::[] -> not expr | _ -> raise (Invalid_argument "not_")
  let eq = eq ctx
  let eq_ = function
    | hd::hd'::[] -> eq hd hd'
    | _ -> raise (Invalid_argument "eq_")
  let (==) = eq
  let boolean = Boolean.mk_sort ctx
  let solver () = Solver.mk_solver ctx None
  let add_assertions = Solver.add
  let add_assertion solver assertion = Solver.add solver [assertion]
  let relation name sort sort' properties =
    let rel = func name [sort; sort'] boolean in
    (rel, Relation.to_z3 ctx rel sort sort' properties)
  let add_relation solver name sort sort' properties =
    let (rel, assertions) = relation name sort sort' properties in
    add_assertions solver assertions;
    rel
  let true_ = Boolean.mk_true ctx
  let false_ = Boolean.mk_false ctx
  let mk_integer = Integer.mk
  let (^) = String.concat
end
