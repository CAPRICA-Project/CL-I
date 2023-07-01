module Symset = Set.Make(Int)
open Types

type vulnerability = string
and kind = Resource | Service | Component | Protocol
and role = string
and template = Tuid of string
             | Tcons of rhs_ty

and protocol_sup = unit

and tag = string

and 'param ty = { ty_name: string; ty_params: ('param arg list option [@opaque]);
                  ty_sub: rhs_ty list }

and lhs_ty = Ltype of (template ty [@opaque])
           | Lttuple of lhs_ty option arg list

and cardinality = From of rhs | To of rhs | From_to of rhs * rhs

and rhs_ty = Fun of { fun_implicit: rhs list; fun_args: rhs_ty option arg list;
                      fun_ret: rhs_ty option }
           | Adj of { adj_implicit: rhs list; adj_arg: rhs_ty option arg option }
           | Rtype of (rhs_ty option ty [@opaque])
           | Word of string
           | Tbuilt_in of string
           | Rttuple of rhs_ty list
           | Rtlist of { ty: rhs_ty; cardinality: cardinality list }
           | Tagged of { tagged_ty: rhs_ty; tagged_tag: tag sum }
           | Template of string
           | Unit
           | Tunknown
           | Tref of int
           | Type_of of int
           | Text of string
           | TBoolean
           | TInteger
           | TString
           | Cmp of cmp
           | Rrtdot of ((rhs, rhs_ty) dotted [@opaque])
           | Rtrtdot of ((rhs_ty, rhs_ty) dotted [@opaque])
and handler = { hdl_obj: rhs; hdl_alias: string option; hdl_evt: string option; hdl_action: expr }
and ('a, 'b) dotted = { dot_obj: ('a [@opaque]); dot_mbr: ('b [@opaque]) }

and string_part = Plain of string | Expr of string

and 'a args = { call_obj: ('a [@opaque]); call_args: (rhs option arg list [@opaque]) }

and rhs = Call of { call_obj: rhs; call_args: rhs option arg list }
        | Tcall of { call_obj: rhs_ty; call_args: rhs option arg list }
        | Adj_call of { adj_name: string; adj_arg: rhs }
        | Rsym of string
        | Rstring of string
        | Rstrcat of rhs list
        | Sym_def of int * rhs
        | Ref of int
        | Ext of string
        | Smth
        | Unknown
        | Rcons of { rcons_obj: rhs; rcons_tag: tag sum option; rcons_ty: rhs_ty option }
        | Rtuple of rhs list
        | Rlist of rhs list
        | Rinit of { init_obj: rhs; init_body: init list }
        | Rrdot of ((rhs, rhs) dotted [@opaque])
        | Rtrdot of ((rhs_ty, rhs) dotted [@opaque])
        | CInst of construct_body option * rhs list
        | Properties of string list
        | Rlitexpr of string
        | Built_in of string
        | Runit
        | Z3_forall of z3_scope
        | Z3_exists of z3_scope
        | Boolean of bool
        | Integer of int

and lhs = Lsym of string
        | Lcons of { lcons_obj: lhs; lcons_ty: rhs_ty }
        | Lfun of { lfun_obj: lhs; lfun_args: lhs option arg list; lfun_ret: rhs_ty option }
        | Ltuple of lhs list
        | Lalt of string
        | Lidx of rhs
        | Lmore
        | Llist
        | Llitexpr of string
        | Lref of int
        | Lsome_of of int

and init = { set_obj: rhs; set_value: rhs }

and z3_scope = { z3_decl: lhs list; z3_body: rhs list }

and struct_ = Def of { def_obj: lhs; def_value: rhs option }
            | Def_type of { deftype_obj: lhs_ty; deftype_ty: rhs_ty option }
            | Init of init
            | Rexpr of rhs
            | Nop

and expr = rhs list

and cmp = { cmp_args: lhs arg list; cmp_body: construct_body option; cmp_ann: rhs_ty list;
            cmp_hooks: rhs list }

and construct_body = CEq of rhs
                   | CBody of struct_ list

and t = struct_ list [@@deriving visitors { variety = "iter"; ancestors = ["Visitors.iter_"];
                                            polymorphic = false },
                                 visitors { variety = "map"; ancestors = ["Visitors.map_"];
                                            polymorphic = false },
                                 visitors { variety = "reduce"; ancestors = ["Visitors.reduce_"];
                                            polymorphic = false },
                                 visitors { variety = "mapreduce"; ancestors = ["Visitors.mapreduce_"];
                                            polymorphic = false }]
