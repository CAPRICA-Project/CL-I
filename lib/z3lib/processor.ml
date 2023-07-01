open Ast
module Ienv = Env.Inherited
module Int_set = Set.Make(Int)

let solve_constraints order syms =
  let module Z3 = Z3_helper.Make () in
  Z3.set_global_param "tactic.default_tactic" "smt";
  Z3.set_global_param "sat.core.minimize" "true";
  Z3.set_global_param "smt.qi.profile" "true";
  Z3.set_global_param "smt.mbqi.max_iterations" "5000";
(*
  Z3.set_global_param "smt.mbqi.trace" "true";
*)
  Z3.set_global_param "solver.smtlib2_log" "solver.log";
  Z3.set_global_param "smt.mbqi.max_cexs" "6";
  let solver = Z3.solver () in

  let rec sort_name_of_ty = function
    | Tref i -> Ienv.Attr.to_string (Tsym i)
    | TBoolean -> "Bool"
    | TString -> "String"
    | Tbuilt_in t -> t
    | TInteger -> "Int"
    | Text s -> s
    | Type_of i ->
        let (_, _, (ty, _), _) = Env.Sym_table.find (Sym i) syms in
        sort_name_of_ty (Option.get ty)
    | Rtlist { ty; _ } -> sort_name_of_ty ty ^ "*" (* TODO: cardinality *)
    | _ -> failwith "sort_name_of_ty: Not implemented" in
  let sort_of_ty ty = Z3.sort (sort_name_of_ty ty) in

  let rec sort_of_ref i =
    let sym = Ienv.Attr.Sym i in
    let (_, _, (ty, _), _) = Env.Sym_table.find sym syms in
    match ty with
      | Some ((Tref _ | Rtlist _) as t) -> sort_of_ty t
      | Some (Type_of j) -> sort_of_ref j
      | None -> Ienv.Attr.to_string sym |> Z3.sort
      | Some TBoolean -> Z3.boolean
      | Some TString -> Z3.string
      | Some TInteger -> Z3.integer
      | Some (Text x | Tbuilt_in x) -> Z3.sort x
      | _ -> failwith (Ienv.Attr.to_string sym ^ ": Internal error") in

  let const_of_ref i =
    let sym = Ienv.Attr.Sym i in
    Z3.const (Ienv.Attr.to_string sym) (sort_of_ref i) in

  let wrap f args = ([f args], []) in
  let assert_ args = ([], [List.hd args]) in

  let vtable = Hashtbl.create 10 in
  Hashtbl.add vtable "&:Bool,Bool" (wrap Z3.and_);
  Hashtbl.add vtable "|:Bool,Bool" (wrap Z3.or_);
  Hashtbl.add vtable "=>:Bool,Bool" (wrap Z3.implies_);
  Hashtbl.add vtable "=>:Bool,Bool,Bool" (wrap Z3.ifte_);
  Hashtbl.add vtable "!:Bool" (wrap Z3.not_);
  Hashtbl.add vtable "=:," (wrap Z3.eq_);
  Hashtbl.add vtable "!=:," (wrap Z3.distinct);
  Hashtbl.add vtable "assert:Bool" assert_;
  Hashtbl.add vtable "String\\starts_with:String,String" (wrap Z3.String.starts_with_);
  Hashtbl.add vtable "String\\ends_with:String,String" (wrap Z3.String.ends_with_);
  Hashtbl.add vtable "String\\contains:String,String" (wrap Z3.String.contains_);
  Hashtbl.add vtable "Math\\+:String,String" (wrap Z3.String.concat_);
  Hashtbl.add vtable "Math\\<:Int,Int" (wrap Z3.Arithmetic.mk_lt_);
  Hashtbl.add vtable "Math\\>:Int,Int" (wrap Z3.Arithmetic.mk_gt_);
  Hashtbl.add vtable "Math\\=>:Bool,Bool" (wrap Z3.implies_);

  let rel name sort sort' =
    let z3_name = name ^ ":" ^ Z3.Sort.to_string sort ^ "," ^ Z3.Sort.to_string sort' in
    let f = Z3.func z3_name [sort; sort'] Z3.boolean |> Z3.call in
    Hashtbl.add vtable z3_name (wrap f);
    f in

  let create_has_can_have l v =
    let (sort, sort') = Z3.Expr.(get_sort l, get_sort v) in
    let has = rel "<has" sort sort' in
    let can_have = rel "<can have" sort sort' in
    let (cst, cst') = (Z3.anonymous_const sort, Z3.anonymous_const sort') in
    let modal = Z3.(forall [cst; cst'] (has [cst; cst'] => can_have [cst; cst'])) in
    (has [l; v], can_have [l; v], [modal]) in

  let create_has = function
    | l::v::[] -> let (has, _, asserts) = create_has_can_have l v in ([has], asserts)
    | _ -> failwith "Unreachable" in

  let create_can_have = function
    | l::v::[] -> let (_, can_have, asserts) = create_has_can_have l v in ([can_have], asserts)
    | _ -> failwith "Unreachable" in

  Hashtbl.add vtable "<has:," create_has;
  Hashtbl.add vtable "<can have:," create_can_have;

  let vtable_find_opt name sort_names = match Hashtbl.find_opt vtable (name ^ ":" ^ String.concat "," sort_names) with
    | Some _ as f -> f
    | None -> Hashtbl.find_opt vtable (name ^ ":" ^ String.make (List.length sort_names - 1) ',') in

  let vtable_find name sort_names = vtable_find_opt name sort_names |> Option.get in

  let vtable_add name sort_names = Hashtbl.add vtable (name ^ ":" ^ String.concat "," sort_names) in

  let attr name sort sort' =
    let z3_name = "attr!" ^ Z3.Sort.to_string sort ^ "!" ^ name in
    let f = Z3.call (Z3.func z3_name [sort] sort') in
    let w = function
      | cst::cst'::[] -> Z3.eq (f [cst]) cst'
      | _ -> assert false in
    Hashtbl.add vtable z3_name (wrap w);
    let cst = Z3.anonymous_const sort in
    let cst' = Z3.anonymous_const sort' in
    Z3.(forall [cst] (exists [cst'] (f [cst] == cst'))) in

  let get_attr name sort_name = Hashtbl.find vtable ("attr!" ^ sort_name ^ "!" ^ name) in

  let v = object (self)
    inherit [_] reduce as super
    method! visit_list = super#visit_list
    method! visit_option = super#visit_option
    val mutable fatal = false

    method zero = ([], [])
    method plus (consts, assertions) (consts', assertions') = (consts @ consts', List.rev_append assertions assertions')
    method merge (consts, assertions) = List.rev_append consts assertions

    method visit_rhs' () = function
      | Ref i -> ([const_of_ref i], [])
      | Call { call_obj; call_args } -> self#visit_Call () call_obj call_args
      | Z3_forall z -> self#visit_Z3_forall () z
      | Z3_exists z -> self#visit_Z3_exists () z
      | _ -> if fatal then failwith "visit_rhs': Internal error" else self#zero

    method! visit_Tcall () _ _ = self#zero

    method! visit_Call () call_obj call_args =
      let (args, assertions) = self#visit_list (self#visit_arg (self#visit_option self#visit_rhs'))
                                               () call_args in
      let sorts = List.map (fun arg -> Z3.Expr.get_sort arg) args in
      let sort_names = List.map Z3.Sort.to_string sorts in
      let rec fn_of = function
        | Ref i -> begin
            let (_, ro, (ty, _), _) = Env.Sym_table.find (Sym i) syms in
            match ro with
            | Some rhs -> fn_of rhs
            | None ->
                let name = Ienv.Attr.to_string (Sym i) in
                begin match (vtable_find_opt name sort_names) with
                  | Some z3f -> (z3f, [])
                  | None -> begin match ty with
                      | Some (Text s) -> begin match (String.split_on_char '\\' s) with
                          | "Z3"::rel::[] ->
                              let (sort, sort') = match sorts with
                                | hd::hd'::[] -> (hd, hd')
                                | _ -> failwith "Only relations are supported" in
                              let relty = Z3.Relation.of_string rel in
                              let (z3r, assertions) = Z3.relation (name ^ ":" ^ String.concat "," sort_names) sort sort' [relty] in
                              let z3w = wrap (Z3.call z3r) in
                              vtable_add name sort_names z3w;
                              (z3w, assertions)
                          | _ -> failwith "The Z3 context cannot execute non-Z3 unvalued routines"
                        end
                      | _ -> failwith "The Z3 context cannot execute non-Z3 unvalued routines"
                    end
                end
          end
        | Ext s -> begin match (String.split_on_char '\\' s) with
            | "Z3"::l -> (vtable_find (String.concat "\\" l) sort_names, [])
            | _ -> if fatal then failwith ("The Z3 context can only execute Z3 routines. Got: " ^ s) else ((fun _ -> self#zero), [])
          end
        | Built_in s -> fn_of (Ext ("Z3\\" ^ s))
        | _ -> if fatal then failwith (failwith "Call: not implemented") else ((fun _ -> self#zero), []) in
      let (fn, assertions') = fn_of call_obj in
      self#plus (fn args) ([], List.rev_append assertions' assertions)

    method additional_rules ?(create_attrs=false) const names =
      Env.Name_map.fold
        (fun k v acc -> match k, v with
           | "this", _ | _, Ienv.Attr.Tsym _ -> acc
           | _ when String.starts_with ~prefix:"<" k -> acc
           | _, Sym j ->
               let sort = Z3.Expr.get_sort const in
               let asserts = if create_attrs then [attr k sort (sort_of_ref j)] else [] in
               let const' = const_of_ref j in
               let name = "attr!" ^ Z3.Sort.to_string sort ^ "!" ^ k in
(*XXX*)
(*
               let (consts, asserts') = self#visit_lhs' () (Lref j) in
*)
               let (asserts'', asserts''') = (Hashtbl.find vtable name) [const; const'] in
(*
               (self#plus (consts, List.rev_append asserts''' (List.rev_append asserts'' (List.rev_append asserts' asserts))) acc))
*)
               (self#plus ([const'], List.rev_append asserts''' (List.rev_append asserts'' asserts)) acc))
        names self#zero

    method visit_lhs' () = function
      | Lref i ->
          let const = const_of_ref i in
          let (names, _, _, _) = Env.Sym_table.find (Sym i) syms in
          (const, self#additional_rules const names)
      | Lsome_of i ->
          let sort = sort_of_ty (Tref i) in
          let const = Z3.anonymous_const sort in
          let (names, _, _, _) = Env.Sym_table.find (Tsym i) syms in
          (const, self#additional_rules const names)
      | _ -> failwith "visit_z3_decl: Internal error"

    method! visit_Ref () i = ([const_of_ref i], [])

    method visit_z3_decl (d,(c,a)) = function
      | hd::tl ->
          let (decl,(c',a')) = self#visit_lhs' () hd in
          self#visit_z3_decl (decl::d, (List.rev_append c' c, List.rev_append a' a)) tl
      | [] -> (d,(c,a))

    method! visit_Z3_forall () { z3_decl; z3_body } =
      let is_fatal = fatal in
      fatal <- true;
(*
      let (consts, conds) = self#visit_list self#visit_lhs' () z3_decl in
*)
      let (decl, (consts, conds)) = self#visit_z3_decl ([],self#zero) z3_decl in
      let body = Z3.and_ (self#merge (self#visit_list self#visit_rhs () z3_body)) in
      fatal <- is_fatal;
      ([Z3.forall (List.rev_append decl consts) (Z3.implies (Z3.and_ conds) body)], [])

    method! visit_Z3_exists () { z3_decl; z3_body } =
      let is_fatal = fatal in
      fatal <- true;
(*
      let (consts, conds) = self#visit_list self#visit_lhs' () z3_decl in
*)
      let (decl, (consts, conds)) = self#visit_z3_decl ([],self#zero) z3_decl in
      let body = Z3.and_ (self#merge (self#visit_list self#visit_rhs () z3_body)) in
      fatal <- is_fatal;
      ([Z3.exists decl (Z3.forall consts (Z3.implies (Z3.and_ conds) body))], [])
  end in

  let visit_symbol sym (names, rhs, (ty, _), _) =
    let ty_properties = match sym with
      | Ienv.Attr.Tsym i -> ([], Ienv.fold
          (fun name sym acc -> match name, sym with
             | "this", _ | _, Ienv.Attr.Tsym _ -> acc
             | _ when String.starts_with ~prefix:"<" name -> acc
             | _, Sym j -> attr name (sort_of_ty (Tref i)) (sort_of_ref j)::acc)
          names [])
      | Sym i ->
          let names_properties = Ienv.fold
            (fun name sym acc -> match name, sym with
               | "this", _ | _, Ienv.Attr.Tsym _ -> acc
               | _ when String.starts_with ~prefix:"<" name -> acc
               | _, Sym j ->
                  let attr = get_attr name (sort_name_of_ty (Option.get ty)) in
                  v#plus (attr [const_of_ref i; const_of_ref j]) acc)
            names ([], []) in
          v#plus names_properties begin match rhs with
            | Some (Rlist l) ->
                let sort_name = sort_name_of_ty (Option.get ty) in
                assert (String.ends_with ~suffix:"*" sort_name);
                let sort_name' = String.sub sort_name 0 (String.length sort_name - 1) in
                let has = vtable_find "<has" [sort_name; sort_name'] in
                let (consts, asserts) = v#visit_list v#visit_rhs () l in
                let cst = Z3.const (Ienv.Attr.to_string sym) (Z3.sort sort_name) in
                let cst' = Z3.anonymous_const (Z3.sort sort_name') in
                let (has_c, has_a) = has [cst; cst'] in
                let ass = Z3.(forall [cst'] (List.hd has_c => or_ (List.map (fun c -> cst' == c) consts))) in
                let zenv = List.fold_left (fun acc expr -> v#plus (has [cst; expr]) acc) ([], ass::has_a) consts in (*XXX*)
                v#plus zenv ([], asserts)
            | Some (Rstring s) -> ([], [Z3.eq (const_of_ref i) (Z3.String.mk s)])
            | Some (Boolean true) -> ([], [Z3.eq (const_of_ref i) Z3.true_])
            | Some (Boolean false) -> ([], [Z3.eq (const_of_ref i) Z3.false_])
            | Some (Integer j) -> ([], [Z3.eq (const_of_ref i) (Z3.mk_integer j)])
            | Some (Ref j) -> ([], [Z3.eq (const_of_ref i) (const_of_ref j)])
            | Some (Call { call_obj; call_args }) -> begin
                let (consts, asserts) = v#visit_Call () call_obj call_args in
                match consts with
                | hd::[] -> ([], [Z3.eq (const_of_ref i) hd])
                | [] -> ([], asserts)
(*
                | [] -> begin match asserts with
                    | _::[] as l -> ([], l)
                    | _ -> failwith "Call: Unsupported 1"
                  end
*)
                | _ -> failwith "Call: Unsupported 2"
              end
            | _ -> v#visit_option v#visit_rhs () rhs
          end in
    v#merge ty_properties in
  let properties = List.fold_left (fun acc sym -> List.rev_append (visit_symbol sym (Env.Sym_table.find sym syms)) acc) [] order in
(*
  List.iter (fun e ->
let ast = Z3.Expr.ast_of_expr e in
print_endline (string_of_int (Z3.AST.get_id ast));
print_endline (Z3.AST.to_string ast)) properties;
*)
  match Z3.Solver.check solver properties with
  | Z3.Solver.UNSATISFIABLE ->
      print_endline "UNSAT";
(*
      print_endline "--- DEBUG ---";
      Z3.Solver.get_unsat_core solver |> List.iter (fun e ->
let ast = Z3.Expr.ast_of_expr e in
print_endline (string_of_int (Z3.AST.get_id ast));
print_endline (Z3.AST.to_string ast));
      print_endline "---(UNSAT)---";
*)
  | Z3.Solver.SATISFIABLE ->
      print_endline "SAT";
(*
      print_endline "--- DEBUG ---";
      Z3.Solver.get_model solver |> Option.get |> Z3.Model.to_string |> print_endline;
      print_endline "----(SAT)----";
      print_endline "-------------";
*)
  | Z3.Solver.UNKNOWN ->
      print_endline "UNKNOWN";
