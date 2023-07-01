open Ast
open Exceptions
open Util
open Visitors
module Cons = Env.Constraint
module Ienv = Env.Inherited
module Senv = Env.Synthetized
module Name_map = Env.Name_map
module Name_set = Set.Make(String)
module Int_map = Map.Make(Int)


(** Evaluation context *)
type context = Top | Z3


(** Semantic pass
   @param ast The parsed AST
   @return (names, v) *)
let run ast =
  (** Semantic pass visitors *)
  let v = object (self)
    inherit [_] mapreduce as super

    (** The symbol table *)
    val mutable symbols = Env.Sym_table.of_sym_map Built_in.symbols
    method symbols = symbols

    (** The evaluation context *)
    val mutable context = Top

    (** Inherited type constructors mapping visitors *)
    method! visit_list = super#visit_list
    method! visit_option = super#visit_option

    (** List reducing visitor *)
    method private visit_list' :
      'a 'b . ('ienv->'a->'b*'senv) -> 'ienv -> 'a list -> 'b list * 'senv =
      fun f ienv -> function
        | [] -> ([], self#zero)
        | hd::tl ->
            let (hd, (names, _ as senv)) = f ienv hd in
            let ienv = Senv.wake names ienv in
            let (tl, senv') = self#visit_list' f ienv tl in
            (hd::tl, self#plus senv senv')


    (** Shift a name map given an association map. This will explore the symbols to replace
        references and, if needed, create new symbols.
        @param assoc The association map (Sym a -> Sym a'; Sym b -> Sym b')
        @param names The name map (value -> Sym a; value' -> Sym b)
        @return The updated name map *)
    method shift ?(replace=false) assoc names =
      let v' = object (self')
        inherit [_] map

        (** Visit a constraint *)
        method visit_cons key name (ienv, names) (names', value, (ty, tys), kind) = (*XXX*)
          (* Visit the value *)
          let value = self'#visit_option self'#visit_rhs ienv value in
          let cons = (names', value, (ty, tys), kind) in
          match replace with
          | true ->
              self#replace key cons;
              (ienv, names)
          | false -> (* Create a new value *)
              let sym = Ienv.Attr.Sym (self#add_val cons) in
              (Env.Sym_map.add key sym ienv, Name_map.add name sym names)

        (** Visit a reference *)
        method! visit_Ref ienv i = match Env.Sym_map.find_opt (Ienv.Attr.Sym i) ienv with
          | Some (Sym j) -> (* Shift the reference *)
              Ref j
          | None -> (* Keep the reference *)
              Ref i
          | Some _ -> raise (Internal_failure (Not_processed, "shift/Ref"))

        method! visit_Tref = raise (Internal_failure (Not_allowed, "shift/Tref"))
      end in

      (** Shift a name *)
      let shift_name name symbol (assoc, symbols) =
        match Env.Sym_map.find_opt symbol assoc with
          | Some sym -> (* If an association is found, replace the symbol *)
              (assoc, Name_map.add name sym symbols)
          | None -> (* Else, explore the data structure *)
              let cons = self#get_sym symbol in
              v'#visit_cons symbol name (assoc, symbols) cons in

      let (rev_names, symbols) =
        Name_map.fold
          (fun k v (acc, acc') ->
             (Env.Sym_map.add v k acc, Env.Sym_table.add v (self#get_symbol v) acc'))
          names (Env.Sym_map.empty, Env.Sym_table.empty) in
      let order = Topo.process symbols |> List.filter (fun v -> Env.Sym_map.mem v rev_names) in
      let (_, names) = List.fold_left (fun acc v -> shift_name (Env.Sym_map.find v rev_names) v acc)
                                      (assoc, names) order in
      names
    (* shift *)


    (** Add a new value in the symbol table and return its index *)
    method add_val cons =
      let k = Env.Sym_table.next symbols in
      symbols <- Env.Sym_table.add (Sym k) cons symbols;
      k

    (** Add a new type in the symbol table and return its index *)
    method private add_ty cons =
      let k = Env.Sym_table.next symbols in
      symbols <- Env.Sym_table.add (Tsym k) cons symbols;
      k

    (** Replace a symbol *)
    method replace key value = symbols <- Env.Sym_table.replace key value symbols

    (** Replace a value from the symbol table by another one *)
    method private replace_val ?(shift=Int_map.empty) key value =
      let sym = match Int_map.find_opt key shift with
        | Some i -> i
        | None -> key in
      symbols <- Env.Sym_table.replace (Sym sym) value symbols

    (** Replace a type from the symbol table by another one *)
    method private replace_ty key ty = symbols <- Env.Sym_table.replace (Tsym key) ty symbols

    method get_symbol sym = Env.Sym_table.find sym symbols

    (** Get a value from the symbol table *)
    method get_val key = self#get_symbol (Sym key)

    (** Get a type from the symbol table *)
    method get_ty key = self#get_symbol (Tsym key)


    (** Allocate a new value in the symbol table and return its index *)
    method private allocate_val =
      let k = Env.Sym_table.next symbols in
      symbols <- Env.Sym_table.allocate symbols;
      k


    (** Get a symbol from the symbol table *)
    method private get_sym sym = Env.Sym_table.find sym symbols

    method deref = function
      | Ref i ->
          let (_, ro, _, _) = self#get_val i in
          self#deref (Option.get ro)
      | rhs -> rhs

    method pointer_to = function
      | Ienv.Attr.Sym i ->
          let (names, value, ty, kind) = self#get_val i in
          let ty = if Env.Sym_table.mem (Tsym i) symbols then ty else (Some (Type_of i), []) in
          let tcons = (names, value, ty, kind) in
          let j = self#add_val tcons in
          Ienv.Attr.Sym j
      | _ -> failwith "pointer_to: Internal error"

    method map_pointer_to ?(exclude=Name_set.empty) names =
      Env.Sym_map.fold
        (fun r name acc ->
           let r = if (Name_set.mem name exclude || String.starts_with ~prefix:"<" name || name = "this") then r else self#pointer_to r in
           Name_map.add name r acc)
        (Name_map.swap names) Name_map.empty

    method add_this ?(lifetime=Lifetime.at 1) ?sym senv =
      let sym = match sym with
        | Some sym -> sym
        | None -> let (i, _) = Senv.to_added senv |> Added.to_node in Ienv.Attr.Sym i in
      let this = (Name_map.singleton "this" (sym, lifetime), Added.None) in
      self#plus senv (self#lapse this)

    method sym_of_lref = function
      | Lref i -> Ienv.Attr.Sym i
      | _ -> failwith "sym_of_lref: Internal error"

    method private solve_name names sym =
      match Name_map.find_opt sym names with
        | Some (Ienv.Attr.Sym i) ->
            let (_, senv) = self#visit_Ref Name_map.empty i in
            Some (i, Cons.of_singleton senv)
        | None -> None
        | _ -> failwith "UNREACHABLE"

    method private solve_val names sym =
      match context with
        | Top -> begin match self#solve_name names sym with
            | Some node -> node
            | None -> begin match Built_in.solve_val sym ||| Math.solve_val sym with
                | Some (names, _, _, kind as tcons) ->
                    let i = self#add_val tcons in
                    (i, (names, Some (Ref i), (Some (Type_of i), []), kind))
                | None -> raise (Value_error (Not_found, sym))
              end
          end
        | Z3 -> begin match self#solve_name names sym with (* TODO: prevent asserts?? *)
            | Some node -> node
            | None -> begin match Z3lib.Cl1lib.solve_val sym ||| Math.solve_val sym with
                | Some (names, _, _, kind as tcons) ->
                    let i = self#add_val tcons in
                    (i, (names, Some (Ref i), (Some (Type_of i), []), kind))
                | None -> raise (Value_error (Not_found, sym))
              end
          end

    method private solve_ty_name names ty_name =
      match Name_map.find_opt ty_name names with
        | Some (Ienv.Attr.Tsym i) ->
            let (_, senv) = self#visit_Tref Name_map.empty i in
            Some (i, Cons.of_singleton senv) (* TODO?: indirection *)
        | None -> None
        | _ -> failwith "UNREACHABLE"

    method solve_ty names ty_name =
      match self#solve_ty_name names ty_name with
      | Some node -> node
      | None -> begin match Built_in.solve_ty ty_name ||| Z3lib.Cl1lib.solve_ty ty_name with
          | Some tcons -> (-1, tcons)
          | None -> raise (Type_error (Not_found, ty_name))
        end

    method private merge_names (names, added) names' =
      match added with
        | Added.Node (i, (names'', value, tys, kind)) -> (names, Added.Node (i, (Ienv.inject names'' names', value, tys, kind)))
        | Added.None -> assert (Name_map.is_empty names'); (names, added)
        | _ -> failwith "merge_names: Not implemented"

    method plus (names, added) (names', added') =
      (Name_map.merge merge names' names,
       match added, added' with
         | Added.None  , a        -> a
         | a      , Added.None    -> a
         | Added.List l, Added.List l' -> Added.List (l@l')
         | Added.List l, _        -> Added.List (l@[added'])
         | _      , Added.List l  -> Added.List (added::l)
         | _      , _        -> Added.List [added; added'])

    method zero = (Name_map.empty, Added.None)

    method singleton node =
      (Name_map.empty, Added.Node node)

    method private lapse (names, added) =
      (Name_map.filter_map
         (fun _ -> function
            | _, Lifetime.Between (0, 0)            -> None
            | _, Lifetime.Between (i, j) when i > j -> raise (Sanity_error 2)
            | s, Lifetime.Between (i, j)            -> Some (s, Lifetime.Between (max (i-1) 0, max (j-1) 0))
            | s, (Lifetime.From 0 as l)             -> Some (s, l)
            | s, Lifetime.From i                    -> Some (s, Lifetime.From (i-1)))
         names, added)

    method private extract names =
      Name_map.filter_map
        (fun _ -> function
           | s, Lifetime.Between (0, 0)            -> Some s
           | _, Lifetime.Between (i, j) when i > j -> raise (Sanity_error 2)
           | s, Lifetime.From 0                    -> Some s
           | _                                     -> None)
        names

    (** Add a new type to the symbol map and return the proper summary *)
    method private add_ty_cons ~lifetime name cons =
  (*XXX*)
      let i = self#add_ty cons in
      (Name_map.singleton name (Ienv.Attr.Tsym i, lifetime), Added.Node (i, cons))

    (** Add a new empty value to the symbol map and return the proper summary *)
    method private add_empty_val ~lifetime symbols =
      let cons = Cons.empty in
      let i = self#add_val cons in
      let names = List.fold_left (fun acc name -> Name_map.add name
                                                  (Ienv.Attr.Sym i, lifetime) acc)
                                 Name_map.empty symbols in
      (names, Added.Node (i, cons))

    method private extract_subnames = function
      | [] -> Name_map.empty
      | Tref i::tl -> let (names,_,_,_) = self#get_ty i in Ienv.inject names (self#extract_subnames tl)
      | _ -> failwith "extract_subnames: Not implemented"

    method private extract_subnames' = function
      | [] -> Name_map.empty
      | Ref i::tl -> let (names,_,_,_) = self#get_val i in Ienv.inject names (self#extract_subnames' tl)
      | _ -> failwith "extract_subnames': Not implemented"

    (*----- VISITORS -----*)

    (** Template visitor *)
    (** UNCHECKED!!! *)
    method! visit_template ienv = function
      | Tuid t ->
          (* Type_1<Ty_1, Ty_2=Type_2, `Word`>
                    ^^^^                       *)
          (Tuid t, self#add_ty_cons ~lifetime:Lifetime.now t Cons.empty)
      | Tcons (Word _ as c) ->
          (* Type_1<Ty_1, Ty_2=Type_2, `Word`>
                                       ^^^^^^  *)
          let (c, senv) = self#visit_rhs_ty ienv c in
          (Tcons c, senv)
      | Tcons _ -> raise Not_implemented

    (** LHS visitor *)
    method visit_lhs' ?(redef=true) ?(lifetime=Lifetime.at 1) ienv = function
      | Lsym s ->
          (** Add the symbol to the name map and initialize it with no constraint or value *)
          (* let (value : Type)
                  ^^^^^         *)
          if not redef && Ienv.mem s ienv then raise (Value_error (Redefined, s)) else
          let (_, added as senv) = self#add_empty_val ~lifetime [s] in
          let (i, _) = Added.to_node added in
          (Lref i, self#add_this ~lifetime senv)

      | Llitexpr s ->
          (** Add the symbol to the name map and initialize it with no constraint or value *)
          (* let <<expr>>
                 ^^^^^^^^ *)
          let s = "<" ^ s in
          if not redef && Ienv.mem s ienv then raise (Value_error (Redefined, s)) else
          let (_, added as senv) = self#add_empty_val ~lifetime:Lifetime.eternal [s] in
          let (i, _) = Added.to_node added in
          (Lref i, senv)

      | Lcons { lcons_obj; lcons_ty } ->
          (** Add a constraint to a symbol *)
          (* let (value : Type)
                 ^^^^^^^^^^^^^^ *)

          (* let (value : Type)
                  ^^^^^         *)
          let (lcons_obj, (names, added)) = self#visit_lhs' ~redef ~lifetime ienv lcons_obj in

          (* let (value : Type)
                          ^^^^  *)
          (* The synthetized environment is not relevant *)
          let (lcons_ty, _) = self#visit_rhs_ty ienv lcons_ty in

          let tcons = if context = Z3
            then match lcons_ty with
              | Tref i ->
                  let (names', _, _, kind) = self#get_ty i in
                  let (j, _) = Added.to_node added in
                  let names' = self#map_pointer_to names' |> Name_map.add "this" (Ienv.Attr.Sym j) in
                  (names', None, (Some (Tref i), []), kind)
              | (Text _ | Tbuilt_in _ | TBoolean | TString | TInteger | Rtlist _) as t -> (*FIXME*)
                  (Name_map.empty, None, (Some t, []), None)
              | _ -> failwith "Z3 Lcons: Not implemented"
            else Cons.of_type lcons_ty in
          (lcons_obj, (names, Cons.merge self#replace_val tcons added)) (*XXX*)

      | Ltuple l ->
          (* Recursively visit the tuple *)
          (* let (value_1, value_2, value_3)
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^ *)
          let (l, senv) = self#visit_list (self#visit_lhs' ~redef ~lifetime) ienv l in
          (Ltuple l, senv)

      | Lalt a ->
          (* let[] value
                   ^^^^^ *)
          if not redef && Ienv.mem a ienv then raise (Value_error (Redefined, a)) else
          (Lalt a, self#add_empty_val ~lifetime [a; ""])

      | Lidx i ->
          (* [index]
             ^^^^^^^ *)
          self#visit_Lidx ienv i

      | Lmore ->
          (* [+]
             ^^^ *)
          self#visit_Lmore ienv

      | Llist ->
          (* []
             ^^ *)
          self#visit_Llist ienv

      | Lfun _ -> failwith "NOT IMPLEMENTED"

      | Lref i ->
          let tcons = Cons.of_type Unit in
          self#replace (Sym i) tcons;
          (Lref i, (Name_map.empty, Added.Node (i, tcons)))

      | Lsome_of _ -> failwith "NOT IMPLEMENTED"
    (* visit_lhs' *)


    method! visit_lhs = failwith "visit_lhs: Disabled"


    (** LHS type visitor *)
    method visit_lhs_ty' ?(redef=true) ?(lifetime=Lifetime.at 1) ienv = function
      | Ltype { ty_name; ty_params; ty_sub } ->
          (* type Type_1<Ty_1, Ty_2=Type_2, `Word`> <: Type_3
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ *)

          (* type Type_1<Ty_1, Ty_2=Type_2, `Word`> <: Type_3
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^           *)
          let (ty_params, senv') = self#visit (option (list (arg$ self#visit_template))) ienv
                                              ty_params in

          (* type Type_1<Ty_1, Ty_2=Type_2, `Word`> <: Type_3
                                                       ^^^^^^ *)
          let (ty_sub, (_, added)) = self#visit_list self#visit_rhs_ty ienv ty_sub in
          let subtypes = Added.to_list added in

          (* Collect subnames *)
          let names = List.fold_right
            (fun added names ->
               let (_, (names', _, _, _)) = Added.to_node added in
               Name_map.merge merge names' names (*XXX*))
            subtypes
            Name_map.empty in

          (* Create copies to allow for redefinitions *)

          let (assoc, names) = Name_map.fold
            (fun k v (assoc, names) -> match v with
               | Ienv.Attr.Sym i ->
                   let j = self#add_val (self#get_val i) in
                   (Env.Sym_map.add (Sym i) (Ienv.Attr.Sym j) assoc, Name_map.add k (Ienv.Attr.Sym j) names)
               | Tsym i ->
                   (assoc, Name_map.add k (Ienv.Attr.Tsym i) names))
            (Name_map.remove "this" names) (Env.Sym_map.empty, Name_map.empty) in

          let names = self#shift ~replace:true assoc names in

          (* type Type_1<Ty_1, Ty_2=Type_2, `Word`> <: Type_3
                  ^^^^^^                                      *)
          if not redef && Ienv.mem ty_name ienv then raise (Type_error (Redefined, ty_name)) else
          let cons = (names, None, (None, ty_sub), None) in
          let senv = self#add_ty_cons ~lifetime ty_name cons in

          (Ltype { ty_name; ty_params; ty_sub }, self#plus senv' (self#merge_names (self#add_this ~lifetime senv) names))

      | Lttuple l ->
          (* type (Type_1, Type_2, Type_3)
                  ^^^^^^^^^^^^^^^^^^^^^^^^ *)
          let (l, senv) = self#visit (list (arg (option$ self#visit_lhs_ty')))
                                                   ienv l in
          (Lttuple l, senv)


    (** Default LHS type visitor *)
    method! visit_lhs_ty _ = failwith "visit_lhs_ty: Disabled"


    (** Definition visitor *)
    method visit_struct_' ?(redef=true) ienv = function
      | Def { def_obj; def_value } ->
          (* let value_1 = value_2;
             ^^^^^^^^^^^^^^^^^^^^^^ *)
          self#visit_Def' ~redef ienv def_obj def_value
      | Def_type { deftype_obj; deftype_ty } ->
          (* type Type_1 = Type_2;
             ^^^^^^^^^^^^^^^^^^^^^ *)
          self#visit_Def_type' ~redef ienv deftype_obj deftype_ty
      | Init { set_obj; set_value } ->
          (* value_1 <- value_2;
             ^^^^^^^^^^^^^^^^^^^ *)
          self#visit_Init ienv { set_obj; set_value }
      | Rexpr rhs -> let _ = self#visit_Def ienv (Lref (self#allocate_val)) (Some rhs) in (Nop, self#zero)
      | Nop -> (Nop, self#zero)


    (** Default definition visitor *)
    method! visit_struct_ ienv = self#visit_struct_' ienv


    method match_ merge added added' =
      let rec match_rec = function
        | Added.Node _ as added, tcons -> Cons.merge merge tcons added
        | Added.List l, (_, Some (Rlist l'), (Some (Rtlist { ty; _ }), _), _) ->
            if List.length l = List.length l'
            then Added.List (List.map2 (fun a r -> match_rec (a, Cons.of_typed r ty)) l l')
            else failwith "Cannot pattern match"
        | Added.List l, (_, Some (Rtuple l'), (Some (Rttuple l''), _), _) ->
            if List.length l = List.length l'
            then Added.List (Util.map3 (fun a r t -> match_rec (a, Cons.of_typed r t)) l l' l'')
            else failwith "Cannot pattern match"
        | Added.List l, (_, _, (Some Rtlist { ty; cardinality }, _), _) -> begin
            match List.hd (List.rev cardinality) with
            | From_to (Integer i, Integer j) when i = j && List.length l = i ->
                Added.List (List.map (fun a -> match_rec (a, Cons.of_type ty)) l)
            | _ -> failwith "Cannot pattern match"
          end
        | Added.List l, (_, _, (Some Rttuple l', _), _) ->
            Added.List (List.map2 (fun a rt -> match_rec (a, Cons.of_type rt)) l l')
        | Added.List _, _ -> failwith "Cannot pattern match"
        | _ -> failwith "match_rec: Unreachable state" in

      match (added, added') with
      | Added.None, _ -> failwith "Ignored statement: Not implemented"
      | _, Added.None -> Added.node_map (Cons.merge merge (Cons.empty)) added
      | _, Added.Node (_, tcons) -> match_rec (added, tcons)
      | _ -> failwith "match_: Unreachable state"


    (** Value definition visitor *)
    method visit_Def' ?(redef=true) ienv def_obj def_value =
      (* let value_1 = value_2;
         ^^^^^^^^^^^^^^^^^^^^^^ *)

      (* let value_1 = value_2;
             ^^^^^^^            *)
      let (def_obj, (names, added)) = self#visit_lhs' ~redef ienv def_obj in
      let ienv = Senv.wake names ienv in

      (* let value_1 = value_2;
                       ^^^^^^^  *)
      let (_, (names', added')) = self#visit_option self#visit_rhs ienv def_value in

      let added = self#match_ self#replace_val added added' in (*XXX*)

      (* Remove the expired names *)
      let (names, _) = self#lapse (self#plus (names, added) (names', added')) in
      (Def { def_obj; def_value = None }, (names, added))


    (** Default value definition visitor *)
    method! visit_Def ienv = self#visit_Def' ienv


    method visit_init' ienv { set_obj; set_value } =
      (* value_1 <- value_2;
         ^^^^^^^^^^^^^^^^^^^ *)
      let (names, _, _, _) = self#get_symbol (Name_map.find "this" ienv) in
      let rev_names = Name_map.swap names in

      (* value_1 <- value_2;
         ^^^^^^^             *)
      let (set_obj, (_, added)) = self#visit_rhs' names set_obj in

      let (shift, names) = List.fold_left
        (fun (shift, names as acc) -> function
         | (-1, _) -> acc
         | (i, _) ->
             let j = self#allocate_val in
             (Int_map.add i j shift, Name_map.add (Env.Sym_map.find (Sym i) rev_names) (Ienv.Attr.Sym j, Lifetime.now) names))
        (Int_map.empty, Name_map.empty) (Added.flatten added) in

      (* value_1 <- value_2;
                    ^^^^^^^  *)
      let (set_value, (_, added')) = self#visit_rhs ienv set_value in

      let added = self#match_ (self#replace_val ~shift) added added' in (*XXX*)

      ({ set_obj; set_value }, (names, added))


    method! visit_init ienv { set_obj; set_value } =
      (* value_1 <- value_2;
         ^^^^^^^^^^^^^^^^^^^ *)
      let (names, _, _, _) = self#get_symbol (Name_map.find "this" ienv) in

      (* value_1 <- value_2;
         ^^^^^^^             *)
      let (set_obj, (_, added)) = self#visit_rhs' names set_obj in

      (* value_1 <- value_2;
                    ^^^^^^^  *)
      let (set_value, (_, added')) = self#visit_rhs ienv set_value in

      let added = self#match_ self#replace_val added added' in (*XXX*)
      ({ set_obj; set_value }, (Name_map.map (fun name -> (name, Lifetime.now)) names, added))



    (** Type definition visitor *)
    method visit_Def_type' ?(redef=true) ienv deftype_obj deftype_ty =
      (* type Type_1 = Type_2;
         ^^^^^^^^^^^^^^^^^^^^^ *)

      (* type Type_1 = Type_2;
              ^^^^^^           *)
      let (deftype_obj, (names, added)) = self#visit_lhs_ty' ~redef ienv deftype_obj in
      let ienv = Senv.wake names ienv in

      (* type Type_1 = Type_2;
                       ^^^^^^  *)
      let (_, (names', added')) = self#visit_option self#visit_rhs_ty ienv deftype_ty in
      let added = self#match_ self#replace_ty added added' in (*XXX*)

      let (names, _) = self#lapse (self#plus (names, added) (names', added')) in
      (Def_type { deftype_obj; deftype_ty = None }, (names, added))


    (** Default type definition visitor unused *)
    method! visit_Def_type = failwith "Unreachable"


    (** Z3 scope visitor *)
    method! visit_z3_scope ienv { z3_decl; z3_body } =
      (* for all (value : Type) { assertion; }
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ *)
      let ctx = context in
      context <- Z3;
      let (z3_decl, (names, _)) = self#visit_list (self#visit_lhs' ~lifetime:Lifetime.now) ienv z3_decl in
      let ienv = Senv.wake names ienv in
      let (z3_body, _) = self#visit_list' self#visit_rhs ienv z3_body in
      context <- ctx;
      ({ z3_decl; z3_body }, self#zero)


    method! visit_Z3_forall ienv z3_scope =
      let ({ z3_decl; z3_body } as z3_scope, _) = self#visit_z3_scope ienv z3_scope in
      let z3_scope = match (context, Name_map.find_opt "this" ienv) with
        | Top, Some (Sym i | Tsym i) -> { z3_decl = Lsome_of i::z3_decl; z3_body }
        | _ -> z3_scope in
      let z3_forall = Z3_forall z3_scope in
      (z3_forall, self#singleton (-1, Env.Constraint.of_typed z3_forall (Unit)))


    method! visit_Z3_exists ienv z3_scope =
      let (z3_scope, _) = self#visit_z3_scope ienv z3_scope in
      let z3_exists = Z3_exists z3_scope in
      let z3_rhs = match (context, Name_map.find_opt "this" ienv) with
        | Top, Some (Sym i | Tsym i) -> Z3_forall { z3_decl = [Lsome_of i]; z3_body = [z3_exists] }
        | _ -> z3_exists in
      (z3_rhs, self#singleton (-1, Env.Constraint.of_typed z3_rhs (Unit)))


    (** RHS symbol visitor *)
    method! visit_Rsym ienv sym =
      (* assert (x > 0)
                 ^      *)
      let (_, (_, rhs, _, _) as node) = self#solve_val ienv sym in
      (Option.get rhs, self#singleton node)


    (** RHS symbol visitor *)
    method! visit_Rlitexpr ienv litexpr =
      (* assert (x <<y>> z)
                   ^^^^^    *)
      self#visit_rhs ienv (Rsym ("<" ^ litexpr))


    method visit_rhs' ienv = function
      | Rtuple tuple -> super#visit_Rtuple ienv tuple
      | Rlist _ -> failwith "Illegal RHS assignment"
      | rhs -> self#visit_rhs ienv rhs


    (** RHS tuple visitor *)
    method! visit_Rtuple ienv tuple =
      let (_, (_, added)) = self#visit_list self#visit_rhs ienv tuple in
      let (rhses, rhs_tys) =
        let rec extract_vt_rec (rhses, rhs_tys) = function
          | hd::tl ->
              let (rhs, rhs_ty) = Env.Constraint.(of_added hd |> to_typed) in
              extract_vt_rec (rhs::rhses, rhs_ty::rhs_tys) tl
          | [] -> (List.rev rhses, List.rev rhs_tys) in
        extract_vt_rec ([], []) (Added.to_list added) in
      let rtuple = Rtuple rhses in
      (rtuple, self#singleton (-1, Env.Constraint.of_typed rtuple (Rttuple rhs_tys)))


    (** RHS list visitor *)
    method! visit_Rlist ienv l =
      let (_, (_, added)) = self#visit_list self#visit_rhs ienv l in
      let (rhses, rhs_ty) =
        let rec extract_vt_rec (rhses, rhs_ty) = function (*XXX: typing*)
          | hd::tl ->
              let (rhs, rhs_ty) = Env.Constraint.(of_added hd |> to_typed) in
              extract_vt_rec (rhs::rhses, rhs_ty) tl
          | [] -> (List.rev rhses, rhs_ty) in
        extract_vt_rec ([], Tunknown) (Added.to_list added) in
      let rlist = Rlist rhses in
      (rlist, self#singleton (-1, Env.Constraint.of_typed rlist (Rtlist { ty = rhs_ty; cardinality = [From (Integer 0)] })))


    (** RHS type tuple visitor *)
    method! visit_Rttuple ienv tuple =
      let (_, (_, added)) = self#visit_list self#visit_rhs_ty ienv tuple in
      let rhs_tys = List.map (fun a -> Env.Constraint.(of_added a |> to_type))
                             (Added.to_list added) in
      let rttuple = Rttuple rhs_tys in
      (rttuple, self#singleton (-1, Env.Constraint.of_type rttuple))


    (** Value reference visitor *)
    method! visit_Ref _ i =
      let (names, _, ty, kind) = self#get_val i in
      (Ref i, self#singleton (-1, (names, Some (Ref i), ty, kind))) (*NOTE: TO CHECK!*)


    (** External value visitor *)
    method! visit_Ext _ = failwith "visit_Ext: Unreachable"


    (** Value reference visitor *)
    method! visit_Rstring _ s =
      let i = self#add_val (Name_map.empty, Some (Rstring s), (Some TString, []), None) in
      (Ref i, self#singleton (i, (Name_map.empty, Some (Ref i), (Some (Type_of i), []), None)))


    (** RHS regular type visitor *)
    method! visit_Rtype ienv { ty_name; ty_params; _(*TODO: remove other fields*) } =
      (* value : Type<Ty>
                 ^^^^^^^^ *)
      ignore ty_params; (*TODO*)
      let (_, (_, _, (ty, _), _) as node) = self#solve_ty ienv ty_name in
      (Option.get ty, self#singleton node)

    (** <RHS>.<RHS type> visitor *)
    method! visit_Rrtdot ienv { dot_obj; dot_mbr } =
      (* value_1 : value_2.Type
                   ^^^^^^^^^^^^ *)
      let (dot_obj, senv) = self#visit_rhs ienv dot_obj in
      (* value_1 : value_2.Type
                   ^^^^^^^      *)
      let names = self#extract_subnames' [dot_obj] in (*XXX?*)
      let (dot_mbr, senv') = self#visit_rhs_ty names dot_mbr in
      (* value_1 : value_2.Type
                           ^^^^ *)
      (dot_mbr, self#plus senv senv')

    (** <RHS type>.<RHS type> visitor *)
    method! visit_Rtrtdot ienv { dot_obj; dot_mbr } =
      (* value : Type_1.Type_2
                 ^^^^^^^^^^^^^ *)
      let (_dot_obj, senv) = self#visit_rhs_ty ienv dot_obj in
      (* value : Type_1.Type_2
                 ^^^^^^        *)
      let names = Senv.to_added senv |> Cons.of_added |> Cons.get_names in
      let (dot_mbr, senv) = self#visit_rhs_ty names dot_mbr in
      (* value : Type_1.Type_2
                        ^^^^^^ *)
      (dot_mbr, senv)

    (** <RHS type>.<RHS> visitor *)
    method! visit_Rtrdot ienv { dot_obj; dot_mbr } =
      (* value_1 = Type.value_2
                   ^^^^^^^^^^^^ *)
      let (_dot_obj, senv) = self#visit_rhs_ty ienv dot_obj in
      (* value_1 : Type.value_2
                   ^^^^         *)
      let names = Senv.to_added senv |> Cons.of_added |> Cons.get_names in
      let (dot_mbr, senv) = self#visit_rhs names dot_mbr in
      (* value_1 : Type.value_2
                        ^^^^^^^ *)
      (dot_mbr, senv)

    (** <RHS>.<RHS> visitor *)
    method! visit_Rrdot ienv { dot_obj; dot_mbr } =
      (* value_1 : value_2.value_3
                   ^^^^^^^^^^^^^^^ *)
      let (dot_obj, _) = self#visit_rhs ienv dot_obj in
      (* value_1 : value_2.value_3
                   ^^^^^^^         *)
      let names = self#extract_subnames' [dot_obj] in (*XXX?*)
      let (dot_mbr, senv) = self#visit_rhs names dot_mbr in
      (* value_1 : value_2.value_3
                           ^^^^^^^ *)
      (dot_mbr, senv)


    (** Component visitor *)
    method! visit_cmp ienv { cmp_args; cmp_body; cmp_ann; cmp_hooks } =
      (* @Annotation component Cmp<Ty>(value_1, value_2) { let value_3; } ^Hook
         ^^^^^^^^^^^                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ *)

      (* @Annotation component Cmp<Ty>(value_1, value_2) { let value_3; } ^Hook
                                      ^^^^^^^^^^^^^^^^^^                        *)
      let (cmp_args, (names, _ )) =
        self#visit (list (arg$ (self#visit_lhs' ~lifetime:Lifetime.now))) ienv cmp_args in
      let ienv = Senv.wake names ienv in

      (* @Annotation component Cmp<Ty>(value_1, value_2) { let value_3; } ^Hook
                                                         ^^^^^^^^^^^^^^^^       *)
      let (cmp_body, senv') = self#visit_option self#visit_construct_body ienv cmp_body in
      let senv' = self#add_this ~sym:(Name_map.find "this" ienv) senv' in

      (* @Annotation component Cmp<Ty>(value_1, value_2) { let value_3; } ^Hook
         ^^^^^^^^^^^                                                            *)
      let (cmp_ann, senv'') = self#visit_list self#visit_rhs_ty ienv cmp_ann in

      (* @Annotation component Cmp<Ty>(value_1, value_2) { let value_3; } ^Hook
                                                                          ^^^^^ *)
      let (cmp_hooks, _) = self#visit_list self#visit_rhs ienv cmp_hooks in
      ({ cmp_args; cmp_body; cmp_ann; cmp_hooks }, self#plus senv'' senv')


    (** Component implementation visitor *)
    method! visit_Cmp ienv cmp =
      let (cmp, (names, _)) = self#visit_cmp ienv cmp in
      let cmp = Cmp cmp in
      (*TODO: annotations*)
      (cmp, (names, Added.Node (-1, (self#extract names, None, (Some cmp, []), Some Component))))


    (** Component initialization visitor *)
    method! visit_Rinit ienv init_obj init_body =
      (** cmp { value_1 <- value_2; }
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^ *)

      (** cmp { value_1 <- value_2; }
          ^^^                         *)
      let (init_obj, (_, added)) = self#visit_rhs ienv init_obj in

      let (_, (names, _, (rhs_ty, _), kind)) = Added.to_node added in

      let rhs_ty = Option.get rhs_ty in
      let hooks = match self#deref init_obj with
        | CInst (_, hooks) -> hooks
        | _ -> raise (Type_error (Mismatching, "?")) in
      match kind with (*FIXME: wording*)
        | Some Component -> self#init_Component ienv names hooks init_body rhs_ty
        | Some k -> raise (Kind_error (Mismatching, "Expected component, got " ^ Ast_format.kind k))
        | None -> raise (Kind_error (Mismatching, "Expected component, got None"))

    (** Component initialization routine *)
    method init_Component ienv names hooks init_body call_obj =
      (** Cmp { value_1 <- value_2; }
              ^^^^^^^^^^^^^^^^^^^^^^^ *)
      let i = self#allocate_val in
      let ienv = Name_map.(add "this" (find "this" names) ienv) in
      let (_, (names', _)) = self#visit_list self#visit_init' ienv init_body in
      let assoc = Name_map.fold (fun k (v, _) acc -> Env.Sym_map.add (Name_map.find k names) v acc)
                                names' (Env.Sym_map.empty)
                  |> Env.Sym_map.add (Ienv.find "this" names) (Ienv.Attr.Sym i) in
      let rhs = CInst (None, hooks) in
      let cons = (self#shift assoc names, Some rhs, (Some call_obj, []), Some Component) in
      self#replace_val i cons;
      (Ref i, (Name_map.empty, Added.Node (i, cons)))

    (** Type reference visitor *)
    method! visit_Tref _ i =
      let (names, _, _, kind) = self#get_ty i in
      (Tref i, self#singleton (-1, (names, None, (Some (Tref i), []), kind)))

    (** Type extension visitor *)
    method! visit_Text _ s =
      (Text s, self#singleton (-1, (Name_map.empty, None, (Some (Text s), []), None)))

    (** Builtin type visitor *)
    method! visit_Tbuilt_in _ s =
      (Tbuilt_in s, self#singleton (-1, (Name_map.empty, None, (Some (Tbuilt_in s), []), None)))

    (** Type call visitor *)
    method! visit_Tcall ienv call_obj call_args =
      (** Cmp(value_1, value_2)
          ^^^^^^^^^^^^^^^^^^^^^ *)

      (** Cmp(value_1, value_2)
          ^^^                   *)
      let (call_obj, senv) = self#visit_rhs_ty ienv call_obj in
      let (_, (names, _, (ty, _), kind)) = Senv.to_added senv |> Added.to_node in

      let rec cmp_of = function
      | Some (Cmp c) -> c
      | Some (Tref i) -> let (_, _, (ty, _), _) = self#get_ty i in cmp_of ty
      | Some _ -> failwith "Cannot create a component out of that"
      | None -> failwith "Cannot instantiate an unknown component" in
      match kind with
        | Some Component -> self#call_Component ienv names (cmp_of ty) call_args call_obj
        | None ->
            let (call_args, _) = self#visit (list (arg (option$ self#visit_rhs))) ienv call_args in
            let tcall = Tcall { call_obj; call_args } in
            (tcall, self#singleton (-1, (Name_map.empty, Some tcall, (None, []), None)))
        | _ -> failwith "visit_Tcall: Not implemented"

    (** Component call routine *)
    method call_Component ienv names { cmp_args; cmp_hooks; _ } call_args call_obj =
      (** Cmp(value_1, value_2)
             ^^^^^^^^^^^^^^^^^^ *)
      let i = self#allocate_val in
      let assoc =
        Env.Sym_map.(of_list
          (List.map
            (fun (lhs, value) ->
              let (ro, _) = self#visit_option self#visit_rhs ienv value in
              let sym = match ro with
                | Some (Ref i) -> i
                | Some _ -> failwith "assoc: Internal error"
                | None -> self#add_val Cons.empty in
              (self#sym_of_lref lhs, Ienv.Attr.Sym sym))
            (Arg.assoc cmp_args call_args))
        |> add (Ienv.find "this" names) (Ienv.Attr.Sym i)) in
      let rhs = CInst (None, cmp_hooks) in
      let cons = (self#shift assoc names, Some rhs, (Some call_obj, []), Some Component) in
      self#replace_val i cons;
      (Ref i, (Name_map.empty, Added.Node (i, cons)))


    (** Function/operator call visitor *)
    method! visit_Call ienv call_obj call_args =
      (** let z = x + y;
                  ^^^^^  *)

      (** let z = x + y;
                    ^    *)
      let (call_obj, senv) = self#visit_rhs ienv call_obj in

      (** let z = x + y;
                  ^   ^  *)
      let (call_args, senv') = self#visit (list (arg (option$ self#visit_rhs))) ienv call_args in

      let (names, _) = self#plus senv senv' in
      let call = Call { call_obj; call_args } in
      (call, (names, Added.Node (-1, (Name_map.empty, Some call, (None, []), None)))) (*XXX*)

    method visit_ast = self#visit_list' self#visit_struct_

    method! visit_CBody ienv l =
      let (l, senv) = self#visit_list' (self#visit_struct_' ~redef:false) ienv l in
      (CBody l, senv)
  end in

  let (ast, (names, added)) = v#visit_ast Name_map.empty ast in
  (ast, Name_map.map (fun (sym, _) -> sym) names, v, added)
