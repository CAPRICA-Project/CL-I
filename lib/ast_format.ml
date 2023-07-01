open Ast
module Ienv = Env.Inherited

let name_of_lhs = function Lsym s -> s | Lref _ -> failwith "name_of_lhs: Lref" | _ -> failwith "name_of_lhs: Not implemented"
let name_of_lhs_ty = function
  | Ltype { ty_name; _ } -> ty_name
  | _ -> failwith "name_of_lhs_ty: Not implemented"

let kind ?(pretty=false) = function
  | Resource  -> if pretty then "𝓇" else "resource"
  | Service   -> if pretty then "𝓈" else "service"
  | Component -> if pretty then "𝒸" else "component"
  | Protocol  -> if pretty then "𝓅" else "protocol"

let template = function
  | Tuid s -> s
  | Tcons (Word w) -> "`" ^ w ^ "`"
  | _ -> failwith "string_of_template: Not implemented"

module type Printer = sig
  val cardinality : ienv:Ienv.t -> cardinality list -> string
  val rhs : ienv:Ienv.t -> rhs -> string
  val rhs_ty : ienv:Ienv.t -> rhs_ty -> string
  val lhs : ienv:Ienv.t -> lhs -> string
  val lhs_ty : ienv:Ienv.t -> lhs_ty -> string
  val struct_ : ienv:Ienv.t -> struct_ -> string
  val ast : ienv:Ienv.t -> t -> string
  val kind : ?pretty:bool -> kind -> string
end

let printer instance = object (self)
  inherit [_] reduce
  method plus = (^)
  method zero = ""

  method! visit_cardinality ienv = function
    | From (Rsym "1") -> "+"
    | From (Integer 1) -> "+"
    | From (Rsym "0") -> "*"
    | From (Integer 0) -> "*"
    | From_to (Smth, Smth) -> "[]"
    | From_to (r, r') when r=r' -> "[" ^ self#visit_rhs ienv r ^ "]"
    | From_to (r, r') -> "[" ^ self#visit_rhs ienv r ^ ", " ^ self#visit_rhs ienv r' ^ "]"
    | From r -> "[" ^ self#visit_rhs ienv r ^ "…]"
    | To r -> "[…" ^ self#visit_rhs ienv r ^ "]"

  method! visit_Rsym _ s = s

  method! visit_Rstring _ s = [%string {|"%{s}"|}]

  method! visit_Call ienv call_obj call_args =
     self#visit_rhs ienv call_obj ^
     "(" ^
       (String.concat ", "
          (List.map
             (Arg.fold ~etc:"…"
                       ~arg:(Option.fold ~none:"?" ~some:(self#visit_rhs ienv))) call_args)) ^
     ")"

  method! visit_Tcall ienv call_obj call_args =
    self#visit_rhs_ty ienv call_obj ^
    "(" ^
      (String.concat ", "
         (List.map
            (Arg.fold ~etc:"…"
                      ~arg:(Option.fold ~none:"?" ~some:(self#visit_rhs ienv))) call_args)) ^
    ")"

  method private format_rtys ienv rtys =
    String.concat " + " (List.map (self#visit_rhs_ty ienv) rtys)

  method! visit_Ref ienv i =
    let (_, value, (ty, tys), _) = instance#get_val i in
    let rtys = Option.to_list ty @ tys in
    let r = [%string "𝐫 %{i#Int}"] in
    (match rtys with
     | [] -> r
     | _ -> "(" ^ r ^ " : " ^ self#format_rtys ienv rtys ^ ")") ^
    Option.fold ~none:"" ~some:(fun v -> " = " ^ self#visit_rhs ienv v) value

  method! visit_Smth _ = "something"

  method! visit_Unknown _ = "unknown"

  method! visit_Ext _ e = "𝑒 " ^ e

  method! visit_Rcons _ _ = failwith"Rcons"

  method! visit_Rtuple ienv l =
    "(" ^
      String.concat ", " (List.map (self#visit_rhs ienv) l) ^
    ")"

  method! visit_Rlist ienv l =
    "[" ^
      String.concat "; " (List.map (self#visit_rhs ienv) l) ^
    "]"

  method! visit_Init _ _ = "<%Init%>"

  method! visit_Rrdot _ _ = failwith"Rrdot"

  method! visit_Rtrdot _ _ = failwith"Rtrdot"

  method! visit_CInst _ _ _ = "<%CInst%>"

  method! visit_Fun ienv _ fun_args fun_ret =
    "λ(" ^
      (String.concat ", "
         (List.map
            (Arg.fold ~etc:"…"
                      ~arg:(Option.fold ~none:"?" ~some:(self#visit_rhs_ty ienv))) fun_args)) ^
    ") → " ^
    (Option.fold ~none:"?" ~some:(self#visit_rhs_ty ienv) fun_ret)

  method! visit_Word _ s = [%string "`%{s}`"]

  method! visit_Tbuilt_in _ s = s

  method! visit_Rttuple ienv l =
    "(" ^
      (String.concat ", " (List.map (self#visit_rhs_ty ienv) l)) ^
    ")"

  method! visit_Rtlist ienv ty cardinality =
    (self#visit_rhs_ty ienv ty) ^ (self#visit_list self#visit_cardinality ienv cardinality)

  method! visit_Unit _ = "()"

  method! visit_Text _ s = [%string {|"%{s}"|}]

  method! visit_Cmp ienv { cmp_args; cmp_body; _ } =
    "𝒸(" ^
      (String.concat ", " (List.map (Arg.fold ~etc:"…" ~arg:(self#visit_lhs ienv)) cmp_args)) ^
    ")" ^
    (match cmp_body with
       | Some (CEq _) -> failwith "string_of_rhs_ty: Not implemented"
       | Some (CBody l) -> " {" ^ String.concat "; " (List.map (self#visit_struct_ ienv) l) ^ "}"
       | None -> "")

  method! visit_Rtype ienv { ty_name; ty_params; ty_sub } =
    let (_, (_, _, (tyo, _), _)) = instance#solve_ty ienv ty_name in
    let ty = Option.get tyo in
    "(" ^
      Option.fold ~none:"" ~some:(fun k -> self#visit_kind ienv k ^ " ") None ^
      ty_name ^
      Option.fold ~none:"" ~some:(fun x ->
        ("<" ^
           (String.concat ", "
              (List.map
                 (Arg.fold ~etc:"…"
                           ~arg:(Option.fold ~none:"?" ~some:(self#visit_rhs_ty ienv))) x)) ^
         ">")) ty_params ^
      (match ty_sub with
         | [] -> ""
         | _ -> " <: " ^ String.concat " + " (List.map (self#visit_rhs_ty ienv) ty_sub)) ^
      " = " ^
      self#visit_rhs_ty ienv ty ^
    ")"

  method! visit_Tref ienv i =
    let (_names, _, (ty, tys), _) = instance#get_ty i in
    let rtys = Option.to_list ty @ tys in
(*
    print_string "NAMES: ";
    Env.Name_map.iter (fun k _ -> print_string k) names;
    print_newline ();
*)
    let r = [%string "𝐑 %{i#Int}"] in
    (match rtys with
     | [] -> r
     | _ -> "(" ^ r ^ " = " ^ self#format_rtys ienv rtys ^ ")")

  method! visit_Type_of ienv i =
    let (_, _, (ty, tys), _) = instance#get_val i in
    let rtys = Option.to_list ty @ tys in
    let r = [%string "𝐓𝐫 %{i#Int}"] in
    (match rtys with
     | [] -> r
     | _ -> "(" ^ r ^ " = " ^ self#format_rtys ienv rtys ^ ")")

  method! visit_Lsym _ s = s

  method! visit_Lcons ienv lcons_obj lcons_ty =
    "(" ^ self#visit_lhs ienv lcons_obj ^ " : " ^ self#visit_rhs_ty ienv lcons_ty ^ ")"

  method! visit_Lalt _ s = "[]" ^ s

  method! visit_Lidx ienv idx = "[" ^ self#visit_rhs ienv idx ^ "]"

  method! visit_Lmore _ = "[+]"

  method! visit_Llist _ = "[]"

  method! visit_Ltype ienv { ty_name; ty_params; ty_sub } =
    Option.fold ~none:"" ~some:(fun k -> self#visit_kind ienv k ^ " ") None ^ ty_name ^
    Option.fold ~none:"" ~some:(fun x ->
      ("<" ^
         (String.concat ", "
            (List.map
               (Arg.fold ~etc:"…" ~arg:(self#visit_template ienv)) x)) ^
       ">")) ty_params ^
    (match ty_sub with
       | [] -> ""
       | _ -> " <: " ^ String.concat " + " (List.map (self#visit_rhs_ty ienv) ty_sub))

  method! visit_Lttuple ienv l =
    "(" ^
      (String.concat ", "
         (List.map
            (Arg.fold ~etc:"…" ~arg:(Option.fold ~none:"?" ~some:(self#visit_lhs_ty ienv))) l)) ^
    ")"

  method! visit_Def ienv def_obj def_value =
    self#visit_lhs ienv def_obj ^
    Option.fold ~none:"" ~some:(fun v -> " = " ^ self#visit_rhs ienv v) def_value

  method! visit_Def_type ienv deftype_obj deftype_ty =
    self#visit_lhs_ty ienv deftype_obj ^
    Option.fold ~none:"" ~some:(fun ty -> " = " ^ self#visit_rhs_ty ienv ty) deftype_ty

  method! visit_kind _ = kind ~pretty:true

  method ast ienv a = String.concat "\n" (List.map (self#visit_struct_ ienv) a)

  method struct_ = self#visit_struct_
end
