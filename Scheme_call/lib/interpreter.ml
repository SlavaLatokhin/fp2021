open Ast
open Parser

(** Аналогично ast-шной версии *)
type pqlist =
  | PQLList of prep_quasiquote list
  | PQLQuote of prep_quasiquote
  | PQLQuasiquote of prep_quasiquote

(** Аналогично ast-шной версии *)
and prep_quasiquote =
  | PQConst of dconst
  | PQList of pqlist
  | PQUnquote of prep_expr
      (** Аналогично ast-шной версии, но с дополнительными типами для call/cc *)

and prep_expr =
  | PVar of variable
  | PQuote of datum
  | PQuasiquote of prep_quasiquote
  | PConst of const
  | PProcCall of prep_expr * prep_expr list
  | PLam of formals * (variable * prep_expr) list * prep_expr * prep_expr list
      (** (lambda <formals> <definition> list <expr> <expr> list ) *)
  | PCond of prep_expr * prep_expr * prep_expr option
  | PEscaper of prep_expr
      (** Функция выхода. Бросает исключение, и позволяет вернуться в функцию - обертку *)
  | PCallCCLam of int * prep_expr (** Контекст выражения *)
  | PCallCC of int (** Переменная для вызова call/cc *)
[@@deriving show { with_path = false }]

type value =
  | VString of string
  | VInt of int
  | VBool of bool
  | VSymbol of string
  | VList of value list
  | VAbreviation of char * value
  | VExprList of prep_expr list
  | VVoid
  | VVar of id
  | VLambda of formals * (variable * prep_expr) list * prep_expr list
  (*_________________________________________*)
  | VCallCC of prep_expr
  | VCallCCLam of int * prep_expr
  | VEscaper of prep_expr
  | VPreprocValue of value
  | VPreprocExpr of prep_expr
  | VProcCall of value * value list
[@@deriving show { with_path = false }]

type err_interpr =
  | Err of string
  | Escaper of value

type error = ERROR of string

type var =
  { name : id
  ; value : value
  }

type call_cc_var =
  { number : int
  ; value : value
  }

type context =
  { vars : var list
  ; call_cc_vars : call_cc_var list
  }

type lambda_var =
  { l_name : id
  ; expr : prep_expr
  }

type lambda_context = { lambda_vars : lambda_var list }

let ( let* ) m f = Result.bind m f
let ( <*> ) f x = Result.bind f (fun f -> Result.bind x (fun x -> Result.Ok (f x)))
let return = Result.ok
let error = Result.error

let rec mapm f = function
  | [] -> return []
  | x :: xs -> return List.cons <*> f x <*> mapm f xs
;;

module Interpret = struct
  let bin_ops =
    [ "+"
    ; "*"
    ; "-"
    ; "/"
    ; "="
    ; ">"
    ; "<"
    ; ">="
    ; "<="
    ; "and"
    ; "or"
    ; "cons"
    ; "list"
    ; "append"
    ; "apply"
    ; "newline"
    ]
  ;;

  let un_ops =
    [ "not"
    ; "zero?"
    ; "positive?"
    ; "negative?"
    ; "odd?"
    ; "even?"
    ; "abs"
    ; "boolean?"
    ; "integer?"
    ; "number?"
    ; "procedure?"
    ; "pair?"
    ; "list?"
    ; "null?"
    ; "car"
    ; "cdr"
    ; "length"
    ; "display"
    ; "call/cc"
    ; "call-with-current-continuation"
    ]
  ;;

  let rec prep_expr = function
    | Var x -> PVar x
    | Const c -> PConst c
    | ProcCall (Op op_expr, objs) ->
      PProcCall (prep_expr op_expr, List.map (fun expr -> prep_expr expr) objs)
    | Lam (FVarList formals, defs, expr, exprs) ->
      let new_defs = List.map (fun (var, expr) -> var, prep_expr expr) defs in
      let new_exprs = List.map prep_expr exprs in
      PLam (FVarList formals, new_defs, prep_expr expr, new_exprs)
    | Lam (FVar formal, defs, expr, exprs) ->
      let new_defs = List.map (fun (var, expr) -> var, prep_expr expr) defs in
      let new_exprs = List.map prep_expr exprs in
      PLam (FVar formal, new_defs, prep_expr expr, new_exprs)
    | Cond (test, conseq, Some alt) ->
      PCond (prep_expr test, prep_expr conseq, Some (prep_expr alt))
    | Cond (test, conseq, None) -> PCond (prep_expr test, prep_expr conseq, None)
    | Quote d -> PQuote d
    | Quasiquote x -> PQuasiquote (prep_quasiquote x)

  and prep_quasiquote = function
    | QConst x -> PQConst x
    | QList (QLList x) -> PQList (PQLList (List.map prep_quasiquote x))
    | QList (QLQuote x) -> PQList (PQLQuote (prep_quasiquote x))
    | QList (QLQuasiquote x) -> PQList (PQLQuasiquote (prep_quasiquote x))
    | QUnquote x -> PQUnquote (prep_expr x)
  ;;

  let interpr_dconst = function
    | DInt x -> VInt x
    | DBool x -> VBool x
    | DString x -> VString x
    | DSymbol x -> VSymbol x
  ;;

  let rec interpr_abbrev c d = VAbreviation (c, interpr_datum d)

  and interpr_datum = function
    | DConst x -> interpr_dconst x
    | DList x -> VList (List.map interpr_datum x)
    | DAbreviation (x, d) -> interpr_abbrev x d
  ;;

  let find_var_for_lambda l_vars l_name =
    List.find_map
      (fun var ->
        match var.l_name with
        | var_name when String.equal var_name l_name -> Some var.expr
        | _ -> None)
      l_vars
  ;;

  let rec substitute_vars l_vars = function
    | PVar v ->
      (match find_var_for_lambda l_vars v with
      | Some expr -> expr
      | None -> PVar v)
    | PConst c -> PConst c
    | PProcCall (op_expr, objs) ->
      PProcCall
        ( substitute_vars l_vars op_expr
        , List.map (fun expr -> substitute_vars l_vars expr) objs )
    | PLam (FVarList formals, defs, expr, exprs) ->
      let act_ctx =
        List.filter (fun l_var -> not (List.mem l_var.l_name formals)) l_vars
      in
      let new_defs =
        List.map (fun (var, expr) -> var, substitute_vars act_ctx expr) defs
      in
      let new_exprs = List.map (fun expr -> substitute_vars act_ctx expr) exprs in
      PLam (FVarList formals, new_defs, substitute_vars act_ctx expr, new_exprs)
    | PLam (FVar formal, defs, expr, exprs) ->
      let act_ctx = List.filter (fun l_var -> l_var.l_name <> formal) l_vars in
      let new_defs =
        List.map (fun (var, expr) -> var, substitute_vars act_ctx expr) defs
      in
      let new_exprs = List.map (fun expr -> substitute_vars act_ctx expr) exprs in
      PLam (FVar formal, new_defs, substitute_vars act_ctx expr, new_exprs)
    | PCond (test, conseq, Some alt) ->
      PCond
        ( substitute_vars l_vars test
        , substitute_vars l_vars conseq
        , Some (substitute_vars l_vars alt) )
    | PCond (test, conseq, None) ->
      PCond (substitute_vars l_vars test, substitute_vars l_vars conseq, None)
    | PQuote d -> PQuote d
    | PEscaper x -> PEscaper (substitute_vars l_vars x)
    | PCallCC x -> PCallCC x
    | PCallCCLam (n, x) -> PCallCCLam (n, substitute_vars l_vars x)
    | PQuasiquote x -> PQuasiquote (subs_qq l_vars x)

  and subs_qq l_vars = function
    | PQConst x -> PQConst x
    | PQList (PQLList x) -> PQList (PQLList (List.map (subs_qq l_vars) x))
    | PQList (PQLQuote x) -> PQList (PQLQuote (subs_qq l_vars x))
    | PQList (PQLQuasiquote x) -> PQList (PQLQuasiquote (subs_qq2 l_vars x))
    | PQUnquote x -> PQUnquote (substitute_vars l_vars x)

  and subs_qq2 l_vars = function
    | PQConst x -> PQConst x
    | PQList (PQLList x) -> PQList (PQLList (List.map (subs_qq l_vars) x))
    | PQList (PQLQuote x) -> PQList (PQLQuote (subs_qq l_vars x))
    | PQList (PQLQuasiquote x) -> PQList (PQLQuasiquote (subs_qq l_vars x))
    | PQUnquote x -> PQUnquote x
  ;;

  let find_var ctx name =
    List.find_map
      (fun var ->
        match var.name with
        | var_name when String.equal var_name name -> Some var
        | _ -> None)
      ctx.vars
  ;;

  let find_call_cc_var ctx number =
    (List.find (fun var -> var.number = number) ctx.call_cc_vars).value
  ;;

  let create_or_update_var ctx var =
    match find_var ctx var.name with
    | Some _ ->
      let vars =
        var
        :: List.find_all
             (fun v ->
               match v.name with
               | v_name when String.equal v_name var.name -> false
               | _ -> true)
             ctx.vars
      in
      return { ctx with vars }
    | None -> return { ctx with vars = var :: ctx.vars }
  ;;

  let rec helper_display = function
    | VString v -> v
    | VInt v -> Base.string_of_int v
    | VBool true -> "#t"
    | VBool false -> "#f"
    | VSymbol v -> v
    | VAbreviation (c, v) -> Printf.sprintf "%c%s" c (helper_display v)
    | VList v ->
      let rec helper = function
        | [ y ] -> helper_display y
        | hd :: tl -> Printf.sprintf "%s %s" (helper_display hd) (helper tl)
        | _ -> ""
      in
      Printf.sprintf "(%s)" @@ helper v
    | VVar v -> "#<procedure >" ^ v
    | VLambda _ -> "#<procedure>"
    | VVoid -> "#<void>"
    | _ -> ""
  ;;

  let interpr_display x =
    let _ = Printf.printf "%s" (helper_display x) in
    return VVoid

  and interpr_newline = function
    | [] ->
      let _ = Printf.printf "\n" in
      return VVoid
    | _ -> error (Err "Exception in newline: incorrect argument count\n")

  and l_car = function
    | hd :: _ -> return hd
    | [] -> error (Err "Exception in car: () is not a pair\n")

  and l_cdr = function
    | _ :: tl -> return (VList tl)
    | [] -> error (Err "Exception in cdr: () is not a pair\n")

  and is_pair = function
    | VList xs when List.length xs != 0 -> return (VBool true)
    | _ -> return (VBool false)

  and is_list = function
    | VList _ -> return (VBool true)
    | _ -> return (VBool false)

  and is_null = function
    | VList [] -> return (VBool true)
    | _ -> return (VBool false)

  and is_procedure = function
    | VVar v when List.mem v (bin_ops @ un_ops) -> return (VBool true)
    | VLambda _ -> return (VBool true)
    | _ -> return (VBool false)

  and interpr_bool_value = function
    | VBool false -> false
    | _ -> true

  and is_num = function
    | VInt _ -> return (VBool true)
    | _ -> return (VBool false)

  and is_bool = function
    | VBool _ -> return (VBool true)
    | _ -> return (VBool false)
  ;;

  let create_empty_ctx = { vars = []; call_cc_vars = [] }
  let ex_to_prep_v expr = VPreprocExpr expr
  let exs_to_prep_vs exprs = List.map ex_to_prep_v exprs

  let value_to_expr f = function
    | VPreprocExpr x -> return x
    | VPreprocValue x -> f x
    | VBool x -> return (PConst (Bool x))
    | VInt x -> return (PConst (Int x))
    | VString x -> return (PConst (String x))
    | VVar x -> return (PVar x)
    | VList x ->
      let* l = mapm f x in
      return (PProcCall (PVar "list", l))
    | VLambda (formals, defs, exprs) ->
      return (PLam (formals, defs, List.hd exprs, List.tl exprs))
    | VProcCall (v, vs) ->
      let* oper = f v in
      let* objs = mapm f vs in
      return (PProcCall (oper, objs))
    | _ -> error (Err "Yet not implemented in call/cc")
  ;;

  let rec call_cc1 var_name = function
    | VCallCC _ -> return (PCallCC var_name)
    | v -> value_to_expr (call_cc1 var_name) v
  ;;

  let rec call_cc2 var_name expr = function
    | VCallCC v -> return (PProcCall (v, [ PEscaper (PCallCCLam (var_name, expr)) ]))
    | v -> value_to_expr ((call_cc2 var_name) expr) v
  ;;

  let rec interpr_expr ctx = function
    | PVar v ->
      (match find_var ctx v with
      | Some var -> return var.value
      | None ->
        (match v with
        | vv when List.mem vv (bin_ops @ un_ops) -> return (VVar v)
        | _ -> error (Err (Printf.sprintf "Exception: variable %s is not bound\n" v))))
    | PQuote x -> return (interpr_datum x)
    | PConst (Int x) -> return (VInt x)
    | PConst (Bool x) -> return (VBool x)
    | PConst (String x) -> return (VString x)
    | PLam (formals, defs, expr, exprs) -> return (VLambda (formals, defs, expr :: exprs))
    | PProcCall (op_expr, objs) -> interpr_proc_call ctx op_expr objs
    | PCond (test, conseq, alter) -> interpr_if_condionals ctx test conseq alter
    | PEscaper x -> return (VEscaper x)
    | PCallCCLam (n, expr) -> return (VCallCCLam (n, expr))
    | PCallCC x -> return (find_call_cc_var ctx x)
    | PQuasiquote x -> interpr_qquote ctx x

  and interpr_qquote ctx = function
    | PQConst x -> return (interpr_dconst x)
    | PQList (PQLList x) ->
      let* vlist = mapm (interpr_qquote ctx) x in
      return (VList vlist)
    | PQList (PQLQuote x) ->
      let* vquote = interpr_qquote ctx x in
      return (VAbreviation ('\'', vquote))
    | PQList (PQLQuasiquote x) ->
      let* vquote = interpr_qquote ctx x in
      return (VAbreviation ('`', vquote))
    | PQUnquote x ->
      let* unquotation = interpr_expr ctx x in
      return (VString (helper_display unquotation))

  and interpr_proc_call ctx op_expr objs =
    let* op = interpr_expr ctx op_expr in
    match op with
    | VEscaper v ->
      let* ans = interpr_expr ctx (PProcCall (v, objs)) in
      error (Escaper ans)
    | VPreprocValue v -> return (VPreprocValue (VProcCall (v, exs_to_prep_vs objs)))
    | VVar v when List.mem v un_ops -> interpr_un_expr ctx v objs
    | VVar v -> interpr_bin_expr ctx v objs
    | VCallCCLam (n, expr) -> interpr_lambda_call_cc ctx expr n objs
    | VLambda (FVarList formals, defs, exprs) ->
      interpr_lambda_vars ctx exprs defs formals objs
    | VLambda (FVar formal, defs, exprs) -> interpr_lambda_var ctx exprs defs formal objs
    | _ -> error (Err "Exception: attempt to apply non-procedure value\n")

  and interpr_lambda_call_cc ctx expr number = function
    | [ obj ] ->
      let* value = expr_call ctx obj in
      interpr_expr { ctx with call_cc_vars = { number; value } :: ctx.call_cc_vars } expr
    | _ -> error (Err "Exception: incorrect argument count in call/cc\n")

  and interpr_lambda_expr ctx =
    let rec helper acc = function
      | hd :: tl ->
        let* inner_expr = interpr_expr ctx hd in
        helper inner_expr tl
      | [] -> return acc
    in
    helper VVoid

  and interpr_lambda_vars ctx exprs defs formals objs =
    match List.compare_lengths formals objs with
    | 0 ->
      let l_vars = List.map2 (fun l_name expr -> { l_name; expr }) formals objs in
      let new_defs =
        List.map (fun (var, expr) -> var, substitute_vars l_vars expr) defs
      in
      let* new_ctx = interpr_defs ctx new_defs in
      let new_exprs = List.map (fun expr -> substitute_vars l_vars expr) exprs in
      interpr_lambda_expr new_ctx new_exprs
    | _ -> error (Err "Exception: incorrect argument count in lambda\n")

  and interpr_lambda_var ctx exprs defs formal objs =
    let* new_ctx = create_or_update_var ctx { name = formal; value = VExprList objs } in
    let* new_ctx = interpr_defs new_ctx defs in
    interpr_lambda_expr new_ctx exprs

  and interpr_un_expr ctx op_str exprs =
    match op_str, exprs with
    | "display", [ expr ] ->
      let* v = expr_call ctx expr in
      interpr_display v
    | "call-with-current-continuation", [ expr ] | "call/cc", [ expr ] ->
      return (VPreprocValue (VCallCC expr))
    | _ ->
      (match exprs with
      | [ v ] ->
        let* v = interpr_expr ctx v in
        (match op_str, v with
        | _, VPreprocValue v -> return (VPreprocValue (VProcCall (VVar op_str, [ v ])))
        | "not", v -> return (VBool (not (interpr_bool_value v)))
        | "zero?", VInt x -> return (VBool (x = 0))
        | "positive?", VInt x -> return (VBool (x > 0))
        | "negative?", VInt x -> return (VBool (x < 0))
        | "odd?", VInt x -> return (VBool (x mod 2 = 1))
        | "even?", VInt x -> return (VBool (x mod 2 = 0))
        | "abs", VInt x -> return (VInt (abs x))
        | "boolean?", x -> is_bool x
        | "integer?", x -> is_num x
        | "number?", x -> is_num x
        | "procedure?", x -> is_procedure x
        | "pair?", x -> is_pair x
        | "list?", x -> is_list x
        | "null?", x -> is_null x
        | "car", VList l -> l_car l
        | "cdr", VList l -> l_cdr l
        | "length", VList l -> return (VInt (List.length l))
        | _ ->
          error (Err (Printf.sprintf "Exception in %s: invalid variable type\n" op_str)))
      | _ ->
        error (Err (Printf.sprintf "Exception in %s: incorrect argument count\n" op_str)))

  and interpr_bin_expr ctx op_str =
    match op_str with
    | "+" -> helper_int_bin_expr ctx ( + ) op_str 0
    | "*" -> helper_int_bin_expr ctx ( * ) op_str 1
    | "-" -> interpr_sub_div_expr ctx ( - ) op_str 0
    | "/" -> interpr_sub_div_expr ctx ( / ) op_str 1
    | "=" -> interpr_comparing_expr ctx ( = ) op_str
    | ">" -> interpr_comparing_expr ctx ( > ) op_str
    | "<" -> interpr_comparing_expr ctx ( < ) op_str
    | ">=" -> interpr_comparing_expr ctx ( >= ) op_str
    | "<=" -> interpr_comparing_expr ctx ( <= ) op_str
    | "and" -> interpr_and_or_expr ctx ( && ) op_str true
    | "or" -> interpr_and_or_expr ctx ( || ) op_str false
    | "cons" -> create_pair ctx
    | "list" -> create_list ctx
    | "append" -> interpr_append ctx
    | "apply" -> interpr_apply ctx
    | "newline" -> interpr_newline
    | _ ->
      fun _ -> error (Err (Printf.sprintf "Exception: variable %s is not bound\n" op_str))

  and interpr_newline = function
    | [] ->
      let _ = Printf.printf "\n" in
      return VVoid
    | _ -> error (Err "Exception in newline: incorrect argument count\n")

  and interpr_dconst_app = function
    | DInt x -> PConst (Int x)
    | DBool x -> PConst (Bool x)
    | DString x -> PConst (String x)
    | DSymbol x -> PQuote (DConst (DSymbol x))

  and interpr_dlist_app = function
    | hd :: tl ->
      (match hd with
      | DConst dc -> List.cons (interpr_dconst_app dc) (interpr_dlist_app tl)
      | DList dl -> interpr_dlist_app dl @ interpr_dlist_app tl
      | DAbreviation (c, d) ->
        List.cons (PQuote (DAbreviation (c, d))) (interpr_dlist_app tl))
    | [] -> []

  and interpr_datum_f_apply = function
    | DConst dc -> [ interpr_dconst_app dc ]
    | DList l -> interpr_dlist_app l
    | DAbreviation (c, d) -> [ PQuote (DAbreviation (c, d)) ]

  and interpr_apply ctx = function
    | [ op_expr; list_expr ] ->
      (match list_expr with
      | PQuote l -> interpr_proc_call ctx op_expr (interpr_datum_f_apply l)
      | PProcCall (PVar "list", objs) -> interpr_proc_call ctx op_expr objs
      | PVar v ->
        let* x = interpr_expr ctx (PVar v) in
        (match x with
        | VExprList exprs -> interpr_proc_call ctx op_expr exprs
        | _ -> error (Err "Exception in apply: this is not a proper list"))
      | _ -> error (Err "Exception in apply: this is not a proper list"))
    | _ -> error (Err "Exception in apply: incorrect argument count")

  and interpr_append ctx =
    let rec helper_lists acc = function
      | hd :: tl ->
        let* head = interpr_expr ctx hd in
        (match head with
        | VList h -> helper_lists (acc @ h) tl
        | _ -> error (Err "Exception in append: this is not a proper list\n"))
      | [] -> return (VList acc)
    in
    helper_lists []

  and create_list ctx objs =
    let* list = mapm (interpr_expr ctx) objs in
    return (VList list)

  and create_pair ctx = function
    | [ car; cdr ] ->
      let* hd = interpr_expr ctx car in
      let* tl = interpr_expr ctx cdr in
      (match tl with
      | VList l -> return (VList (hd :: l))
      | _ -> error (Err "Exception in cons: incorrect type of cdr\n"))
    | _ -> error (Err "Exception in cons: incorrect argument count\n")

  and helper_int_bin_expr ctx op op_str acc = function
    | [] -> return (VInt acc)
    | hd :: tl ->
      let* l = interpr_expr ctx hd in
      (match l with
      | VPreprocValue v ->
        return
          (VPreprocValue
             (VProcCall (VVar op_str, VInt acc :: VPreprocValue v :: exs_to_prep_vs tl)))
      | VInt 0 when op_str = "/" -> error (Err "Exception in /: undefined for 0\n")
      | VInt n -> helper_int_bin_expr ctx op op_str (op acc n) tl
      | _ -> error (Err (Printf.sprintf "Exception in %s: this is not a number\n" op_str)))

  and interpr_sub_div_expr ctx op op_str c = function
    | [] ->
      error
        (Err (Printf.sprintf "Exception: incorrect argument count in call (%s)\n" op_str))
    | [ el ] -> interpr_sub_div_expr ctx op op_str c [ PConst (Int c); el ]
    | head :: objs ->
      let* h = interpr_expr ctx head in
      (match h with
      | VPreprocValue v ->
        return
          (VPreprocValue (VProcCall (VVar op_str, VPreprocValue v :: exs_to_prep_vs objs)))
      | VInt v -> helper_int_bin_expr ctx op op_str v objs
      | _ -> error (Err (Printf.sprintf "Exception in %s: this is not a number\n" op_str)))

  and interpr_and_or_expr ctx op op_str =
    let rec helper acc = function
      | [] -> return (VBool acc)
      | hd :: tl ->
        let* l = interpr_expr ctx hd in
        (match l with
        | VPreprocValue v ->
          return
            (VPreprocValue (VProcCall (VVar op_str, VPreprocValue v :: exs_to_prep_vs tl)))
        | VBool b -> helper (op acc b) tl
        | _ ->
          error (Err (Printf.sprintf "Exception in %s: this is not bool value\n" op_str)))
    in
    helper

  and interpr_comparing_expr ctx op op_str = function
    | [] ->
      error
        (Err (Printf.sprintf "Exception: incorrect argument count in call (%s)\n" op_str))
    | hd :: tl ->
      let rec helper acc = function
        | head :: tail ->
          let* h = expr_call ctx head in
          (match h with
          | VPreprocValue v ->
            return
              (VPreprocValue
                 (VProcCall (VVar op_str, VInt acc :: VPreprocValue v :: exs_to_prep_vs tl)))
          | VInt v -> if op acc v then helper v tail else return (VBool false)
          | _ ->
            error
              (Err (Printf.sprintf "Exception in %s: this is not a real number\n" op_str)))
        | [] -> return (VBool true)
      in
      let* head = expr_call ctx hd in
      (match head with
      | VPreprocValue v ->
        return
          (VPreprocValue (VProcCall (VVar op_str, VPreprocValue v :: exs_to_prep_vs tl)))
      | VInt v -> helper v tl
      | _ ->
        error (Err (Printf.sprintf "Exception in %s: this is not a real number\n" op_str)))

  and interpr_if_condionals ctx test cons alter =
    let* t = interpr_expr ctx test in
    match t, alter with
    | VBool false, Some alt -> interpr_expr ctx alt
    | VBool false, None -> return VVoid
    | _ -> interpr_expr ctx cons

  and expr_call ctx expr =
    match interpr_expr ctx expr with
    | Error (Escaper value) -> return value
    | Ok (VPreprocValue v) ->
      let* cal_cc_expr1 = (call_cc1 (List.length ctx.call_cc_vars)) v in
      let* cal_cc_expr2 = (call_cc2 (List.length ctx.call_cc_vars)) cal_cc_expr1 v in
      expr_call ctx cal_cc_expr2
    | result -> result

  and interpr_defs ctx =
    let rec helper acc_ctx = function
      | (var, expr) :: tl ->
        let* ctx, _ = interpr_def var acc_ctx expr in
        helper ctx tl
      | [] -> return acc_ctx
    in
    helper ctx

  and interpr_def name ctx expr =
    let* value = expr_call ctx expr in
    let* new_ctx = create_or_update_var ctx { name; value } in
    return (new_ctx, VVar name)

  and interpr_form ctx = function
    | Def (name, expr) -> interpr_def name ctx (prep_expr expr)
    | Expr expr ->
      let* exp = expr_call ctx (prep_expr expr) in
      return (ctx, exp)
  ;;

  let interpr_prog =
    let ctx = create_empty_ctx in
    let rec helper ctx value = function
      | hd :: tl ->
        (match interpr_form ctx hd with
        | Ok (ctx, value) -> helper ctx value tl
        | Error (Err err) -> error (ERROR err)
        | Error (Escaper v) -> return (ctx, v))
      | [] -> return (ctx, value)
    in
    helper ctx VVoid
  ;;
end

let parse_and_run_prog str =
  let module I = Interpret in
  match parse_prog str with
  | Some ast -> I.interpr_prog ast
  | None -> error (ERROR "Exception: invalid syntax\n")
;;
