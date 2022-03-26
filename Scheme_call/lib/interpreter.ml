open Ast
open Parser

type value =
  | VString of string
  | VInt of int
  | VBool of bool
  | VList of value list
  | VExprList of expr list
  | VVoid
  | VVar of id
  | VLambda of formals * expr list
  (*_________________________________________*)
  | VCallCC of expr
  | VEscaper of expr
  | VPreprocValue of value
  | VPreprocExpr of expr
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

type context = { vars : var list }

type lambda_var =
  { l_name : id
  ; expr : expr
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
    ; "escaper"
    ]
  ;;

  let interpr_datum = function
    | DInt x -> VInt x
    | DBool x -> VBool x
    | DString x -> VString x
  ;;

  let rec interpr_dlist xs =
    List.map
      (function
        | DConst d -> interpr_datum d
        | List l -> VList (interpr_dlist l))
      xs
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
    | Var v ->
      (match find_var_for_lambda l_vars v with
      | Some expr -> expr
      | None -> Var v)
    | Const c -> Const c
    | ProcCall (Op op_expr, objs) ->
      ProcCall
        ( Op (substitute_vars l_vars op_expr)
        , List.map (fun expr -> substitute_vars l_vars expr) objs )
    | Lam (FVarList formals, expr, exprs) ->
      let act_ctx =
        List.filter (fun l_var -> not (List.mem l_var.l_name formals)) l_vars
      in
      let new_exprs = List.map (fun expr -> substitute_vars act_ctx expr) exprs in
      Lam (FVarList formals, substitute_vars act_ctx expr, new_exprs)
    | Lam (FVar formal, expr, exprs) ->
      let act_ctx = List.filter (fun l_var -> l_var.l_name <> formal) l_vars in
      let new_exprs = List.map (fun expr -> substitute_vars act_ctx expr) exprs in
      Lam (FVar formal, substitute_vars act_ctx expr, new_exprs)
    | Cond (test, conseq, Some alt) ->
      Cond
        ( substitute_vars l_vars test
        , substitute_vars l_vars conseq
        , Some (substitute_vars l_vars alt) )
    | Cond (test, conseq, None) ->
      Cond (substitute_vars l_vars test, substitute_vars l_vars conseq, None)
    | Quote d -> Quote d
  ;;

  let find_var ctx name =
    List.find_map
      (fun var ->
        match var.name with
        | var_name when String.equal var_name name -> Some var
        | _ -> None)
      ctx.vars
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
      return { vars }
    | None -> return { vars = var :: ctx.vars }
  ;;

  let interpr_display x =
    let rec helper_display = function
      | VString v -> v
      | VInt v -> Base.string_of_int v
      | VBool true -> "#t"
      | VBool false -> "#f"
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
    in
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

  let create_empty_ctx = { vars = [] }
  let ex_to_prep_v expr = VPreprocExpr expr
  let exs_to_prep_vs exprs = List.map ex_to_prep_v exprs

  let value_to_expr f = function
    | VPreprocExpr x -> return x
    | VPreprocValue x -> f x
    | VBool x -> return (Const (Bool x))
    | VInt x -> return (Const (Int x))
    | VString x -> return (Const (String x))
    | VVar x -> return (Var x)
    | VList x ->
      let* l = mapm f x in
      return (ProcCall (Op (Var "list"), l))
    | VLambda (formals, exprs) -> return (Lam (formals, List.hd exprs, List.tl exprs))
    | VProcCall (v, vs) ->
      let* oper = f v in
      let* objs = mapm f vs in
      return (ProcCall (Op oper, objs))
    | _ -> error (Err "Yet not implemented in call/cc")
  ;;

  let rec call_cc1 = function
    | VCallCC _ -> return (Var "var_reserved_for_call_cc_call")
    | v -> value_to_expr call_cc1 v
  ;;

  let rec call_cc2 expr = function
    | VCallCC v ->
      return
        (ProcCall
           ( Op v
           , [ ProcCall
                 ( Op (Var "escaper")
                 , [ Lam (FVarList [ "var_reserved_for_call_cc_call" ], expr, []) ] )
             ] ))
    | v -> value_to_expr (call_cc2 expr) v
  ;;

  let rec interpr_expr ctx = function
    | Var v ->
      (match find_var ctx v with
      | Some var -> return var.value
      | None ->
        (match v with
        | vv when List.mem vv (bin_ops @ un_ops) -> return (VVar v)
        | _ -> error (Err (Printf.sprintf "Exception: variable %s is not bound\n" v))))
    | Quote (DConst c) -> return (interpr_datum c)
    | Quote (List l) -> return (VList (interpr_dlist l))
    | Const (Int x) -> return (VInt x)
    | Const (Bool x) -> return (VBool x)
    | Const (String x) -> return (VString x)
    | Lam (formals, expr, exprs) -> return (VLambda (formals, expr :: exprs))
    | ProcCall (Op op_expr, objs) -> interpr_proc_call ctx op_expr objs
    | Cond (test, conseq, alter) -> interpr_if_condionals ctx test conseq alter

  and interpr_proc_call ctx op_expr objs =
    let* op = interpr_expr ctx op_expr in
    match op with
    | VEscaper v ->
      let* ans = interpr_expr ctx (ProcCall (Op v, objs)) in
      error (Escaper ans)
    | VPreprocValue v -> return (VPreprocValue (VProcCall (v, exs_to_prep_vs objs)))
    | VVar v when List.mem v un_ops -> interpr_un_expr ctx v objs
    | VVar v -> interpr_bin_expr ctx v objs
    | VLambda (FVarList formals, exprs) -> interpr_lambda_vars ctx exprs formals objs
    | VLambda (FVar formal, exprs) -> interpr_lambda_var ctx exprs formal objs
    | _ -> error (Err "Exception: attempt to apply non-procedure value\n")

  and interpr_lambda_expr ctx =
    let rec helper acc = function
      | hd :: tl ->
        let* inner_expr = interpr_expr ctx hd in
        helper inner_expr tl
      | [] -> return acc
    in
    helper VVoid

  and interpr_lambda_vars ctx exprs formals objs =
    match List.compare_lengths formals objs with
    | 0 ->
      let l_vars = List.map2 (fun l_name expr -> { l_name; expr }) formals objs in
      let new_exprs = List.map (fun expr -> substitute_vars l_vars expr) exprs in
      interpr_lambda_expr ctx new_exprs
    | _ -> error (Err "Exception: incorrect argument count in lambda\n")

  and interpr_lambda_var ctx exprs formal objs =
    let* new_ctx = create_or_update_var ctx { name = formal; value = VExprList objs } in
    interpr_lambda_expr new_ctx exprs

  and interpr_un_expr ctx op_str exprs =
    match op_str, exprs with
    | "display", [ expr ] ->
      let* v = expr_call ctx expr in
      interpr_display v
    | "escaper", [ expr ] -> return (VEscaper expr)
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

  and interpr_datum_for_apply = function
    | DInt x -> Const (Int x)
    | DBool x -> Const (Bool x)
    | DString x -> Const (String x)

  and interpr_dlist_for_apply = function
    | hd :: tl ->
      (match hd with
      | DConst d -> List.cons (interpr_datum_for_apply d) (interpr_dlist_for_apply tl)
      | List l -> interpr_dlist_for_apply l @ interpr_dlist_for_apply tl)
    | [] -> []

  and interpr_apply ctx = function
    | [ op_expr; list_expr ] ->
      (match list_expr with
      | Quote l ->
        (match l with
        | DConst dc -> interpr_proc_call ctx op_expr [ interpr_datum_for_apply dc ]
        | List l -> interpr_proc_call ctx op_expr (interpr_dlist_for_apply l))
      | ProcCall (Op (Var "list"), objs) -> interpr_proc_call ctx op_expr objs
      | Var v ->
        let* x = interpr_expr ctx (Var v) in
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
    | [ el ] -> interpr_sub_div_expr ctx op op_str c [ Const (Int c); el ]
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
      let* cal_cc_expr1 = call_cc1 v in
      let* cal_cc_expr2 = call_cc2 cal_cc_expr1 v in
      expr_call ctx cal_cc_expr2
    | result -> result

  and interpr_def name ctx expr =
    let* value = expr_call ctx expr in
    let* new_ctx = create_or_update_var ctx { name; value } in
    return (new_ctx, VVar name)

  and interpr_form ctx = function
    | Def (name, expr) -> interpr_def name ctx expr
    | Expr expr ->
      let* exp = expr_call ctx expr in
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
