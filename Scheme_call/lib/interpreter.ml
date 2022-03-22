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
  | VLambda of formals * expr
[@@deriving show { with_path = false }]

type err = string

type var =
  { name : id
  ; value : value
  }

type context =
  { vars : var list
  ; call_cc : value
  }

type lambda_var =
  { l_name : id
  ; expr : expr
  }

type lambda_context = { lambda_vars : lambda_var list }

module Interpret = struct
  let ( let* ) m f = Result.bind m f
  let return = Result.ok
  let error = Result.error

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
    ]
  ;;

  let with_return (type u) (f : (u -> _) -> u) : u =
    let exception R of u in
    try f (fun x -> raise (R x)) with
    | R u -> u
  ;;

  let rec interpr_expr ctx expr =
    match expr with
    | Var v ->
      (match find_var ctx v with
      | Some var -> return var.value
      | None ->
        (match v with
        | vv when List.mem vv (bin_ops @ un_ops) -> return (VVar v)
        | _ -> error (Printf.sprintf "Exception: variable %s is not bound\n" v)))
    | Quote d ->
      (match d with
      | DConst c -> return (interpr_datum c)
      | List l -> return (VList (interpr_dlist l)))
    | Const c ->
      (match c with
      | Int x -> return (VInt x)
      | Bool x -> return (VBool x)
      | String x -> return (VString x))
    | Lam (formals, expr) -> return (VLambda (formals, expr))
    | Proc_call (Op op_expr, objs) -> interpr_proc_call ctx op_expr objs
    | Cond (test, conseq, alter) -> interpr_if_condionals ctx test conseq alter

  and interpr_datum = function
    | DInt x -> VInt x
    | DBool x -> VBool x
    | DString x -> VString x

  and interpr_dlist = function
    | hd :: tl ->
      (match hd with
      | DConst d -> List.cons (interpr_datum d) (interpr_dlist tl)
      | List l -> List.cons (VList (interpr_dlist l)) (interpr_dlist tl))
    | [] -> []

  and interpr_proc_call ctx op_expr objs =
    let* op = interpr_expr ctx op_expr in
    match op with
    | VVar v when List.mem v un_ops -> interpr_un_expr ctx v objs
    | VVar v -> interpr_bin_expr ctx v objs
    | VLambda (FVarList formals, expr) -> interpr_lambda_vars ctx expr formals objs
    | VLambda (FVar formal, expr) -> interpr_lambda_var ctx expr formal objs
    | _ -> error "Exception: attempt to apply non-procedure value\n"

  and substitute_vars l_vars = function
    | Var v ->
      (match find_var_for_lambda l_vars v with
      | Some expr -> expr
      | None -> Var v)
    | Const c -> Const c
    | Proc_call (Op op_expr, objs) ->
      Proc_call
        ( Op (substitute_vars l_vars op_expr)
        , List.map (fun expr -> substitute_vars l_vars expr) objs )
    | Lam (FVarList formals, expr) ->
      Lam
        ( FVarList formals
        , substitute_vars
            (List.filter (fun l_var -> not (List.mem l_var.l_name formals)) l_vars)
            expr )
    | Lam (FVar formal, expr) ->
      Lam
        ( FVar formal
        , substitute_vars (List.filter (fun l_var -> l_var.l_name <> formal) l_vars) expr
        )
    | Cond (test, conseq, alter) ->
      (match alter with
      | Some alt ->
        Cond
          ( substitute_vars l_vars test
          , substitute_vars l_vars conseq
          , Some (substitute_vars l_vars alt) )
      | None -> Cond (substitute_vars l_vars test, substitute_vars l_vars conseq, None))
    | Quote d -> Quote d

  and find_var_for_lambda l_vars l_name =
    List.find_map
      (fun var ->
        match var.l_name with
        | var_name when String.equal var_name l_name -> Some var.expr
        | _ -> None)
      l_vars

  and interpr_lambda_vars ctx expr formals objs =
    match List.compare_lengths formals objs with
    | 0 ->
      let l_vars = List.map2 (fun l_name expr -> { l_name; expr }) formals objs in
      let new_expr = substitute_vars l_vars expr in
      interpr_expr ctx new_expr
    | _ -> error "Exception: incorrect argument count in lambda\n"

  and interpr_lambda_var ctx expr formal objs =
    let* new_ctx = create_or_update_var ctx { name = formal; value = VExprList objs } in
    interpr_expr new_ctx expr

  and interpr_un_expr ctx op_str = function
    | [ v ] ->
      let* v = interpr_expr ctx v in
      (match op_str, v with
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
      | "display", x -> interpr_display x
      | _ -> error (Printf.sprintf "Exception in %s: invalid variable type\n" op_str))
    | _ -> error (Printf.sprintf "Exception in %s: incorrect argument count\n" op_str)

  and interpr_display x =
    let rec helper_display = function
      | VString v -> Printf.printf "%s" v
      | VInt v -> Printf.printf "%s" (Base.string_of_int v)
      | VBool true -> Printf.printf "#t"
      | VBool false -> Printf.printf "#f"
      | VList v ->
        let _ = Printf.printf "(" in
        let rec helper = function
          | [ y ] ->
            let _ = helper_display y in
            ()
          | hd :: tl ->
            let _ = helper_display hd in
            let _ = Printf.printf " " in
            helper tl
          | _ -> ()
        in
        let _ = helper v in
        Printf.printf ")"
      | VVar v -> Printf.printf "#<procedure %s>" v
      | VLambda _ -> Printf.printf "#<procedure>"
      | _ -> ()
    in
    let _ = helper_display x in
    return VVoid

  and l_car = function
    | hd :: _ -> return hd
    | [] -> error "Exception in car: () is not a pair\n"

  and l_cdr = function
    | _ :: tl -> return (VList tl)
    | [] -> error "Exception in cdr: () is not a pair\n"

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

  and interpr_bin_expr ctx op_str =
    match op_str with
    | "+" -> interpr_add_mul_expr ctx ( + ) op_str 0
    | "*" -> interpr_add_mul_expr ctx ( * ) op_str 1
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
    | _ -> fun _ -> error (Printf.sprintf "Exception: variable %s is not bound\n" op_str)

  and interpr_newline = function
    | [] ->
      let _ = Printf.printf "\n" in
      return VVoid
    | _ -> error "Exception in newline: incorrect argument count\n"

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
      | Proc_call (Op (Var "list"), objs) -> interpr_proc_call ctx op_expr objs
      | Var v ->
        let* x = interpr_expr ctx (Var v) in
        (match x with
        | VExprList exprs -> interpr_proc_call ctx op_expr exprs
        | _ -> error "Exception in apply: this is not a proper list")
      | _ -> error "Exception in apply: this is not a proper list")
    | _ -> error "Exception in apply: incorrect argument count"

  and interpr_append ctx =
    let rec helper_lists acc = function
      | hd :: tl ->
        let* head = interpr_expr ctx hd in
        (match head with
        | VList h -> helper_lists (acc @ h) tl
        | _ -> error "Exception in append: this is not a proper list\n")
      | [] -> return (VList acc)
    in
    helper_lists []

  and create_list ctx =
    let rec helper acc = function
      | hd :: tl ->
        let* head = interpr_expr ctx hd in
        helper (acc @ [ head ]) tl
      | [] -> return (VList acc)
    in
    helper []

  and create_pair ctx = function
    | [ car; cdr ] ->
      let* hd = interpr_expr ctx car in
      let* tl = interpr_expr ctx cdr in
      (match tl with
      | VList l -> return (VList (hd :: l))
      | _ -> error "Exception in cons: incorrect type of cdr\n")
    | _ -> error "Exception in cons: incorrect argument count\n"

  and interpr_add_mul_expr ctx op op_str =
    let rec helper acc = function
      | [] -> return (VInt acc)
      | hd :: tl ->
        let* l = interpr_expr ctx hd in
        (match l with
        | VInt n -> helper (op acc n) tl
        | _ -> error (Printf.sprintf "Exception in %s: this is not a number\n" op_str))
    in
    helper

  and interpr_sub_div_expr ctx op op_str c = function
    | [] ->
      error (Printf.sprintf "Exception: incorrect argument count in call (%s)\n" op_str)
    | [ el ] -> interpr_sub_div_expr ctx op op_str c [ Const (Int c); el ]
    | hd :: tl ->
      let* l = interpr_expr ctx hd in
      let* r = interpr_add_mul_expr ctx (if c = 0 then ( + ) else ( * )) op_str c tl in
      (match l, r, c with
      | VInt _, VInt 0, 1 -> error "Exception in /: undefined for 0\n"
      | VInt n, VInt m, _ -> return (VInt (op n m))
      | _ -> error (Printf.sprintf "Exception in %s: this is not a number\n" op_str))

  and interpr_and_or_expr ctx op op_str =
    let rec helper acc = function
      | [] -> return (VBool acc)
      | hd :: tl ->
        let* l = interpr_expr ctx hd in
        (match l with
        | VBool b -> helper (op acc b) tl
        | _ -> error (Printf.sprintf "Exception in %s: this is not bool value\n" op_str))
    in
    helper

  and interpr_comparing_expr ctx op op_str = function
    | [] ->
      error (Printf.sprintf "Exception: incorrect argument count in call (%s)\n" op_str)
    | hd :: tl ->
      let rec helper = function
        | [] -> return (VBool true)
        | hd :: tl ->
          let* l = interpr_expr ctx hd in
          let* r = helper tl in
          (match l, r with
          | VInt _, VBool false -> return (VBool false)
          | VInt n, VBool true -> return (VInt n)
          | VInt n, VInt m when op n m -> return (VInt n)
          | VInt _, VInt _ -> return (VBool false)
          | _ ->
            error (Printf.sprintf "Exception in %s: this is not a real number\n" op_str))
      in
      let* l = interpr_expr ctx hd in
      (match helper tl, l with
      | Ok (VInt rn), VInt ln when op ln rn -> return (VBool true)
      | Ok (VInt _), VInt _ -> return (VBool false)
      | Ok (VBool b), _ -> return (VBool b)
      | Error err, _ -> error err
      | _ -> error (Printf.sprintf "Exception in %s: this is not a real number\n" op_str))

  and interpr_if_condionals ctx test cons alter =
    let* t = interpr_expr ctx test in
    match t, alter with
    | VBool false, Some alt -> interpr_expr ctx alt
    | VBool false, None -> return VVoid
    | _ -> interpr_expr ctx cons

  and create_empty_ctx = { vars = []; call_cc = VVoid }

  and find_var ctx name =
    List.find_map
      (fun var ->
        match var.name with
        | var_name when String.equal var_name name -> Some var
        | _ -> None)
      ctx.vars

  and create_or_update_var ctx vv =
    match find_var ctx vv.name with
    | Some var ->
      let vars =
        vv
        :: List.find_all
             (fun v ->
               match v.name with
               | v_name when String.equal v_name var.name -> false
               | _ -> true)
             ctx.vars
      in
      return { ctx with vars }
    | None -> return { ctx with vars = vv :: ctx.vars }

  and interpr_def name expr ctx =
    let* value = interpr_expr ctx expr in
    let* new_ctx = create_or_update_var ctx { name; value } in
    return (new_ctx, VVar name)

  and interpr_form ctx = function
    | Def (var, expr) ->
      let* def = interpr_def var expr ctx in
      return def
    | Expr expr ->
      let* exp = interpr_expr ctx expr in
      return (ctx, exp)
  ;;

  let interpr_prog =
    let ctx = create_empty_ctx in
    let rec helper ctx value = function
      | hd :: tl ->
        let* ctx, value = interpr_form ctx hd in
        helper ctx value tl
      | [] -> return (ctx, value)
    in
    helper ctx VVoid
  ;;
end

let parse_and_run_form str =
  let module I = Interpret in
  let ctx = I.create_empty_ctx in
  match parse_this form str with
  | Some ast -> I.interpr_form ctx ast
  | None -> I.error "Exception: invalid syntax\n"
;;

let parse_and_run_prog str =
  let module I = Interpret in
  match parse_prog str with
  | Some ast -> I.interpr_prog ast
  | None -> I.error "Exception: invalid syntax\n"
;;
