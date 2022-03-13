open Ast
open Parser

type value =
  | VString of string
  | VInt of int
  | VBool of bool
  (* | VFraction of int * int *)
  | VVoid
  | VVar of id
  | VLambda of formals * expr
[@@deriving show { with_path = false }]

type err =
  | IncorrectType
  (* | VarNotBound of string *)
  | TODOERROR of string
  | UnreachableError
  | Err of string
[@@deriving show { with_path = false }]

type var =
  { name : id
  ; value : value
  }

type context = { vars : var list }

(* type lambda_var =
  { l_name : id
  ; expr : expr
  }

type lambda_context = { lambda_vars : lambda_var list } *)

module Interpret = struct
  let ( let* ) m f = Result.bind m f
  let return = Result.ok
  let error = Result.error
  let bin_ops = [ "+"; "*"; "-"; "/"; "="; ">"; "<"; ">="; "<=" ]

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
    ; "number?" (* ; "null?" скорее всего для листа (с '), сделать*)
    ]
  ;;

  let rec interpr_expr ctx expr =
    match expr with
    | Var v ->
      (match find_var ctx v with
      | Some var -> return var.value
      | None ->
        (match v with
        | vv when List.mem vv bin_ops || List.mem vv un_ops -> return (VVar v)
        | _ ->
          error
            (TODOERROR
               "Вызов функции с несуществующим названием, или попытка вызвать не \
                процедуру")))
    | Const c ->
      (match c with
      | Int x -> return (VInt x)
      | Bool x -> return (VBool x)
      | String x -> return (VString x))
    | Lam (formals, expr) -> return (VLambda (formals, expr))
    | Proc_call (Op op_expr, objs) ->
      let* op = interpr_expr ctx op_expr in
      (match op with
      | VVar v when List.mem v bin_ops -> interpr_bin_expr ctx v objs
      | VVar v when List.mem v un_ops -> interpr_un_expr ctx v objs
      | VLambda (FVarList formals, expr) -> interpr_lambda ctx expr formals objs
      | _ ->
        error
          (TODOERROR
             "Вызов функции с несуществующим названием, или попытка вызвать не процедуру"))
    | Cond (test, conseq, alter) -> interpr_if_condionals ctx test conseq alter
    | _ -> error (TODOERROR "Еще не умею интерпретировать")

  (* and substitute_vars ctx expr =
    match expr with
    | Var v ->
      (match find_var ctx v with
      | Some var -> return var.value
      | None -> return (Var v))
    | _ -> error (TODOERROR "Еще не умею интерпретировать") *)

  (* and create_or_update_var_for_lambda ctx name expr =
    let* value = interpr_expr ctx expr in
    match
      List.find_map
        (fun var ->
          match var.name with
          | var_name when String.equal var_name name -> Some var
          | _ -> None)
        ctx.vars
    with
    | Some var ->
      let vars =
        { name; value }
        :: List.find_all
             (fun v ->
               match v.name with
               | v_name when String.equal v_name var.name -> false
               | _ -> true)
             ctx.vars
      in
      return { vars }
    | None -> return { vars = { name; value } :: ctx.vars } *)

  and interpr_lambda ctx expr formals objs =
    let rec helper ctx vars values =
      match vars, values with
      | var_hd :: var_tl, obj_hd :: obj_tl ->
        let* new_ctx = create_or_update_var ctx var_hd obj_hd in
        helper new_ctx var_tl obj_tl
      | [], [] -> interpr_expr ctx expr
      | _, [] -> error (TODOERROR "Неверное число аргументов в лямбде")
      | [], _ -> error (TODOERROR "Неверное число аргументов в лямбде")
    in
    helper ctx formals objs

  and interpr_un_expr ctx op vs =
    match vs with
    | [ v ] ->
      let* v = interpr_expr ctx v in
      (match op, v with
      | "not", VBool b -> return (VBool (not b))
      | "zero?", VInt n -> return (VBool (n = 0))
      | "positive?", VInt n -> return (VBool (n > 0))
      | "negative?", VInt n -> return (VBool (n < 0))
      | "odd?", VInt n -> return (VBool (n mod 2 = 1))
      | "even?", VInt n -> return (VBool (n mod 2 = 0))
      | "abs", VInt n -> return (VInt (abs n))
      | "boolean?", n -> is_bool n
      | s, n when List.mem s [ "integer?"; "number?" ] -> is_num n
      | _ -> error UnreachableError)
    | _ -> error (TODOERROR "Несколько аргументов у унарной операции")

  and is_num = function
    | VInt _ -> return (VBool true)
    | _ -> return (VBool false)

  and is_bool = function
    | VInt _ -> return (VBool true)
    | _ -> return (VBool false)

  and interpr_bin_expr ctx = function
    | "+" -> interpr_add_mul_expr ctx ( + ) 0
    | "*" -> interpr_add_mul_expr ctx ( * ) 1
    | "-" -> interpr_sub_div_expr ctx ( - ) 0
    | "/" -> interpr_sub_div_expr ctx ( / ) 1
    | "=" -> interpr_comparing_expr ctx ( = )
    | ">" -> interpr_comparing_expr ctx ( > )
    | "<" -> interpr_comparing_expr ctx ( < )
    | ">=" -> interpr_comparing_expr ctx ( >= )
    | "<=" -> interpr_comparing_expr ctx ( <= )
    | _ -> fun _ -> error UnreachableError

  and interpr_add_mul_expr ctx op =
    let rec helper acc = function
      | [] -> return (VInt acc)
      | hd :: tl ->
        let* l = interpr_expr ctx hd in
        (match l with
        | VInt n -> helper (op acc n) tl
        | _ -> error (TODOERROR "Не численный аргумент в бинарной операции"))
    in
    helper

  and interpr_sub_div_expr ctx op c = function
    | [] -> error (TODOERROR "Ни одного аргумента в вычитании или делении")
    | [ el ] -> interpr_sub_div_expr ctx op c [ Const (Int c); el ]
    | hd :: tl ->
      let* l = interpr_expr ctx hd in
      let* r = interpr_add_mul_expr ctx (if c = 0 then ( + ) else ( * )) c tl in
      (match l, r, c with
      | VInt _, VInt 0, 1 -> error (Err "Exception in /: undefined for 0")
      | VInt n, VInt m, _ -> return (VInt (op n m))
      | _ -> error (TODOERROR "Не численный аргумент в бинарной операции"))

  and interpr_comparing_expr ctx op = function
    | [] -> error (Err "Exception: incorrect argument count in call (=)")
    | hd :: tl ->
      let rec helper arr =
        match arr with
        | [] -> return (VBool true)
        | hd :: tl ->
          let* l = interpr_expr ctx hd in
          let* r = helper tl in
          (match l, r with
          | VInt _, VBool false -> return (VBool false)
          | VInt n, VBool true -> return (VInt n)
          | VInt n, VInt m when op n m -> return (VInt n)
          | VInt _, VInt _ -> return (VBool false)
          | _ -> error (TODOERROR "Неверный тип переменной при сравнении"))
      in
      let* l = interpr_expr ctx hd in
      (match helper tl, l with
      | Ok (VInt rn), VInt ln when op ln rn -> return (VBool true)
      | Ok (VInt _), VInt _ -> return (VBool false)
      | Ok (VBool b), _ -> return (VBool b)
      | Error err, _ -> error err
      | _ -> error (TODOERROR "Неверный тип переменной при сравнении"))

  and interpr_if_condionals ctx test cons alter =
    let* t = interpr_expr ctx test in
    match t, alter with
    | VBool false, Some alt ->
      let* a = interpr_expr ctx alt in
      return a
    | VBool false, None -> return VVoid
    | _ ->
      let* a = interpr_expr ctx cons in
      return a

  and create_empty_ctx = { vars = [] }

  and find_var ctx name =
    List.find_map
      (fun var ->
        match var.name with
        | var_name when String.equal var_name name -> Some var
        | _ -> None)
      ctx.vars

  and create_or_update_var ctx name expr =
    let* value = interpr_expr ctx expr in
    match
      List.find_map
        (fun var ->
          match var.name with
          | var_name when String.equal var_name name -> Some var
          | _ -> None)
        ctx.vars
    with
    | Some var ->
      let vars =
        { name; value }
        :: List.find_all
             (fun v ->
               match v.name with
               | v_name when String.equal v_name var.name -> false
               | _ -> true)
             ctx.vars
      in
      return { vars }
    | None -> return { vars = { name; value } :: ctx.vars }

  and interpr_def name expr ctx =
    (* let* value = interpr_expr ctx expr in *)
    let* new_ctx = create_or_update_var ctx name expr in
    return (new_ctx, VVoid)

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
  | None ->
    Printf.eprintf "Parsing error\n%!";
    exit 1
;;

let parse_and_run_prog str =
  let module I = Interpret in
  match parse_prog str with
  | Some ast -> I.interpr_prog ast
  | None ->
    Format.eprintf "Parsing error\n%!";
    exit 1
;;

let () =
  let input_str =
    "(define fac (lambda (n) (if (< n 1) 1 (* n (fac (- n 1)))))) (fac 6)"
  in
  match parse_and_run_prog input_str with
  | Ok (ctx, ans) ->
    let _ = Printf.printf "Actual ctx: " in
    let rec helper = function
      | hd :: tl ->
        let _ = Format.printf "%a = %a;\n " pp_variable hd.name pp_value hd.value in
        helper tl
      | [] -> Format.printf "Actual ans: %a" pp_value ans
    in
    helper ctx.vars
  | Error err ->
    (match err with
    | Err err_msg -> Printf.printf "%s" err_msg
    | TODOERROR err_msg -> Printf.printf "%s" err_msg
    | _ -> print_endline "ERROR!")
;;

exception Test_failed

let test_suc expr expected =
  match expr with
  | Error _ -> raise Test_failed
  | Ok (_, ans) -> ans = expected
;;

let%test _ =
  let expr = parse_and_run_form "(+)" in
  test_suc expr (VInt 0)
;;

let%test _ =
  let expr = parse_and_run_form "(+ 1 2 3 4 5)" in
  test_suc expr (VInt 15)
;;

let%test _ =
  let expr = parse_and_run_form "(*)" in
  test_suc expr (VInt 1)
;;

let%test _ =
  let expr = parse_and_run_form "(* 1 2 3 4 5)" in
  test_suc expr (VInt 120)
;;

let%test _ =
  let expr = parse_and_run_prog "(+ 1 2 3 4 5)" in
  test_suc expr (VInt 15)
;;

(* let%test _ =
  let expr = Interpret.interpr_expr (Const (Int -1)) in
  test_suc expr (VInt (-1))
;;*)
