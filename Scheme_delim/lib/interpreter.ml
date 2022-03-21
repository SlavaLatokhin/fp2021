open Parser
open Ast

let ( let* ) m f = Result.bind m f
let return = Result.ok
let fail = Result.error

type value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VVar of string
  | VLambda of formals * expr * expr list option
  | VLiso of value list
[@@deriving show { with_path = false }]

and err = IncorrectType of string
and context = variable_t list

and variable_t =
  { variable_name : string
  ; variable_value : value
  }

let rec eval_expr context expr =
  match expr with
  | EConst c ->
    (match c with
    | CInt i -> return (VInt i)
    | CString s -> return (VString s)
    | CBool b -> return (VBool b))
  | EVar v -> return (VVar v)
  | ELambda (formals, command, sequence) -> return (VLambda (formals, command, sequence))
  | ECond (test, consequent, alternative) ->
    for_conditional context test consequent alternative
  | EProc_call (operator, operands) ->
    let* opers = for_opers context operands in
    let* oper = eval_expr context operator in
    (match oper with
    | VVar "+" -> for_add_or_mul ( + ) 0 opers
    | VVar "-" -> for_sub_or_div ( - ) 0 opers
    | VVar "*" -> for_add_or_mul ( * ) 1 opers
    | VVar "/" -> for_sub_or_div ( / ) 1 opers
    | VVar "=" -> for_comparison ( = ) opers
    | VVar ">" -> for_comparison ( > ) opers
    | VVar "<" -> for_comparison ( < ) opers
    | VVar ">=" -> for_comparison ( >= ) opers
    | VVar "<=" -> for_comparison ( <= ) opers
    | VVar "or" -> for_or opers
    | VVar "and" -> for_and opers
    | VVar "max" -> for_max_or_min max opers
    | VVar "min" -> for_max_or_min min opers
    | VVar "not" -> for_unary_operations oper opers
    | VVar "integer?" -> for_unary_operations oper opers
    | VVar "boolean?" -> for_unary_operations oper opers
    | VVar "string?" -> for_unary_operations oper opers
    | VVar "zero?" -> for_unary_operations oper opers
    | VVar "positive?" -> for_unary_operations oper opers
    | VVar "negative?" -> for_unary_operations oper opers
    | VVar "odd?" -> for_unary_operations oper opers
    | VVar "even?" -> for_unary_operations oper opers
    | VVar "abs" -> for_unary_operations oper opers
    | VLambda (formals, command, sequence) ->
      for_lambda context formals command sequence opers
    | _ -> fail (IncorrectType "Проц колл не такой умный"))
  | _ -> fail (IncorrectType "Грустно, что этого еще нет")

and for_opers context operands =
  let rec helper answer = function
    | [] -> return answer
    | hd :: tl ->
      let* e = eval_expr context hd in
      helper (e :: answer) tl
  in
  helper [] operands

and check_if_var_in_context context var =
  let rec helper name = function
    | [] -> false
    | hd :: tl -> if hd.variable_name = name then true else helper name tl
  in
  helper var.variable_name context

and create_new_var context var_name eval_value =
  return ({ variable_name = var_name; variable_value = eval_value } :: context)

and update_var context var_name eval_value =
  let rec helper name answer = function
    | [] -> fail (IncorrectType "кто-то удалил переменную, хотя ее уже видели :(")
    | hd :: tl ->
      if hd.variable_name = var_name
      then
        return (answer @ ({ variable_name = var_name; variable_value = eval_value } :: tl))
      else helper name (hd :: answer) tl
  in
  helper var_name [] context

and for_definition context var_name var_value =
  let* eval_value = eval_expr context var_value in
  let* new_context =
    match
      check_if_var_in_context
        context
        { variable_name = var_name; variable_value = eval_value }
    with
    | false -> create_new_var context var_name eval_value
    | true -> update_var context var_name eval_value
  in
  return new_context

and for_add_or_mul operator start opers =
  let rec cata answer = function
    | [] -> answer
    | hd :: tl ->
      (match hd with
      | VInt i ->
        let* a = cata answer tl in
        return (operator i a)
      | _ -> fail (IncorrectType "Broken add/mul operation"))
  in
  let* ans = cata (Result.ok start) opers in
  return (VInt ans)

and for_sub_or_div operator start = function
  | [] -> fail (IncorrectType "ccccc не делай так")
  | [ x ] -> for_for_sub_or_div [ x ] start operator start
  | hd :: tl ->
    (match hd with
    | VInt i -> for_for_sub_or_div tl i operator start
    | _ -> fail (IncorrectType "Broken sub/div operation"))

and for_for_sub_or_div opers head operator flag =
  let rec cata answer = function
    | [] -> answer
    | hd :: tl ->
      (match hd with
      | VInt 0 when flag = 1 -> fail (IncorrectType "Деление на 0")
      | VInt i ->
        let* a = cata answer tl in
        return (operator a i)
      | _ -> fail (IncorrectType "Broken sub/div operation"))
  in
  let* ans = cata (Result.Ok head) opers in
  return (VInt ans)

and for_comparison op = function
  | hd :: tl ->
    (match hd with
    | VInt i -> for_for_comparison tl i op
    | _ -> fail (IncorrectType "Broken operation в персом элементе"))
  | _ -> fail (IncorrectType "Бяка")

and for_for_comparison opers left op =
  let rec cata f = function
    | r :: xs ->
      (match r with
      | VInt s ->
        if op f s
        then
          let* a = cata s xs in
          return a
        else return (VBool false)
      | _ -> fail (IncorrectType "Сравнение сломалось"))
    | [] -> return (VBool true)
  in
  let* ans = cata left opers in
  return ans

and for_or opers =
  let rec cata = function
    | hd :: tl ->
      (match hd with
      | VBool b when b = false ->
        let* a = cata tl in
        return a
      | v -> return v)
    | [] -> return (VBool false)
  in
  let* ans = cata opers in
  return ans

and for_and opers =
  let rec cata ans = function
    | hd :: tl ->
      (match hd with
      | VBool b when b = false -> return (VBool false)
      | v -> cata v tl)
    | [] -> return ans
  in
  let* ans = cata (VBool true) opers in
  return ans

and for_max_or_min op = function
  | [] -> fail (IncorrectType "Так тоже не делай")
  | [ x ] ->
    (match x with
    | VInt i -> return (VInt i)
    | _ -> fail (IncorrectType "Broken max operation"))
  | hd :: tl ->
    (match hd with
    | VInt i -> for_for_max_or_min op tl i
    | _ -> fail (IncorrectType "Broken max operation"))

and for_for_max_or_min op opers head =
  let rec cata answer = function
    | [] -> answer
    | hd :: tl ->
      (match hd with
      | VInt i ->
        let* a = cata answer tl in
        return (op i a)
      | _ -> fail (IncorrectType "Broken max operation"))
  in
  let* ans = cata (Result.Ok head) opers in
  return (VInt ans)

and for_unary_operations operator = function
  | [ x ] ->
    (match operator with
    | VVar "not" ->
      (match x with
      | VBool b when b = false -> return (VBool true)
      | _ -> return (VBool false))
    | VVar "integer?" -> is_int_bool_str x 0
    | VVar "boolean?" -> is_int_bool_str x 1
    | VVar "string?" -> is_int_bool_str x 2
    | VVar "zero?" -> return (VBool (x = VInt 0))
    | VVar "positive?" -> return (VBool (x > VInt 0))
    | VVar "negarive?" -> return (VBool (x < VInt 0))
    | VVar "odd?" ->
      (match x with
      | VInt i -> return (VBool (i mod 2 = 1))
      | _ -> fail (IncorrectType "integer required"))
    | VVar "even?" ->
      (match x with
      | VInt i -> return (VBool (i mod 2 = 0))
      | _ -> fail (IncorrectType "integer required"))
    | VVar "abs" ->
      (match x with
      | VInt i -> return (VInt (abs i))
      | _ -> fail (IncorrectType "integer required"))
    | _ -> fail (IncorrectType "такой унарной нет"))
  | _ -> fail (IncorrectType "Wrong number of arguments")

and is_int_bool_str flag expr =
  match expr, flag with
  | 0, VInt _ -> return (VBool true)
  | 1, VBool _ -> return (VBool true)
  | 2, VString _ -> return (VBool true)
  | _ -> return (VBool false)

and for_conditional context test cons alt =
  let* expr = eval_expr context test in
  match expr with
  | VBool false ->
    (match alt with
    | Some v ->
      let* ans = eval_expr context v in
      return ans
    | None -> fail (IncorrectType "#<undef>"))
  | _ ->
    let* ans = eval_expr context cons in
    return ans

and for_lambda context formals command sequence operands =
  match formals with
  | FVar x ->
    let* new_context =
      if check_if_var_in_context
           context
           { variable_name = x; variable_value = VLiso operands }
      then update_var context x (VLiso operands)
      else create_new_var context x (VLiso operands)
    in
    let rec helper = function
      | [] -> fail (IncorrectType "Недостижима")
      | [ el ] -> eval_expr new_context el
      | hd :: tl ->
        let* _ = eval_expr new_context hd in
        helper tl
    in
    (match sequence with
    | None -> helper [ command ]
    | Some y -> helper (command :: y))
  | FVarList _ -> fail (IncorrectType "sssss")
;;

(* and for_lambda_context context var =
  List.filter (fun ctx_var -> ctx_var.variable_name <> var) context
;; *)

(* and subs_var context expr = eval_expr *)

(* let run_expr str =
  match Angstrom.parse_string ~consume:All expr str with
  | Ok v -> eval_expr context v
  | Error _ -> fail (IncorrectType "Печаль")
;; *)

let eval_form context = function
  | FDef (var, expr) ->
    let* a = for_definition context var expr in
    return (VVar var, a)
  | FExpr expr ->
    let* a = eval_expr context expr in
    return (a, context)
;;

let run_program str =
  match Angstrom.parse_string ~consume:All prog str with
  | Ok v ->
    let context = [] in
    let rec helper ans = function
      | [] -> return ans
      | hd :: tl ->
        let* ans = eval_form context hd in
        helper ans tl
    in
    helper (VInt 0, context) v
  | Error _ -> fail (IncorrectType "Печаль")
;;

let () =
  let str = "((lambda x (+ x 1) ) 1)" in
  match run_program str with
  | Ok (ok, _) -> Format.printf "%a\n" pp_value ok
  | Error (IncorrectType err) -> Printf.printf "%s\n" err
;;
