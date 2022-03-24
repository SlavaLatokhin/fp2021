open Parser
open Ast

let ( let* ) m f = Result.bind m f
let return = Result.ok
let fail = Result.error

type value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VSymbol of id
  | VVar of string
  | VLambda of formals * expr * expr list option
  | VList of value list
[@@deriving show { with_path = false }]

and err =
  | IncorrectType of string
  | ParsingError
  | UnboundVariable
  | DivisionByZero
  | WrongNumber

and context = variable_t list

and variable_t =
  { variable_name : string
  ; variable_value : value
  }

let for_var context var =
  let rec helper = function
    | [] -> return (VVar var)
    | hd :: tl -> if hd.variable_name = var then return hd.variable_value else helper tl
  in
  helper context
;;

let for_add_or_mul operator start opers =
  let rec cata answer = function
    | [] -> return (VInt answer)
    | hd :: tl ->
      (match hd with
      | VInt i -> cata (operator i answer) tl
      | _ -> fail (IncorrectType "ERROR: real number required"))
  in
  cata start opers
;;

let eval_sub_or_div opers head operator flag =
  let rec cata answer = function
    | [] -> return (VInt answer)
    | hd :: tl ->
      (match hd with
      | VInt 0 when flag = 1 -> fail DivisionByZero
      | VInt i -> cata (operator answer i) tl
      | _ -> fail (IncorrectType "ERROR: real number required"))
  in
  cata head opers
;;

let for_sub_or_div operator start = function
  | [] -> fail WrongNumber
  | [ x ] -> eval_sub_or_div [ x ] start operator start
  | hd :: tl ->
    (match hd with
    | VInt i -> eval_sub_or_div tl i operator start
    | _ -> fail (IncorrectType "ERROR: real number required"))
;;

let eval_comparison opers left operator =
  let rec cata f = function
    | r :: xs ->
      (match r with
      | VInt s -> if operator f s then cata s xs else return (VBool false)
      | VVar _ -> fail UnboundVariable
      | _ -> fail (IncorrectType "ERROR: real number required"))
    | [] -> return (VBool true)
  in
  cata left opers
;;

let for_comparison operator = function
  | hd :: tl ->
    (match hd with
    | VInt i -> eval_comparison tl i operator
    | VVar _ -> fail UnboundVariable
    | _ -> fail (IncorrectType "ERROR: real number required"))
  | _ -> fail WrongNumber
;;

let for_or opers =
  let rec cata = function
    | hd :: tl ->
      (match hd with
      | VBool false -> cata tl
      | v -> return v)
    | [] -> return (VBool false)
  in
  cata opers
;;

let for_and opers =
  let rec cata ans = function
    | hd :: tl ->
      (match hd with
      | VBool false -> return hd
      | v -> cata v tl)
    | [] -> return ans
  in
  cata (VBool true) opers
;;

let for_for_max_or_min operator opers head =
  let rec cata ans = function
    | [] -> return (VInt ans)
    | hd :: tl ->
      (match hd with
      | VInt i -> cata (operator i ans) tl
      | _ -> fail (IncorrectType "ERROR: real number required"))
  in
  cata head opers
;;

let for_max_or_min operator = function
  | [] -> fail WrongNumber
  | [ x ] ->
    (match x with
    | VInt i -> return (VInt i)
    | _ -> fail (IncorrectType "ERROR: real number required"))
  | hd :: tl ->
    (match hd with
    | VInt i -> for_for_max_or_min operator tl i
    | _ -> fail (IncorrectType "ERROR: real number required"))
;;

let is_int_bool_str_list = function
  | 0, VInt _ -> return (VBool true)
  | 1, VBool _ -> return (VBool true)
  | 2, VString _ -> return (VBool true)
  | 3, VList _ -> return (VBool true)
  | _ -> return (VBool false)
;;

let for_display operand =
  let rec cata ppf = function
    | VInt i -> Format.printf "%d" i
    | VString s -> Format.printf "%s" s
    | VBool true -> Format.printf "#t"
    | VBool false -> Format.printf "#f"
    | VSymbol s -> Format.printf "%s" s
    | VLambda _ -> Format.printf "#<procedure>"
    | VVar v -> Format.printf "#<procedure %s>" v
    | VList l ->
      let _ = Format.printf "(" in
      let rec print_list ppf = function
        | [ x ] -> cata ppf x
        | hd :: tl -> Format.fprintf ppf "%a %a" cata hd print_list tl
        | _ -> ()
      in
      let _ = print_list ppf l in
      Format.printf ")"
  in
  let _ = cata Format.std_formatter operand in
  return (VString "#undef")
;;

let for_newline = function
  | [] ->
    let _ = Format.printf "\n" in
    return (VString "#undef")
  | _ -> fail WrongNumber
;;

let for_unary_operations operator = function
  | [ operand ] ->
    (match operator with
    | VVar "not" ->
      (match operand with
      | VBool b when b = false -> return (VBool true)
      | _ -> return (VBool false))
    | VVar "integer?" -> is_int_bool_str_list (0, operand)
    | VVar "boolean?" -> is_int_bool_str_list (1, operand)
    | VVar "string?" -> is_int_bool_str_list (2, operand)
    | VVar "list?" -> is_int_bool_str_list (3, operand)
    | VVar "zero?" -> return (VBool (operand = VInt 0))
    | VVar "positive?" -> return (VBool (operand > VInt 0))
    | VVar "negarive?" -> return (VBool (operand < VInt 0))
    | VVar "odd?" ->
      (match operand with
      | VInt i -> return (VBool (i mod 2 = 1))
      | _ -> fail (IncorrectType "ERROR: real number required"))
    | VVar "even?" ->
      (match operand with
      | VInt i -> return (VBool (i mod 2 = 0))
      | _ -> fail (IncorrectType "ERROR: real number required"))
    | VVar "abs" ->
      (match operand with
      | VInt i -> return (VInt (abs i))
      | _ -> fail (IncorrectType "ERROR: real number required"))
    | VVar "display" -> for_display operand
    | _ -> fail (IncorrectType "ERROR: invalid application"))
  | _ -> fail (IncorrectType "Wrong number of arguments")
;;

let rec for_dconst = function
  | DInt i -> return (VInt i)
  | DString s -> return (VString s)
  | DBool b -> return (VBool b)
  | DSymbol smbl -> return (VSymbol smbl)

and for_datum_list datum_list =
  let rec cata = function
    | [] -> return []
    | hd :: tl ->
      (match hd with
      | DConst d ->
        let* a = for_dconst d in
        let* b = cata tl in
        return (a :: b)
      | DList dl ->
        let* a = for_datum_list dl in
        let* b = cata tl in
        return (a :: b))
  in
  let* f = cata datum_list in
  return (VList f)
;;

let rec eval_expr context = function
  | EConst c ->
    (match c with
    | CInt i -> return (VInt i)
    | CString s -> return (VString s)
    | CBool b -> return (VBool b))
  | EVar v -> for_var context v
  | EQuote q ->
    (match q with
    | DConst d -> for_dconst d
    | DList dl -> for_datum_list dl)
  | ELambda (formals, command, sequence) -> return (VLambda (formals, command, sequence))
  | ECond (test, consequent, alternative) ->
    for_conditional context test consequent alternative
  | EProc_call (operator, operands) ->
    let* opers = for_opers context operands in
    let* oper = eval_expr context operator in
    for_proc_call context oper opers

and for_opers context operands =
  let rec helper = function
    | [] -> return []
    | hd :: tl ->
      let* e = eval_expr context hd in
      let* r = helper tl in
      return (e :: r)
  in
  helper operands

and for_conditional context test cons alt =
  let* expr = eval_expr context test in
  match expr with
  | VBool false ->
    (match alt with
    | Some v -> eval_expr context v
    | None -> fail (IncorrectType "#<undef>"))
  | _ -> eval_expr context cons

and for_lambda context formals command sequence operands =
  let rec helper new_context = function
    | [] -> fail (IncorrectType "ERROR: invalid application")
    | [ el ] -> eval_expr new_context el
    | hd :: tl ->
      let* _ = eval_expr new_context hd in
      helper new_context tl
  in
  match formals with
  | FVar x ->
    let* new_context =
      if check_if_var_in_context
           context
           { variable_name = x; variable_value = VList operands }
      then update_var context x (VList operands)
      else create_new_var context x (VList operands)
    in
    (match sequence with
    | None -> helper new_context [ command ]
    | Some y -> helper new_context (command :: y))
  | FVarList vars_name ->
    (match List.length vars_name = List.length operands with
    | false -> fail WrongNumber
    | true ->
      let formals_var =
        List.map2
          (fun x y -> { variable_name = x; variable_value = y })
          vars_name
          operands
      in
      let rec cata ans = function
        | [] -> return ans
        | hd :: tl ->
          if check_if_var_in_context context hd
          then
            let* temp = update_var context hd.variable_name hd.variable_value in
            cata temp tl
          else
            let* temp = create_new_var context hd.variable_name hd.variable_value in
            cata temp tl
      in
      let* new_context = cata context formals_var in
      (match sequence with
      | None -> helper new_context [ command ]
      | Some y -> helper new_context (command :: y)))

and check_if_var_in_context context var =
  List.exists (fun x -> x.variable_name = var.variable_name) context

and find_var_in_context context var_name =
  let rec helper name = function
    | [] -> fail UnboundVariable
    | hd :: tl ->
      if hd.variable_name = name then return hd.variable_value else helper name tl
  in
  helper var_name context

and create_new_var context var_name eval_value =
  return ({ variable_name = var_name; variable_value = eval_value } :: context)

and update_var context var_name eval_value =
  return
    ({ variable_name = var_name; variable_value = eval_value }
    :: List.filter (fun x -> x.variable_name <> var_name) context)

and for_definition context var_name var_value =
  let* eval_value = eval_expr context var_value in
  match
    check_if_var_in_context
      context
      { variable_name = var_name; variable_value = eval_value }
  with
  | false -> create_new_var context var_name eval_value
  | true -> update_var context var_name eval_value

and for_proc_call context oper opers =
  match oper with
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
  | VVar "list?" -> for_unary_operations oper opers
  | VVar "zero?" -> for_unary_operations oper opers
  | VVar "positive?" -> for_unary_operations oper opers
  | VVar "negative?" -> for_unary_operations oper opers
  | VVar "odd?" -> for_unary_operations oper opers
  | VVar "even?" -> for_unary_operations oper opers
  | VVar "abs" -> for_unary_operations oper opers
  | VVar "list" -> for_list context opers
  | VVar "apply" -> for_apply context opers
  | VVar "display" -> for_unary_operations oper opers
  | VVar "newline" -> for_newline opers
  | VLambda (formals, command, sequence) ->
    for_lambda context formals command sequence opers
  | _ -> fail (IncorrectType "ERROR: invalid application")

and for_list context opers =
  let rec cata = function
    | [] -> return []
    | VVar hd :: tl ->
      let* var = find_var_in_context context hd in
      let* r = cata tl in
      return (var :: r)
    | hd :: tl ->
      let* r = cata tl in
      return (hd :: r)
  in
  let* f = cata opers in
  return (VList f)

and for_apply context = function
  | [ x1; VList x2 ] -> for_proc_call context x1 x2
  | _ -> fail WrongNumber
;;

let eval_form context = function
  | FDef (var, expr) ->
    let* ans = for_definition context var expr in
    return (VVar var, ans)
  | FExpr expr ->
    let* ans = eval_expr context expr in
    return (ans, context)
;;

let run_program str =
  match Angstrom.parse_string ~consume:All prog str with
  | Ok v ->
    let rec helper aa bb = function
      | [] -> return (aa, bb)
      | hd :: tl ->
        let* a, b = eval_form bb hd in
        helper a b tl
    in
    helper (VString "#void") [] v
  | Error _ -> fail ParsingError
;;

let for_err = function
  | IncorrectType msg -> Format.printf "%s\n" msg
  | ParsingError -> Format.printf "ERROR: invalid syntax\n"
  | UnboundVariable -> Format.printf "ERROR: unbound variable\n"
  | DivisionByZero -> Format.printf "ERROR: attempt to calculate a division by zero\n"
  | WrongNumber -> Format.printf "ERROR: procedure requires at least one argument\n"
;;

(* let () =
  let str = {| (display(list 1 2)) |} in
  match run_program str with
  | Ok (ok, _) -> ()
  | Error err -> for_err err
;; *)
(* Format.printf "%a\n" pp_value ok *)
