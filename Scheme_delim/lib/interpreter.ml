open Parser
open Ast

let ( let* ) m f = Result.bind m f
let return = Result.ok
let fail = Result.error
let ( <*> ) f x = Result.bind f (fun f -> Result.bind x (fun x -> Result.Ok (f x)))

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

let eval_add_mul_sub_div operator start opers =
  let rec cata answer = function
    | [] -> return (VInt answer)
    | hd :: tl ->
      (match hd with
      | VInt i ->
        (match operator answer i with
        | a -> cata a tl
        | exception Division_by_zero -> fail DivisionByZero)
      | _ -> fail (IncorrectType "ERROR: real number required"))
  in
  cata start opers
;;

let for_sub_or_div operator start = function
  | [] -> fail WrongNumber
  | [ x ] -> eval_add_mul_sub_div operator start [ x ]
  | hd :: tl ->
    (match hd with
    | VInt i -> eval_add_mul_sub_div operator i tl
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

let for_display operand =
  let rec cata ppf = function
    | VInt i -> Format.fprintf ppf "%d" i
    | VString s -> Format.fprintf ppf "%s" s
    | VBool true -> Format.fprintf ppf "#t"
    | VBool false -> Format.fprintf ppf "#f"
    | VSymbol s -> Format.fprintf ppf "%s" s
    | VLambda _ -> Format.fprintf ppf "#<procedure>"
    | VVar v -> Format.fprintf ppf "#<procedure %s>" v
    | VList l ->
      let _ = Format.fprintf ppf "(" in
      let rec print_list ppf = function
        | [ x ] -> cata ppf x
        | hd :: tl -> Format.fprintf ppf "%a %a" cata hd print_list tl
        | _ -> ()
      in
      let _ = print_list ppf l in
      Format.fprintf ppf ")"
  in
  let _ = cata Format.std_formatter operand in
  return (VString "#undef")
;;

let for_newline = function
  | [] ->
    let _ = Format.fprintf Format.std_formatter "\n" in
    return (VString "#undef")
  | _ -> fail WrongNumber
;;

let for_unary_operations = function
  | VVar "not", [ operand ] ->
    (match operand with
    | VBool false -> return (VBool true)
    | _ -> return (VBool false))
  | VVar "integer?", [ operand ] ->
    (match operand with
    | VInt _ -> return (VBool true)
    | _ -> return (VBool false))
  | VVar "boolean?", [ operand ] ->
    (match operand with
    | VBool _ -> return (VBool true)
    | _ -> return (VBool false))
  | VVar "string?", [ operand ] ->
    (match operand with
    | VString _ -> return (VBool true)
    | _ -> return (VBool false))
  | VVar "list?", [ operand ] ->
    (match operand with
    | VList _ -> return (VBool true)
    | _ -> return (VBool false))
  | VVar "zero?", [ VInt operand ] -> return (VBool (operand = 0))
  | VVar "positive?", [ VInt operand ] -> return (VBool (operand > 0))
  | VVar "negative?", [ VInt operand ] -> return (VBool (operand < 0))
  | VVar "odd?", [ VInt operand ] -> return (VBool (operand mod 2 = 1))
  | VVar "even?", [ VInt operand ] -> return (VBool (operand mod 2 = 0))
  | VVar "abs", [ VInt operand ] -> return (VInt (abs operand))
  | VVar "display", [ operand ] -> for_display operand
  | _, [ _ ] -> fail (IncorrectType "ERROR: invalid variable type")
  | _ -> fail (IncorrectType "Wrong number of arguments")
;;

let rec mapm f = function
  | [] -> return []
  | x :: xs -> return List.cons <*> f x <*> mapm f xs
;;

let for_dconst = function
  | DInt i -> return (VInt i)
  | DString s -> return (VString s)
  | DBool b -> return (VBool b)
  | DSymbol smbl -> return (VSymbol smbl)
;;

let rec for_datum_list datum_list =
  let* a =
    mapm
      (function
        | DConst d -> for_dconst d
        | DList dl -> for_datum_list dl)
      datum_list
  in
  return (VList a)
;;

let check_if_var_in_context context var =
  List.exists (fun x -> x.variable_name = var.variable_name) context
;;

let find_var_in_context context var_name =
  let rec helper name = function
    | [] -> fail UnboundVariable
    | hd :: tl ->
      if hd.variable_name = name then return hd.variable_value else helper name tl
  in
  helper var_name context
;;

let create_new_var context variable_name variable_value =
  return ({ variable_name; variable_value } :: context)
;;

let update_var context variable_name variable_value =
  return
    ({ variable_name; variable_value }
    :: List.filter (fun x -> x.variable_name <> variable_name) context)
;;

let for_list context opers =
  let* a =
    mapm
      (function
        | VVar var -> find_var_in_context context var
        | y -> return y)
      opers
  in
  return (VList a)
;;

let create_new_context context variable_name variable_value =
  if check_if_var_in_context context { variable_name; variable_value }
  then update_var context variable_name variable_value
  else create_new_var context variable_name variable_value
;;

let rec eval_expr context = function
  | EConst (CInt i) -> return (VInt i)
  | EConst (CString s) -> return (VString s)
  | EConst (CBool b) -> return (VBool b)
  | EVar v -> for_var context v
  | EQuote (DConst d) -> for_dconst d
  | EQuote (DList dl) -> for_datum_list dl
  | ELambda (formals, command, sequence) -> return (VLambda (formals, command, sequence))
  | ECond (test, consequent, alternative) ->
    for_conditional context test consequent alternative
  | EProc_call (operator, operands) ->
    let* opers = for_opers context operands in
    let* oper = eval_expr context operator in
    for_proc_call context oper opers

and for_opers context operands = mapm (fun x -> eval_expr context x) operands

and for_conditional context test cons alt =
  let* expr = eval_expr context test in
  match expr, alt with
  | VBool false, Some v -> eval_expr context v
  | VBool false, None -> fail (IncorrectType "#<undef>")
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
    let* new_context = create_new_context context x (VList operands) in
    (match sequence with
    | None -> helper new_context [ command ]
    | Some y -> helper new_context (command :: y))
  | FVarList vars_name ->
    (match List.length vars_name = List.length operands with
    | false -> fail WrongNumber
    | true ->
      let formals_var =
        List.map2
          (fun variable_name variable_value -> { variable_name; variable_value })
          vars_name
          operands
      in
      let rec cata ans = function
        | [] -> return ans
        | hd :: tl ->
          let* temp = create_new_context context hd.variable_name hd.variable_value in
          cata temp tl
      in
      let* new_context = cata context formals_var in
      (match sequence with
      | None -> helper new_context [ command ]
      | Some y -> helper new_context (command :: y)))

and for_proc_call context oper opers =
  match oper with
  | VVar "+" -> eval_add_mul_sub_div ( + ) 0 opers
  | VVar "-" -> for_sub_or_div ( - ) 0 opers
  | VVar "*" -> eval_add_mul_sub_div ( * ) 1 opers
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
  | VVar "not"
  | VVar "integer?"
  | VVar "boolean?"
  | VVar "string?"
  | VVar "list?"
  | VVar "zero?"
  | VVar "positive?"
  | VVar "negative?"
  | VVar "odd?"
  | VVar "even?"
  | VVar "abs"
  | VVar "display" -> for_unary_operations (oper, opers)
  | VVar "list" -> for_list context opers
  | VVar "apply" -> for_apply context opers
  | VVar "newline" -> for_newline opers
  | VLambda (formals, command, sequence) ->
    for_lambda context formals command sequence opers
  | _ -> fail (IncorrectType "ERROR: invalid application")

and for_apply context = function
  | [ x1; VList x2 ] -> for_proc_call context x1 x2
  | _ -> fail WrongNumber
;;

let for_definition context var_name var_value =
  let* eval_value = eval_expr context var_value in
  create_new_context context var_name eval_value
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

let for_err fmt = function
  | IncorrectType msg -> Format.fprintf fmt "%s\n" msg
  | ParsingError -> Format.fprintf fmt "ERROR: invalid syntax\n"
  | UnboundVariable -> Format.fprintf fmt "ERROR: unbound variable\n"
  | DivisionByZero ->
    Format.fprintf fmt "ERROR: attempt to calculate a division by zero\n"
  | WrongNumber -> Format.fprintf fmt "ERROR: procedure requires at least one argument\n"
;;

(* let () =
  let str =
    {|  

     |}
  in
  match run_program str with
  | Ok _ -> ()
  | Error err -> for_err Format.err_formatter err
;; *)

(* Format.printf "%a\n" pp_value ok *)
