open Opal
open Ast

let rec const_e input = (choice [ integer; boolean; str ] => fun x -> Const x) input

and integer =
  token "+"
  <|> token "-"
  <|> token ""
  >>= fun y ->
  many1 digit
  => implode % int_of_string
  => fun x -> if y == "-" then NegInt x else PosInt x

and boolean =
  token "#t" <|> token "#f" => fun x -> if x == "#t" then Bool true else Bool false

and str =
  between (exactly '"') (exactly '"') (many (satisfy (fun x -> not (Char.equal '"' x))))
  => implode
  >>= fun x -> return (String x)
;;

let spec_initial =
  one_of [ '!'; '$'; '%'; '&'; '*'; '/'; ':'; '<'; '='; '>'; '?'; '^'; '_'; '~' ]
;;

let identifier =
  letter
  <|> spec_initial
  <~> many (alpha_num <|> spec_initial)
  => implode
  <|> token "+"
  <|> token "-"
;;

let my_sep expr ast sep = expr <~> sep ast
let parens = between (spaces >> exactly '(' >> spaces) (spaces >> exactly ')')

let rec expr input = (choice [ const_e; var; conditional; lambda; proc_call ]) input
and proc_call input = parens (sep_by expr spaces >>= operator) input

and operator = function
  | h :: tl -> return (Proc_call (Op h, operand tl))
  | _ -> mzero

and operand = function
  | [] -> []
  | h :: tl -> List.concat [ [ Obj h ]; operand tl ]

and var = identifier => fun x -> Var x

and conditional input =
  parens
    (token "if"
    >> expr
    >>= fun test ->
    expr
    >>= fun cons ->
    option (Cond (test, cons, None)) (expr => fun alter -> Cond (test, cons, Some alter))
    )
    input

and lambda input =
  parens
    (token "lambda"
    >> spaces
    >> formals
    >>= fun f -> spaces >> expr >>= fun e -> body => fun b -> Lam (f, e, b))
    input

and formals =
  between
    (spaces >> exactly '(' >> spaces)
    (spaces >> exactly ')')
    (many (spaces >> identifier))
  => (fun l -> FVarList l)
  <|> (spaces >> identifier => fun l -> FVar l)

and body input = (many expr => fun es -> es) input

let def input =
  between
    (spaces >> exactly '(' >> spaces)
    (spaces >> exactly ')')
    (token "define"
    >> spaces
    >> identifier
    >>= fun var -> expr => fun expression -> Def (var, expression))
    input
;;

let prog = many (def <|> (expr => fun e -> Expr e))

let () =
  let input =
    LazyStream.of_string
      "\n\
      \  (define Y                  \n\
      \    ( lambda (f)             \n\
      \      (lambda (g) (g g)  )   \n\
      \       (lambda (g)\n\
      \         (f (lambda a (apply (g g) a))))))"
  in
  match parse prog input with
  | Some ans -> Format.printf "Actual: %a\n" pp_program ans
  | None -> print_endline "ERROR!"
;;

(* open Opal
open Ast

let integer = many1 digit => implode % int_of_string => fun x -> EConst (CInt x)
(* let add = exactly '+' >> return (fun x y -> Add (x, y))
let sub = exactly '-' >> return (fun x y -> Sub (x, y))
let mul = exactly '*' >> return (fun x y -> Mul (x, y))
let div = exactly '/' >> return (fun x y -> Div (x, y))  *)

(* let rec expr input = chainl1 term (add <|> sub) input
and term input = chainl1 factor (mul <|> div) input
and factor input = (parens expr <|> integer) input *)

let parence_op c =
  between (spaces >> exactly '(' >> spaces >> token c >> spaces) (spaces >> exactly ')')
;;

(* let parence = between (spaces >> exactly '(' >> spaces) (spaces >> exactly ')') *)

let rec
    (*una_expr input = choice [ not; evenq; intq ] input
and not = token "not" >> bin_expr >>= fun x -> return (Not x)
(*????????????????????????????????????????????????????????*)

and evenq = token "even?" >> bin_expr >>= fun x -> return (EvenQ x)
(*????????????????????????????????????????????????????????*)

and intq = token "integer?" >> bin_expr >>= fun x -> return (IntQ x)
(*????????????????????????????????????????????????????????*)

and *)
    bin_expr
    input
  =
  choice
    [ parence_op "+" (bin_op add)
    ; parence_op "-" (bin_op sub)
    ; parence_op "*" (bin_op mul)
    ; parence_op "/" (bin_op div)
    ; parence_op "=" (bin_op equ)
    ; parence_op "<" (bin_op les)
    ; parence_op ">" (bin_op mor) (* ; parence_op "not" (una_op not) *)
(**    ; parence una_op (*????????????????????????????????????????????????????????*)*)
    ; integer
    ]
    input

and bin_op op = chainl1 bin_expr (* <|> una_expr*) op
(*????????????????????????????????????????????????????????*)

(* and una_op = chainl1 una_expr add *)
(*????????????????????????????????????????????????????????*)

and add = spaces >> return (fun x y -> Add (x, y)) (* "+" *)

and sub = spaces >> return (fun x y -> Sub (x, y)) (* "-" *)

and mul = spaces >> return (fun x y -> Mul (x, y)) (* "*" *)

and div = spaces >> return (fun x y -> Div (x, y)) (* "/" *)

and equ = spaces >> return (fun x y -> Equ (x, y)) (* "=" *)

and les = spaces >> return (fun x y -> Les (x, y)) (* "<" *)

and mor = spaces >> return (fun x y -> Les (y, x))
(* ">" *)

let () =
  let input = LazyStream.of_string " (+ 1 (- 1 (* 5 5) (/ 3 2)))" in
  match parse bin_expr input with
  | Some ans -> Format.printf "Actual: %a\n" pp_expr ans
  | None -> print_endline "ERROR!"
;; *)

(* open Angstrom
open Ast

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let parens p = char '(' *> p <* char ')'

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let expr =
  fix (fun expr ->
      let integer =
        take_while1 (function
            | '0' .. '9' -> true
            | _ -> false)
        >>| fun x -> EConst (CInt (int_of_string x))
      in
      let factor = parens expr <|> integer in
      let add = parens *> char '+' *> return (fun x y -> Add (x, y)) in
      let sub = char '-' *> return (fun x y -> Sub (x, y)) in
      let mul = char '*' *> return (fun x y -> Mul (x, y)) in
      let div = char '/' *> return (fun x y -> Div (x, y)) in
      let term = chainl1 factor (mul <|> div) in
      chainl1 term (add <|> sub))
      (* add  *)
;;

(* let () =
  let pp_result ppf = function
    | Ok x | Error x -> Format.pp_print_string ppf x
  in
  Format.printf "%a\n%!" pp_result @@ parse_string ~consume:Consume.All expr "1*0+0*0"
;; *)

let parse_test p str = parse_string ~consume:All p str

let parse_test_suc pp p str exp =
  match parse_test p str with
  | Error err ->
    print_string err;
    flush stdout;
    false
  | Ok ok when exp = ok -> true
  | Ok ok ->
    Format.printf "Expected: %a\nActual: %a\n" pp exp pp ok;
    false
;;

(* let test_for_parser code expected =
  match parse_with progr code with
  | Ok ok ->
    (match List.equal equal_decl ok expected with
    | true -> true
    | false ->
      Format.printf "Expected: %a\nActual: %a\n" pp_program expected pp_program ok;
      false)
  | Error err ->
    Format.printf "Error: %s\n" err;
    false
;; *)

let expr_test_suc = parse_test_suc pp_expr expr

let%test _ =
  expr_test_suc "1+2+15" (Add (Add (EConst (CInt 1), EConst (CInt 2)), EConst (CInt 15)))
;;

(* let%test _ =
  parse_string ~consume:Consume.All demo_prio "0*0+0*0"
  = Ok "Add (Mul (0, 0), Mul (0, 0))"
;;

let%test _ = parse_string ~consume:Consume.All demo_prio "0-0" = Ok "Sub (0, 0)"

let%test _ =
  parse_string ~consume:Consume.All demo_prio "0*0-0" = Ok "Sub (Mul (0, 0), 0)"
;;

let%test _ =
  parse_string ~consume:Consume.All demo_prio "0*0-0+0/0"
  = Ok "(Mul (0, 0), Add (0, Div (0, 0)))"
;; *) *)
