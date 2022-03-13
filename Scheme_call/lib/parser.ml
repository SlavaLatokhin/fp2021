open Opal
open Ast

let rec const_e input = (choice [ integer; boolean; str ] => fun x -> Const x) input

and integer =
  token "+"
  <|> token "-"
  <|> token ""
  >>= fun y ->
  many1 digit => implode % int_of_string => fun x -> if y == "-" then Int (-x) else Int x

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
  <~> many (alpha_num <|> spec_initial <|> one_of [ '+'; '-' ])
  => implode
  <|> token "+"
  <|> token "-"
;;

let parens = between (spaces >> exactly '(' >> spaces) (spaces >> exactly ')')

let rec expr input = (choice [ conditional; lambda; proc_call; const_e; var ]) input
and proc_call input = parens (sep_by expr spaces >>= operator) input

and operator = function
  | h :: tl -> return (Proc_call (Op h, operand tl))
  | _ -> mzero

and operand = function
  | [] -> []
  | h :: tl -> List.concat [ [ h ]; operand tl ]

and var = spaces >> identifier => fun x -> Var x

and conditional input =
  parens
    (token "if"
    >> expr
    >>= fun test ->
    expr
    >>= fun cons ->
    option
      (Cond (test, cons, None))
      (expr >>= fun alter -> return (Cond (test, cons, Some alter))))
    input

and lambda input =
  parens
    (token "lambda"
    >> spaces
    
    >> formals
    >>= fun f -> spaces >> expr (**>>= fun e -> body*) => fun b -> Lam (f,(* e,*) b))
    input

and formals =
  between
    (spaces >> exactly '(' >> spaces)
    (spaces >> exactly ')')
    (many (spaces >> identifier))
  => (fun fs -> FVarList fs)
  <|> (spaces >> identifier => fun f -> FVar f)

and body input = (many expr => fun es -> es) input

let rec def input =
  between
    (spaces >> exactly '(' >> spaces)
    (spaces >> exactly ')')
    (token "define"
    >> spaces
    >> (def2 <|> def3))
    input
and def2 input = 
  (identifier
    >>= fun var -> expr => fun expression -> Def (var, expression)) input
and def3 input = 
  (between
    (spaces >> exactly '(' >> spaces)
    (spaces >> exactly ')') ((many (spaces >> identifier))) >>= fun fs -> expr => fun expression -> Def (List.hd fs, Lam (FVarList(List.tl fs), expression))) input

(* (many (spaces >> identifier))
  => (fun fs -> FVarList fs) *)

(* let quote input = token "'" >> between
    (spaces >> exactly '(' >> spaces)
    (spaces >> exactly ')')  *)
(* let err = any => mzero  *)
let form = def <|> (expr => fun e -> Expr e)
let prog = sep_by form spaces << spaces << eof ()
let parse_prog str = parse prog (LazyStream.of_string str)
let parse_this parser str = parse parser (LazyStream.of_string str)

let () =
  let input =
    LazyStream.of_string
      "(define (a) 5)"
  in
  match parse prog input with
  | Some ans -> Format.printf "Actual: %a\n" pp_program ans
  | None -> print_endline "ERROR!"
;;

(* let%test _ =
  let input =
    LazyStream.of_string
      "(define Y                  \n\
      \    ( lambda (f)             \n\
      \      (lambda (g) (g g)  )   \n\
      \       (lambda (g)\n\
      \         (f (lambda a (apply (g g) a))))))\n\
      \      "
  in
  match parse expr input with
  | Some ans -> ans = Const (Int 3)
  | None -> print_endline "ERROR!"
;; *)

(*
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
