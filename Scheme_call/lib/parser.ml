open Opal
open Ast

let rec const_e input = (const_v => fun x -> Const x) input

and const_v input =
  choice
    [ (integer => fun i -> Int i)
    ; (boolean => fun b -> Bool b)
    ; (str => fun s -> String s)
    ]
    input

and integer =
  token "+"
  <|> token "-"
  <|> token ""
  >>= fun y ->
  many1 digit => implode % int_of_string => fun x -> if y = "-" then -x else x

and boolean = token "#t" <|> token "#f" => fun x -> x = "#t"

and str =
  spaces
  >> between (exactly '"') (exactly '"') (many (satisfy (fun x -> not (x = '"'))))
  => implode
  => fun x -> x
;;

let spec_initial =
  one_of [ '!'; '$'; '%'; '&'; '*'; '/'; ':'; '<'; '='; '>'; '?'; '^'; '_'; '~' ]
;;

let identifier =
  spaces
  >> (letter
     <|> spec_initial
     <~> many (alpha_num <|> spec_initial <|> one_of [ '+'; '-' ])
     => implode
     <|> token "+"
     <|> token "-")
;;

let parens = between (token "(" >> spaces) (token ")")
let parens_id = between (token "(" >> spaces) (token ")")
let ( <~~> ) x y = x >>= fun r -> y >>= fun rs -> return (r, rs)

let operator = function
  | hd :: tl -> return (ProcCall (Op hd, tl))
  | _ -> mzero
;;

let rec abreviation input =
  (spaces
  >> one_of [ '`'; '\''; ',' ]
  >>= fun x -> quote2 => fun datum -> DAbbreviation (x, datum))
    input

and qlist input =
  (between (token "(" >> spaces) (token ")") (many quote2) => fun xs -> DList xs) input

and const_d input =
  (spaces
  >> choice
       [ (integer => fun i -> DInt i)
       ; (boolean => fun b -> DBool b)
       ; (str => fun s -> DString s)
       ; (identifier => fun i -> DSymbol i)
       ])
    input

and quote2 input =
  (spaces >> choice [ (const_d => fun x -> DConst x); qlist; abreviation ]) input
;;

let quote input =
  (token "'"
  >> quote2
  <|> between (token "(" >> spaces) (token ")") (token "quote" >> quote2)
  => fun x -> Quote x)
    input
;;

let var = identifier => fun x -> Var x

let rec expr input =
  (spaces
  >> choice
       [ quasiquote; quote; conditional; derived_expr; lambda; proc_call; const_e; var ])
    input

and qqlist input =
  (between (token "(" >> spaces) (token ")") (many quasiquote2)
  => (fun xs -> QLList xs)
  <|> (token "'" >> quasiquote2 => fun x -> QLQuote x)
  <|> (token "`" >> quasiquote2 => fun x -> QLQuasiquote x)
  => fun qll -> QList qll)
    input

and unquote input =
  (token ","
  >> expr
  <|> between (token "(" >> spaces) (token ")") (token "unquote" >> expr)
  => fun x -> QUnquote x)
    input

and quasiquote2 input =
  (spaces >> choice [ (const_d => fun x -> QConst x); qqlist; unquote ]) input

and quasiquote input =
  (token "`"
  >> quasiquote2
  <|> between (token "(" >> spaces) (token ")") (token "quasiquote" >> quasiquote2)
  => fun x -> Quasiquote x)
    input

and proc_call input = parens (many expr >>= operator) input
and conditional input = (if_conditional <|> cond_conditional) input

and if_conditional input =
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

and cond_conditional input = parens (token "cond" >> cond_conditional2) input

and cond_conditional2 input =
  (parens (token "else" >> expr)
  <|> (between (token "(") (token ")") (many expr)
      >>= function
      | [ test; cons ] ->
        option
          (Cond (test, cons, None))
          (cond_conditional2 >>= fun alter -> return (Cond (test, cons, Some alter)))
      | [ test ] ->
        option
          (Cond (test, test, None))
          (cond_conditional2 >>= fun alter -> return (Cond (test, test, Some alter)))
      | _ -> mzero))
    input

and lambda input =
  parens
    (token "lambda"
    >> spaces
    >> formals
    >>= fun f ->
    many def
    >>= fun defs -> many1 expr => fun exprs -> Lam (f, defs, List.hd exprs, List.tl exprs)
    )
    input

and formals =
  parens_id (many identifier)
  => (fun fs -> FVarList fs)
  <|> (identifier => fun f -> FVar f)

and derived_expr input = (let_expr <|> letrec_expr) input

and let_expr input =
  between
    (token "(" >> spaces)
    (token ")")
    (token "let" >> (let_expr_without_tag <|> let_expr_with_tag))
    input

and let_expr_without_tag input =
  (between
     (token "(" >> spaces)
     (token ")")
     (many (between (token "(" >> spaces) (token ")") (identifier <~~> expr)))
  >>= fun l ->
  many def
  >>= fun defs ->
  many1 expr
  => fun exprs ->
  let names, objs = List.split l in
  ProcCall (Op (Lam (FVarList names, defs, List.hd exprs, List.tl exprs)), objs))
    input

and let_expr_with_tag input =
  (identifier
  >>= fun tag ->
  between
    (token "(" >> spaces)
    (token ")")
    (many (between (token "(" >> spaces) (token ")") (identifier <~~> expr)))
  >>= fun l ->
  many def
  >>= fun defs ->
  many1 expr
  => fun exprs ->
  let names, objs = List.split l in
  ProcCall
    ( Op
        (Lam
           ( FVarList []
           , [ tag, Lam (FVarList names, defs, List.hd exprs, List.tl exprs) ]
           , ProcCall (Op (ProcCall (Op (Lam (FVarList [], [], Var tag, [])), [])), objs)
           , [] ))
    , [] ))
    input

and letrec_expr input =
  between
    (token "(" >> spaces)
    (token ")")
    (token "letrec"
    >> between
         (token "(" >> spaces)
         (token ")")
         (many
            (between (token "(" >> spaces) (token ")" >> spaces) (identifier <~~> expr)))
    >>= fun defs ->
    many1 expr
    => fun exprs ->
    ProcCall (Op (Lam (FVarList [], defs, List.hd exprs, List.tl exprs)), []))
    input

and def_with_formals input =
  (parens_id (many1 identifier)
  >>= fun fs ->
  many def
  >>= fun defs ->
  many1 expr
  >>= fun expr ->
  match fs with
  | [] -> mzero
  | _ -> return (List.hd fs, Lam (FVarList (List.tl fs), defs, List.hd expr, List.tl expr))
  )
    input

and def_without_formals input =
  (identifier >>= fun var -> expr => fun expression -> var, expression) input

and def input =
  (spaces
  >> between
       (token "(" >> spaces)
       (token ")")
       (token "define" >> spaces >> (def_without_formals <|> def_with_formals)))
    input
;;

let form = def => (fun d -> Def d) <|> (expr => fun e -> Expr e)
let prog = many form << spaces << eof ()
let parse_prog str = parse prog (LazyStream.of_string str)
let parse_this parser str = parse parser (LazyStream.of_string str)
