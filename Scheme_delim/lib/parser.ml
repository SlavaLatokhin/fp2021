open Angstrom
open Ast

let is_space = function
  | ' ' | '\t' | '\n' -> true
  | _ -> false
;;

let is_char = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_special_initial = function
  | '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '^' | '_' | '~' ->
    true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_special_subsequent = function
  | '+' | '-' | '.' | '@' -> true
  | _ -> false
;;

let spaces = skip_while is_space
let token x = spaces *> string x
let parens p = spaces *> (token "(" *> p <* token ")")
let ( <~> ) x xs = x >>= fun r -> xs >>= fun rs -> return (r :: rs)
let ( <-> ) x y = x >>= fun m -> y >>= fun n -> return (m, n)

let integer =
  spaces *> choice [ token "-"; token "+"; token "" ]
  >>= fun t ->
  take_while1 is_digit >>| int_of_string >>| fun x -> if t = "-" then -x else x
;;

let boolean = spaces *> (token "#t" *> return true <|> token "#f" *> return false)
let for_str p = spaces *> (char '"' *> p <* char '"')

let str =
  for_str (many (satisfy (fun x -> not (x = '"' or x == '\\'))))
  >>| fun ss -> String.of_seq (List.to_seq ss)
;;

let initial = satisfy is_char <|> satisfy is_special_initial
let subsequent = initial <|> satisfy is_digit <|> satisfy is_special_subsequent

let identifier =
  spaces
  *> (choice [ string "+"; string "-"; string "..." ]
     >>| (fun ss -> ss)
     <|> (initial <~> many subsequent >>| fun ss -> String.of_seq (List.to_seq ss)))
;;

let formals =
  spaces
  *> (parens (many identifier)
     >>| (fun vl -> FVarList vl)
     <|> (identifier >>| fun v -> FVar v))
;;

let datum =
  fix (fun datum ->
      let simple_datum =
        choice
          [ (boolean >>| fun b -> DBool b)
          ; (integer >>| fun i -> DInt i)
          ; (str >>| fun s -> DString s)
          ; (identifier >>| fun id -> DSymbol id)
          ]
        >>| fun x -> DConst x
      in
      let compound_datum = parens (many datum) >>| fun d -> DList d in
      spaces *> (simple_datum <|> compound_datum))
;;

let expr =
  fix (fun expr ->
      spaces
      *>
      let quotation =
        token "'" *> datum
        >>= (fun d -> return (EQuote d))
        <|> parens (token "quote" *> datum >>= fun d -> return (EQuote d))
      in
      let lambda_expr =
        parens
          (token "lambda" *> formals
          >>= fun fs ->
          expr
          >>= fun e ->
          option
            (ELambda (fs, e, None))
            (many1 expr >>= fun me -> return (ELambda (fs, e, Some me))))
      in
      let conditional =
        parens
          (token "if" *> expr
          >>= fun test ->
          expr
          >>= fun consequent ->
          option
            (ECond (test, consequent, None))
            (expr
            >>= fun alternative -> return (ECond (test, consequent, Some alternative))))
      in
      let cond_clause =
        fix (fun cond_clause ->
            spaces
            *> (parens (token "else" *> expr)
               <|> (parens (expr <-> expr)
                   >>= fun (test, sequence) ->
                   option
                     (ECond (test, sequence, None))
                     (cond_clause >>= fun cc -> return (ECond (test, sequence, Some cc)))
                   )))
      in
      let cond = parens (token "cond" *> cond_clause) in
      let proc_call =
        parens
          (sep_by1 spaces expr >>= fun xs -> return (EProc_call (List.hd xs, List.tl xs)))
      in
      spaces
      *> choice
           [ quotation
           ; lambda_expr
           ; conditional
           ; cond
           ; proc_call
           ; (integer >>| fun i -> EConst (CInt i))
           ; (boolean >>| fun b -> EConst (CBool b))
           ; (str >>| fun s -> EConst (CString s))
           ; (identifier >>| fun v -> EVar v)
           ])
;;

let definition =
  spaces
  *> parens
       (token "define"
       *> (identifier
          >>= (fun var -> expr >>| fun e -> FDef (var, e))
          <|> (parens (spaces *> many identifier)
              >>= fun fs ->
              spaces *> expr
              >>= fun e ->
              option
                (FDef (List.hd fs, ELambda (FVarList (List.tl fs), e, None)))
                (spaces *> many expr
                >>| fun me ->
                FDef (List.hd fs, ELambda (FVarList (List.tl fs), e, Some me))))))
;;

let prog = sep_by spaces (definition <|> (expr >>| fun x -> FExpr x)) <* spaces

(* let () =
  let str =
    {| ((lambda (x) 
   (list x (list (quote quote) x))) 
  (quote 
     (lambda (x) 
       (list x (list (quote quote) x))))) |}
  in
  match parse_string ~consume:All prog str with
  | Ok v -> Format.printf "%a\n" pp_program v
  | Error _ -> Format.printf "ERROR: invalid syntax\n"
;; *)
