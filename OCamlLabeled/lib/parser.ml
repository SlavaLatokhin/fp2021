open Angstrom
open Ast
open Base

let parse p s = parse_string ~consume:All p s

let is_empty = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_upper = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_wild = function
  | '_' -> true
  | _ -> false
;;

let is_valid_ident_symb c = is_digit c || is_lower c || is_upper c || is_wild c

let is_keyword = function
  | "let"
  | "in"
  | "true"
  | "false"
  | "rec"
  | "fun"
  | "if"
  | "else"
  | "then"
  | "with"
  | "function"
  | "match" -> true
  | _ -> false
;;

let empty = take_while is_empty
let empty_lr p = empty *> p <* empty
let token s = empty *> string s

let keyword s =
  token s
  <* (peek_char
     >>| function
     | Some x when is_valid_ident_symb x -> fail "Incorrect keyword"
     | _ -> return None)
;;

let lp = token "("
let rp = token ")"
let rsb = token "]"
let lsb = token "["
let comma = token ","
let colon = token ":"
let semi = token ";"
let semisemi = token ";;"
let bar = token "|"
let arrow = token "->"
let between l r p = l *> p <* r
let parens p = lp *> p <* rp
let cint n = CInt n
let cbool b = CBool b
let cstring s = CString s
let econst c = EConst c
let eunop o e = EUnOp (o, e)
let evar id = EVar id
let etuple l = ETuple l
let econs e1 e2 = ECons (e1, e2)
let eif e1 e2 e3 = EIf (e1, e2, e3)
let elet binds e = ELet (binds, e)
let efunction cases = EFun (PVar "match", EMatch (EVar "match", cases))
let efun p e = EFun (p, e)
let eapp = return (fun e1 e2 -> EApp (e1, e2))
let ematch e cases = EMatch (e, cases)
let efun args rhs = List.fold_right args ~f:efun ~init:rhs
let eop o e1 e2 = EOp (o, e1, e2)
let elist = List.fold_right ~f:econs ~init:ENil
let ccase p e = p, e
let bbind isrec p e = isrec, p, e
let pwild _ = PWild
let pvar id = PVar id
let pconst c = PConst c
let ptuple l = PTuple l
let popcons = token "::" *> return (fun p1 p2 -> PCons (p1, p2))
let pcons = return @@ fun p1 p2 -> PCons (p1, p2)
let plist = List.fold_right ~f:(fun p1 p2 -> PCons (p1, p2)) ~init:PNil
let dlet isrec p e = DLet (isrec, p, e)

let chainl1 e op =
  let rec go acc = (fun f x -> f acc x) <$> op <*> e >>= go <|> return acc in
  e >>= go
;;

let chainl1' i e op =
  let rec go acc = (fun f x -> f acc x) <$> op <*> e >>= go <|> return acc in
  i >>= go
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let procr op pl pr =
  let p =
    fix @@ fun p -> pl >>= fun l -> op >>= (fun op -> p <|> pr >>| op l) <|> return l
  in
  p
;;

let procl op pl pr =
  let rec go acc =
    (fun f x -> f acc x) <$> op <*> choice [ pl >>= go; pr ] <|> return acc
  in
  pl >>= go
;;

let choice_op ops =
  choice @@ List.map ~f:(fun (tok, cons) -> token tok *> (return @@ eop cons)) ops
;;

let add_sub = choice_op [ "+", Add; "-", Sub ]
let mult_div = choice_op [ "*", Mul; "/", Div ]
let cmp = choice_op [ ">=", Geq; ">", Gre; "<=", Leq; "<", Less ]
let eq_uneq = choice_op [ "=", Eq; "<>", Neq ]
let conj = choice_op [ "&&", And ]
let disj = choice_op [ "||", Or ]
let cons = token "::" *> return econs
let refupd = token ":=" *> return (fun x y -> EApp (EApp (EVar ":=", x), y))

let apply_unary p =
  choice
    [ token "-" *> p >>| eunop Minus
    ; keyword "not" *> p >>| eunop Not
    ; token "+" *> p
    ; p
    ]
;;

let id valid_fst =
  let* fst = empty *> satisfy valid_fst in
  let take_func =
    match fst with
    | '_' -> many1
    | _ -> many
  in
  let* inner = take_func @@ satisfy is_valid_ident_symb in
  let id = Base.String.of_char_list @@ (fst :: inner) in
  if is_keyword id then fail "Keyword" else return id
;;

let ident =
  id
  @@ function
  | 'a' .. 'z' | '_' -> true
  | _ -> false
;;

let capitalized_ident =
  id
  @@ function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let uns = empty_lr @@ take_while1 is_digit

let cunsint =
  let* uns = uns in
  return @@ Base.Int.of_string uns >>| cint
;;

let cint =
  let* sgn = option "" (token "+" <|> token "-") in
  let* uns = uns in
  return @@ Base.Int.of_string (sgn ^ uns) >>| cint
;;

let cbool =
  let _true = keyword "true" *> return (cbool true) in
  let _false = keyword "false" *> return (cbool false) in
  _true <|> _false
;;

let cstring =
  between (char '"') (char '"')
  @@ take_while (function
         | '"' -> false
         | _ -> true)
  >>| cstring
;;

let const = empty_lr @@ choice [ cint; cbool; cstring ]
let uns_const = empty_lr @@ choice [ cunsint; cbool; cstring ]
let pvar = ident >>| pvar
let pwild = token "_" >>| pwild
let pconst = const >>| pconst

type pdispatch =
  { tuple: pdispatch -> pt t
  ; other: pdispatch -> pt t
  ; pt: pdispatch -> pt t
  }

let pack e =
  let pt d = fix @@ fun _self -> empty_lr @@ choice [ d.tuple d; d.other d ] in
  let tuple d =
    fix
    @@ fun _self ->
    empty_lr ((fun hd tl -> hd :: tl) <$> d.other d <*> many1 (comma *> d.other d))
    >>| ptuple
  in
  let other d =
    fix
    @@ fun _self ->
    let pnone = token "None" *> return PNone in
    let plabeled = char '~' *> ident >>= fun x -> return @@ PLabeled x in
    let popt =
      char '?'
      *> parens
           (let* id = ident in
            let* _ = token "=" in
            let* e = e in
            return @@ POptional (id, e))
    in
    let plist = empty_lr @@ between lsb rsb @@ sep_by semi @@ d.pt d >>| plist in
    let prim =
      empty_lr @@ choice [ pconst; pnone; plabeled; pvar; pwild; plist; parens @@ d.pt d ]
    in
    let prim_or_some = prim <|> (token "Some" *> prim >>= fun x -> return @@ PSome x) in
    popt <|> chainl1 prim_or_some popcons
  in
  { tuple; other; pt }
;;

let pt e = (pack e).pt (pack e)

type edispatch =
  { key: edispatch -> exp t
  ; tuple: edispatch -> exp t
  ; exp: edispatch -> exp t
  ; op: edispatch -> exp t
  }

let pack =
  let pt d = pt (d.exp d) in
  let exp d = fix @@ fun _self -> empty_lr @@ d.key d <|> d.tuple d <|> d.op d in
  let key d =
    fix
    @@ fun _self ->
    let eif =
      empty_lr
      @@ lift3
           eif
           (keyword "if" *> d.exp d)
           (keyword "then" *> d.exp d)
           (keyword "else" *> d.exp d)
    in
    let elet =
      let binding =
        empty_lr
          (bbind
          <$> keyword "let" *> option false (keyword "rec" >>| fun _ -> true)
          <*> pt d
          <*> (efun <$> (empty *> many (pt d) <* token "=") <*> (d.exp d <* keyword "in"))
          )
      in
      empty_lr (elet <$> many1 binding <*> d.exp d)
    in
    let efun = empty_lr (efun <$> (keyword "fun" *> many (pt d) <* arrow) <*> d.exp d) in
    let ematch =
      let fst_case = ccase <$> (option "" bar *> pt d <* arrow) <*> d.exp d in
      let other_cases = ccase <$> (bar *> pt d <* arrow) <*> d.exp d in
      let cases = (fun fst other -> fst :: other) <$> fst_case <*> many other_cases in
      let pmatch = ematch <$> (keyword "match" *> d.exp d <* keyword "with") <*> cases in
      let pfunction = keyword "function" *> cases >>| efunction in
      empty_lr @@ pfunction <|> pmatch
    in
    choice [ elet; eif; ematch; efun ]
  in
  let tuple d =
    ( @ )
    <$> many1 (d.op d <* comma)
    <*> (d.op d <|> d.key d >>| fun rhs -> [ rhs ])
    >>| etuple
  in
  let op d =
    fix
    @@ fun _self ->
    let lst = empty_lr @@ between lsb rsb @@ sep_by semi (d.exp d) in
    let none = token "None" *> return ENone in
    let prim =
      empty_lr
      @@ choice
           [ lst >>| elist
           ; none
           ; uns_const >>| econst
           ; ident >>| evar
           ; parens @@ d.exp d
           ]
    in
    let label_arg =
      empty_lr
      @@ let* id = char '~' *> ident <* char ':' in
         let* e = prim in
         return @@ Labeled (id, e)
    in
    let arg =
      label_arg <|> (prim >>= fun x -> return @@ Expr x) >>= fun a -> return @@ EArg a
    in
    let some = token "Some" *> prim >>= fun x -> return @@ ESome x in
    let app_op = empty_lr @@ chainl1' prim arg eapp in
    let app_or_some = app_op <|> some in
    let mul_op = procl mult_div app_or_some @@ d.key d in
    let add_op = procl add_sub (apply_unary mul_op) (apply_unary @@ d.key d) in
    let cons_op = procr cons add_op @@ d.key d in
    let cmp_op = procl cmp cons_op @@ d.key d in
    let eq_op = procl eq_uneq cmp_op @@ d.key d in
    let conj_op = procl conj eq_op @@ d.key d in
    let disj_op = procl disj conj_op @@ d.key d in
    empty_lr @@ procr refupd disj_op @@ d.key d
  in
  { key; tuple; exp; op }
;;

let exp = pack.exp pack

let decl =
  let dlet =
    lift3
      dlet
      (keyword "let" *> option false (keyword "rec" >>| fun _ -> true))
      (pt exp)
      (efun <$> (empty *> many (pt exp) <* token "=") <*> exp)
  in
  dlet
;;

let pprog (l : decl list) : prog = l
let prog = sep_by1 semisemi decl <* option "" @@ empty_lr semisemi >>| pprog
let f ~x = x
