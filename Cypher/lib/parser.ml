open Angstrom
open Ast

let ( <~> ) x xs = x >>= fun r -> xs >>= fun rs -> return (r :: rs)
let parse_with p s = parse_string ~consume:Consume.All p s
let debug = false
let log s = if debug then Format.printf s else ()

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let pspace1 = take_while1 (fun ch -> ch = ' ' || ch = '\n')
let pspace = take_while (fun ch -> ch = ' ' || ch = '\n')
let pspaces p = pspace *> p <* pspace
let pspaceschar chr = pspaces (char chr)
let pspacesstring str = pspaces (string_ci str)

let pid =
  pspaces
    (take_while1 (fun ch ->
         (ch >= '0' && ch <= '9')
         || (ch >= 'a' && ch <= 'z')
         || (ch >= 'A' && ch <= 'Z')
         || ch = '_'))
;;

let labelsedge = pspaceschar '`' *> take_till (fun ch -> ch = '`') <* pspaceschar '`'

let pcsring =
  pspaceschar '"' *> take_till (fun ch -> ch = '"')
  <* pspaceschar '"'
  >>| fun c -> CString c
;;

let pcsringsnglq =
  pspaceschar '\'' *> take_till (fun ch -> ch = '\'')
  <* pspaceschar '\''
  >>| fun c -> CString c
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let psign =
  choice
    [ pspaceschar '+' *> return 1; pspaceschar '-' *> return (-1); pspace *> return 1 ]
;;

let pcint =
  pspaces
    (lift2
       (fun sign v -> CInt (sign * v))
       psign
       (take_while is_digit
       >>= fun s ->
       match int_of_string_opt s with
       | Some x -> return x
       | None -> fail "incorrect int"))
;;

let pconst = choice [ pcsring; pcsringsnglq; pcint ]
let peconst = pconst >>| fun c -> EConst c
let pgetelm = pid >>| fun var -> EGetElm var

let pgetprop =
  lift2 (fun varelm varprop -> EGetProp (varelm, varprop)) (pid <* pspaceschar '.') pid
;;

let pconstcond = choice [ pcsring; pcsringsnglq ]
let peconstcond = pconstcond >>| fun c -> EConst c

let pstrtwith =
  lift2
    (fun prop str -> EBinop (StartWith, prop, str))
    (pgetprop <* pspacesstring "STARTS WITH")
    peconstcond
;;

let pendwith =
  lift2
    (fun prop str -> EBinop (EndWith, prop, str))
    (pgetprop <* pspacesstring "ENDS WITH")
    peconstcond
;;

let pcontains =
  lift2
    (fun prop str -> EBinop (Contain, prop, str))
    (pgetprop <* pspacesstring "CONTAINS")
    peconst
;;

let pnotnull = pspacesstring "NOT" *> pspacesstring "NULL" >>| fun _ -> IsNotNull
let pnull = pspacesstring "NULL" >>| fun _ -> IsNull
let pegetelmorprop = choice [ pgetprop; pgetelm ]
let pecondforstr = choice [ pstrtwith; pendwith; pcontains ]

let pecondnull =
  pgetprop
  >>= fun prop ->
  pspacesstring "IS" *> choice [ pnotnull; pnull ]
  >>= fun condnull -> return (EUnop (condnull, prop))
;;

let pgettype = pspacesstring "type" *> pspaceschar '(' *> pid <* pspaceschar ')'
let pegettype = pgettype >>| fun var -> EGetType var
let pgetid = pspacesstring "id" *> pspaceschar '(' *> pid <* pspaceschar ')'
let pegetid = pgetid >>| fun var -> EGetId var

let pexpr =
  fix (fun pexpr ->
      let penot = pspacesstring "NOT" *> pexpr >>| fun e -> EUnop (Not, e) in
      let firstlvl =
        choice
          [ pegetid
          ; pspaceschar '(' *> pexpr <* pspaceschar ')'
          ; pecondforstr
          ; pecondnull
          ; penot
          ; pegettype
          ; peconst
          ; pegetelmorprop
          ]
      in
      let mulslash =
        chainl1
          firstlvl
          (choice
             [ char '*' *> return (fun e1 e2 -> EBinop (Star, e1, e2))
             ; char '/' *> return (fun e1 e2 -> EBinop (Slash, e1, e2))
             ])
      in
      let addmin =
        chainl1
          mulslash
          (choice
             [ char '+' *> return (fun e1 e2 -> EBinop (Plus, e1, e2))
             ; char '-' *> return (fun e1 e2 -> EBinop (Minus, e1, e2))
             ])
      in
      let commp =
        chainl1
          addmin
          (choice
             [ string "<>" *> return (fun e1 e2 -> EBinop (NotEqual, e1, e2))
             ; string "<=" *> return (fun e1 e2 -> EBinop (LessEq, e1, e2))
             ; string ">=" *> return (fun e1 e2 -> EBinop (GreEq, e1, e2))
             ; char '<' *> return (fun e1 e2 -> EBinop (Less, e1, e2))
             ; char '>' *> return (fun e1 e2 -> EBinop (Greater, e1, e2))
             ; char '=' *> return (fun e1 e2 -> EBinop (Equal, e1, e2))
             ])
      in
      let logic =
        chainl1 commp (string_ci "AND " *> return (fun e1 e2 -> EBinop (And, e1, e2)))
      in
      let logic =
        chainl1 logic (string_ci "OR " *> return (fun e1 e2 -> EBinop (Or, e1, e2)))
      in
      let logic =
        chainl1 logic (string_ci "XOR " *> return (fun e1 e2 -> EBinop (Xor, e1, e2)))
      in
      logic)
;;

let pproperty =
  lift2
    (fun k v -> k, v)
    (take_till (fun ch -> ch = ':' || ch = ' ') <* pspaceschar ':')
    pexpr
;;

let pproperties =
  pspaceschar '{'
  *> (sep_by (pspaceschar ',') pproperty
     >>| fun props ->
     match props with
     | [] -> None
     | _ -> Some props)
  <* pspaceschar '}'
;;

let pids = sep_by (pspaceschar ':') pid

let pnode =
  pspaceschar '('
  *> lift3
       (fun var label props -> NodeData (var, label, props))
       (option None (pid >>| fun id -> Some id))
       (option
          None
          (pspaceschar ':'
          *> (pids
             >>| fun ids ->
             match ids with
             | [] -> None
             | _ -> Some ids)))
       (option None pproperties)
  <* pspaceschar ')'
;;

let pedgedata =
  lift3
    (fun var label props -> var, label, props)
    (option None (pid >>| fun id -> Some id))
    (option None (pspaceschar ':' *> (labelsedge <|> pid >>| fun id -> Some id)))
    (option None pproperties)
;;

let pedgewithdata =
  pspaceschar '[' *> pedgedata
  >>= fun datas ->
  match datas with
  | var, label, props ->
    pspaceschar ']'
    *> pspaceschar '-'
    *> option
         (EdgeData (UnDirect (var, label, props)))
         (pspaceschar '>' >>| fun _ -> EdgeData (Direct (var, label, props)))
;;

let pedgewithoutdata =
  pspaceschar '-'
  *> option
       (EdgeData (UnDirect (None, None, None)))
       (pspaceschar '>' >>| fun _ -> EdgeData (Direct (None, None, None)))
;;

let pedgetype = pspaceschar '-' *> choice [ pedgewithoutdata; pedgewithdata ]
let pedge = lift3 (fun n1 e n2 -> Edge (n1, e, n2)) pnode pedgetype pnode
let pelm = choice [ pedge; (pnode >>| fun nodedata -> Node nodedata) ]
let pelms = sep_by (pspaceschar ',') pelm
let pcreate = pspacesstring "CREATE" *> pelms >>| fun cmd -> CmdCreate cmd
let pvar = pid
let pvars = sep_by (pspaceschar ',') pvar
let pexprs = sep_by (pspaceschar ',') pexpr

let porder =
  pspacesstring "ORDER" *> pspacesstring "BY" *> choice [ pegetid; pegetelmorprop ]
  >>= fun cmd ->
  option
    (Order (cmd, None))
    (choice
       [ pspacesstring "ASC" *> return (Order (cmd, Some Asc))
       ; pspacesstring "DESC" *> return (Order (cmd, Some Desc))
       ])
;;

let pcmdret =
  lift2
    (fun exprs order -> CMatchRet (exprs, order))
    pexprs
    (option None (pspace *> porder >>| fun cmd -> Some cmd))
;;

let pmatchret = pspacesstring "RETURN" *> pcmdret
let pmatchcreate = pspacesstring "CREATE" *> pelms >>| fun cmd -> CMatchCrt cmd

let pmatchdelete =
  pspacesstring "DETACH" *> pspacesstring "DELETE" *> pvars
  >>| fun vars -> CMatchDelete vars
;;

let pcmd = choice [ pmatchret; pmatchcreate; pmatchdelete ]
let pwhere = pspacesstring "WHERE" *> pexpr >>| fun expr -> CMatchWhere expr

let pcmdsmatch =
  lift3
    (fun elm cmdwhere cmdmatch -> CmdMatch (elm, cmdwhere, cmdmatch))
    pelms
    (option None (pwhere >>| fun cmd -> Some cmd))
    (many1 pcmd)
;;

let pmatch = pspacesstring "MATCH" *> pcmdsmatch
let pcmds = choice [ pcreate; pmatch ] <* pspaceschar ';'
let pcmdssep = many pcmds
