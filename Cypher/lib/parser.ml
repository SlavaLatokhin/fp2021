open Angstrom
open Ast

let parse_with p s = parse_string ~consume:Consume.All p s
let debug = false
let log s = if debug then Format.printf s else ()

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

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

let pidwithoutnumbs =
  pspaces
    (take_while1 (fun ch ->
         (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch = '_'))
;;

let pcsring =
  pspaceschar '"' *> take_till (fun ch -> ch = '"')
  <* pspaceschar '"'
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

let pconst = choice [ pcsring; pcint ]
let peconst = pconst >>| fun c -> EConst c
let pgetelm = pidwithoutnumbs >>| fun var -> EGetElm var

let pgetprop =
  lift2
    (fun varelm varprop -> EGetProp (varelm, varprop))
    (pidwithoutnumbs <* pspaceschar '.')
    pidwithoutnumbs
;;

let pegetelmorprop = choice [ pgetprop; pgetelm ]

let pexpr =
  fix (fun pexpr ->
      let term =
        choice [ pspaceschar '(' *> pexpr <* pspaceschar ')'; peconst; pegetelmorprop ]
      in
      let term =
        chainl1
          term
          (choice
             [ char '*' *> return (fun e1 e2 -> EBinop (Star, e1, e2))
             ; char '/' *> return (fun e1 e2 -> EBinop (Slash, e1, e2))
             ])
      in
      let term =
        chainl1
          term
          (choice
             [ char '+' *> return (fun e1 e2 -> EBinop (Plus, e1, e2))
             ; char '-' *> return (fun e1 e2 -> EBinop (Minus, e1, e2))
             ])
      in
      let term =
        chainl1
          term
          (choice
             [ string "<>" *> return (fun e1 e2 -> EBinop (NotEqual, e1, e2))
             ; string "<=" *> return (fun e1 e2 -> EBinop (LessEq, e1, e2))
             ; string ">=" *> return (fun e1 e2 -> EBinop (GreEq, e1, e2))
             ; char '<' *> return (fun e1 e2 -> EBinop (Less, e1, e2))
             ; char '>' *> return (fun e1 e2 -> EBinop (Greater, e1, e2))
             ; char '=' *> return (fun e1 e2 -> EBinop (Equal, e1, e2))
             ])
      in
      let term =
        chainl1 term (string_ci "AND" *> return (fun e1 e2 -> EBinop (And, e1, e2)))
      in
      let term =
        chainl1 term (string_ci "OR" *> return (fun e1 e2 -> EBinop (Or, e1, e2)))
      in
      term)
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
    (fun var label props -> EdgeData (var, label, props))
    (option None (pid >>| fun id -> Some id))
    (option None (pspaceschar ':' *> pid >>| fun id -> Some id))
    (option None pproperties)
;;

let pedge =
  lift3
    (fun n1 e n2 -> Edge (n1, e, n2))
    pnode
    (pspaceschar '-' *> pspaceschar '[' *> pedgedata
    <* pspaceschar ']'
    <* pspaceschar '-'
    <* pspaceschar '>')
    pnode
;;

let pelm = choice [ pedge; (pnode >>| fun nodedata -> Node nodedata) ]
let pelms = sep_by (pspaceschar ',') pelm
let pcreate = pspacesstring "CREATE" *> pelms >>| fun cmd -> CmdCreate cmd
let pvar = pid
let pvars = sep_by (pspaceschar ',') pvar
let pexprs = sep_by (pspaceschar ',') pexpr
let pmatchret = pspacesstring "RETURN" *> pexprs >>| fun exprs -> CMatchRet exprs
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

(* let%expect_test _ =
  let _ =
    let parsed =
      parse_with
        pcmdssep
        {|
        CREATE (:City{name:"Saint Petersburg", age: 20    });
        CREATE (:City{name:"Moscow"});
        MATCH (c {name: "Moscow"}) DETACH DELETE c;
        MATCH (c) WHERE n.name   <> "Tom"  RETURN a, a.age >10 AND a.age <20 ;
        MATCH ()-[r: SISTER]->() WHERE r.role = "Elder sister" RETURN r;
        |}
    in
    let open Caml.Format in
    match parsed with
    | Error err -> printf "%s%!" err
    | Ok commands -> printf "%a%!" pp_program commands
  in
  [%expect {|
    
     |}]
;; *)
