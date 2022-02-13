open Angstrom
open Ast

let parse_with p s = parse_string ~consume:Consume.All p s
let debug = false
let log s = if debug then Format.printf s else ()

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a
let pspace = take_while (fun ch -> ch = ' ' || ch = '\n')
let pspaces p = pspace *> p <* pspace
let pspaceschar chr = pspaces (char chr)
let pspacesstring str = pspaces (string_ci str)

let pstring =
  pspaceschar '"' *> take_till (fun ch -> ch = '"' || ch = ' ') <* pspaceschar '"'
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

let pcsring = pstring >>| fun s -> CString s
let pconst = choice [ pcsring; pcint ]
let peconst = pconst >>| fun c -> EConst c

let pexpr =
  fix (fun pexpr ->
      let term = char '(' *> pexpr <* char ')' <|> peconst in
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

let pwithoutproperties =
  pspaceschar '{'
  *> (sep_by (pspaceschar ',') pproperty
     >>| fun props ->
     match props with
     | [] -> None
     | _ -> Some props)
  <* pspaceschar '}'
;;

let pid =
  pspaces (take_while1 (fun ch -> (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')))
;;

let pids = sep_by (pspaceschar ':') pid

let pnode =
  pspaceschar '('
  *> lift3
       (fun var label props -> Nodedata (var, label, props))
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
    (fun var label props -> Edgedata (var, label, props))
    (option None (pid >>| fun id -> Some id))
    (pspaceschar ':' *> pid)
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
let pmatchret = pspacesstring "RETURN" *> pvars >>| fun vars -> CMatchRet vars
let pmatchwhere = pspacesstring "WHERE" *> pvars >>| fun vars -> CMatchWhere vars
let pcmd = choice [ pmatchret; pmatchwhere ]
let pcmdsmatch = lift2 (fun elm cmdmatch -> CmdMatch (elm, cmdmatch)) pelms (many pcmd)
let pmatch = pspacesstring "MATCH" *> pcmdsmatch
let pcmds = choice [ pcreate; pmatch ] <* pspaceschar ';'
let pcmdssep = many pcmds

let%expect_test _ =
  let _ =
    let parsed =
      parse_with
        pcmdssep
        {|  CREATE   (  pam  :  Person { name : " Pam " , age : 2+5*3 } )  
        ,  ( david : Person ) , (pam)-[:PARENT]->(david); 
        CREATE   (  pam  :  Person { name : " Pam " , age : 2+5*3 } ) ; |}
    in
    let open Caml.Format in
    match parsed with
    | Error err -> printf "%s%!" err
    | Ok commands -> printf "%a%!" pp_program commands
  in
  [%expect
    {|
    [(CmdCreate
        [(Node
            (Nodedata ((Some "pam"), (Some ["Person"]),
               (Some [("name", (EConst (CString "Pam")));
                       ("age",
                        (EBinop (Plus, (EConst (CInt 2)),
                           (EBinop (Star, (EConst (CInt 5)), (EConst (CInt 3))))
                           )))
                       ])
               )));
          (Node (Nodedata ((Some "david"), (Some ["Person"]), None)));
          (Edge ((Nodedata ((Some "pam"), None, None)),
             (Edgedata (None, "PARENT", None)),
             (Nodedata ((Some "david"), None, None))))
          ]);
      (CmdCreate
         [(Node
             (Nodedata ((Some "pam"), (Some ["Person"]),
                (Some [("name", (EConst (CString "Pam")));
                        ("age",
                         (EBinop (Plus, (EConst (CInt 2)),
                            (EBinop (Star, (EConst (CInt 5)), (EConst (CInt 3))))
                            )))
                        ])
                )))
           ])
      ]
     |}]
;;

let%expect_test _ =
  let _ =
    let parsed =
      parse_with
        pcmdssep
        {|
        CREATE (pam :Person :Student {name: "Pam"}),
                (tom :Person {name: "Tom"}),
                (kate :Person {name: "Kate"}),
                (pam)-[:PARENT {role: "Father"}]->(tom),
                (kate)-[:PARENT]->(jessica:Person{name:"Jessica"});
        CREATE (bob:Person {name: "Bob"})-[:PARENT {role: "Father"}]->(ann), (a)-[:hello]->(b);
        MATCH (n: Person);
        |}
    in
    let open Caml.Format in
    match parsed with
    | Error err -> printf "%s%!" err
    | Ok commands -> printf "%a%!" pp_program commands
  in
  [%expect
    {|
    [(CmdCreate
        [(Node
            (Nodedata ((Some "pam"), (Some ["Person"; "Student"]),
               (Some [("name", (EConst (CString "Pam")))]))));
          (Node
             (Nodedata ((Some "tom"), (Some ["Person"]),
                (Some [("name", (EConst (CString "Tom")))]))));
          (Node
             (Nodedata ((Some "kate"), (Some ["Person"]),
                (Some [("name", (EConst (CString "Kate")))]))));
          (Edge ((Nodedata ((Some "pam"), None, None)),
             (Edgedata (None, "PARENT",
                (Some [("role", (EConst (CString "Father")))]))),
             (Nodedata ((Some "tom"), None, None))));
          (Edge ((Nodedata ((Some "kate"), None, None)),
             (Edgedata (None, "PARENT", None)),
             (Nodedata ((Some "jessica"), (Some ["Person"]),
                (Some [("name", (EConst (CString "Jessica")))])))
             ))
          ]);
      (CmdCreate
         [(Edge (
             (Nodedata ((Some "bob"), (Some ["Person"]),
                (Some [("name", (EConst (CString "Bob")))]))),
             (Edgedata (None, "PARENT",
                (Some [("role", (EConst (CString "Father")))]))),
             (Nodedata ((Some "ann"), None, None))));
           (Edge ((Nodedata ((Some "a"), None, None)),
              (Edgedata (None, "hello", None)),
              (Nodedata ((Some "b"), None, None))))
           ]);
      (CmdMatch ([(Node (Nodedata ((Some "n"), (Some ["Person"]), None)))], []))]
     |}]
;;
