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
let pstring = pspaceschar '"' *> take_till (fun ch -> ch = '"') <* pspaceschar '"'

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

let pid =
  pspaces
    (take_while1 (fun ch ->
         (ch >= '0' && ch <= '9')
         || (ch >= 'a' && ch <= 'z')
         || (ch >= 'A' && ch <= 'Z')
         || ch = '_'))
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
let pmatchret = pspacesstring "RETURN" *> pvars >>| fun vars -> CMatchRet vars
let pmatchcreate = pspacesstring "CREATE" *> pelms >>| fun cmd -> CMatchCrt cmd

let pmatchdelete =
  pspacesstring "DETACH" *> pspacesstring "DELETE" *> pvars
  >>| fun vars -> CMatchDelete vars
;;

let pcmd = choice [ pmatchret; pmatchcreate; pmatchdelete ]
let pcmdsmatch = lift2 (fun elm cmdmatch -> CmdMatch (elm, cmdmatch)) pelms (many1 pcmd)
let pmatch = pspacesstring "MATCH" *> pcmdsmatch
let pcmds = choice [ pcreate; pmatch ] <* pspaceschar ';'
let pcmdssep = many pcmds

let%expect_test _ =
  let _ =
    let parsed =
      parse_with
        pcmdssep
        {|  
          CREATE (:City{name:"Saint Petersburg"}),(:City{name:"Moscow"});

          MATCH (c1:City{name:"Saint Petersburg"}), (c2:City{name:"Moscow"}) 
          CREATE (u:User{name:"Vasya", phone:762042})-[:LIVES_IN]->(c1), (u)-[:BORN_IN]->(c2);

          MATCH (n), ()-[r]->() RETURN n, r;
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
            (NodeData (None, (Some ["City"]),
               (Some [("name", (EConst (CString "Saint Petersburg")))]))));
          (Node
             (NodeData (None, (Some ["City"]),
                (Some [("name", (EConst (CString "Moscow")))]))))
          ]);
      (CmdMatch (
         [(Node
             (NodeData ((Some "c1"), (Some ["City"]),
                (Some [("name", (EConst (CString "Saint Petersburg")))]))));
           (Node
              (NodeData ((Some "c2"), (Some ["City"]),
                 (Some [("name", (EConst (CString "Moscow")))]))))
           ],
         [(CMatchCrt
             [(Edge (
                 (NodeData ((Some "u"), (Some ["User"]),
                    (Some [("name", (EConst (CString "Vasya")));
                            ("phone", (EConst (CInt 762042)))])
                    )),
                 (EdgeData (None, (Some "LIVES_IN"), None)),
                 (NodeData ((Some "c1"), None, None))));
               (Edge ((NodeData ((Some "u"), None, None)),
                  (EdgeData (None, (Some "BORN_IN"), None)),
                  (NodeData ((Some "c2"), None, None))))
               ])
           ]
         ));
      (CmdMatch (
         [(Node (NodeData ((Some "n"), None, None)));
           (Edge ((NodeData (None, None, None)),
              (EdgeData ((Some "r"), None, None)), (NodeData (None, None, None))
              ))
           ],
         [(CMatchRet ["n"; "r"])]))
      ]
          |}]
;;

let%expect_test _ =
  let _ =
    let parsed =
      parse_with
        pcmdssep
        {|  
        CREATE (pam :Person {name: "Pam", age: 40}),
                (tom :Person :Student {name: "Tom", age: 15}),
                (kate :Person {name: "Kate", age: 40});

        MATCH (n {age: 40}) RETURN n;

        MATCH (n: Student) RETURN n;
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
            (NodeData ((Some "pam"), (Some ["Person"]),
               (Some [("name", (EConst (CString "Pam")));
                       ("age", (EConst (CInt 40)))])
               )));
          (Node
             (NodeData ((Some "tom"), (Some ["Person"; "Student"]),
                (Some [("name", (EConst (CString "Tom")));
                        ("age", (EConst (CInt 15)))])
                )));
          (Node
             (NodeData ((Some "kate"), (Some ["Person"]),
                (Some [("name", (EConst (CString "Kate")));
                        ("age", (EConst (CInt 40)))])
                )))
          ]);
      (CmdMatch (
         [(Node
             (NodeData ((Some "n"), None, (Some [("age", (EConst (CInt 40)))]))))
           ],
         [(CMatchRet ["n"])]));
      (CmdMatch ([(Node (NodeData ((Some "n"), (Some ["Student"]), None)))],
         [(CMatchRet ["n"])]))
      ]
          |}]
;;

let%expect_test _ =
  let _ =
    let parsed =
      parse_with
        pcmdssep
        {|  
        CREATE (pam :Person {name: "Pam", age: 40}),
                (tom :Person :Student {name: "Tom", age: 15}),
                (ann :Person {name: "Ann", age: 25}),
                (pam)-[:PARENT {role: "Mother"}]->(tom),
                (ann)-[:PARENT {role: "Mother"}]->(jessica:Person{name:"Jessica", age: 5});

        MATCH (tom {name: "Tom", age: 15}) 
        CREATE (bob:Person {name: "Bob", age: 38})-[:PARENT {role: "Father"}]->(tom);

        MATCH (p1 {name: "Pam"}), (p2 {name: "Ann"}) CREATE (p1)-[:SISTER {role: "Elder sister"}]->(p2);

        MATCH ()-[r:SISTER]->() RETURN r;

        MATCH (tom {name: "Tom", age: 15}) DETACH DELETE tom;

        MATCH (n) RETURN n;
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
            (NodeData ((Some "pam"), (Some ["Person"]),
               (Some [("name", (EConst (CString "Pam")));
                       ("age", (EConst (CInt 40)))])
               )));
          (Node
             (NodeData ((Some "tom"), (Some ["Person"; "Student"]),
                (Some [("name", (EConst (CString "Tom")));
                        ("age", (EConst (CInt 15)))])
                )));
          (Node
             (NodeData ((Some "ann"), (Some ["Person"]),
                (Some [("name", (EConst (CString "Ann")));
                        ("age", (EConst (CInt 25)))])
                )));
          (Edge ((NodeData ((Some "pam"), None, None)),
             (EdgeData (None, (Some "PARENT"),
                (Some [("role", (EConst (CString "Mother")))]))),
             (NodeData ((Some "tom"), None, None))));
          (Edge ((NodeData ((Some "ann"), None, None)),
             (EdgeData (None, (Some "PARENT"),
                (Some [("role", (EConst (CString "Mother")))]))),
             (NodeData ((Some "jessica"), (Some ["Person"]),
                (Some [("name", (EConst (CString "Jessica")));
                        ("age", (EConst (CInt 5)))])
                ))
             ))
          ]);
      (CmdMatch (
         [(Node
             (NodeData ((Some "tom"), None,
                (Some [("name", (EConst (CString "Tom")));
                        ("age", (EConst (CInt 15)))])
                )))
           ],
         [(CMatchCrt
             [(Edge (
                 (NodeData ((Some "bob"), (Some ["Person"]),
                    (Some [("name", (EConst (CString "Bob")));
                            ("age", (EConst (CInt 38)))])
                    )),
                 (EdgeData (None, (Some "PARENT"),
                    (Some [("role", (EConst (CString "Father")))]))),
                 (NodeData ((Some "tom"), None, None))))
               ])
           ]
         ));
      (CmdMatch (
         [(Node
             (NodeData ((Some "p1"), None,
                (Some [("name", (EConst (CString "Pam")))]))));
           (Node
              (NodeData ((Some "p2"), None,
                 (Some [("name", (EConst (CString "Ann")))]))))
           ],
         [(CMatchCrt
             [(Edge ((NodeData ((Some "p1"), None, None)),
                 (EdgeData (None, (Some "SISTER"),
                    (Some [("role", (EConst (CString "Elder sister")))]))),
                 (NodeData ((Some "p2"), None, None))))
               ])
           ]
         ));
      (CmdMatch (
         [(Edge ((NodeData (None, None, None)),
             (EdgeData ((Some "r"), (Some "SISTER"), None)),
             (NodeData (None, None, None))))
           ],
         [(CMatchRet ["r"])]));
      (CmdMatch (
         [(Node
             (NodeData ((Some "tom"), None,
                (Some [("name", (EConst (CString "Tom")));
                        ("age", (EConst (CInt 15)))])
                )))
           ],
         [(CMatchDelete ["tom"])]));
      (CmdMatch ([(Node (NodeData ((Some "n"), None, None)))],
         [(CMatchRet ["n"])]))
      ]
          |}]
;;
