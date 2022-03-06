  $ ./demoCypherParse.exe <<-"EOF"
  > CREATE (:City{name:"Saint Petersburg"}),(:City{name:"Moscow"});
  > MATCH (c1:City{name:"Saint Petersburg"}), (c2:City{name:"Moscow"}) 
  > CREATE (u:User{name:"Vasya", phone:762042})-[:LIVES_IN]->(c1), (u)-[:BORN_IN]->(c2);
  > MATCH (n), ()-[r]-() RETURN n, r;
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
       None,
       [(CMatchCrt
           [(Edge (
               (NodeData ((Some "u"), (Some ["User"]),
                  (Some [("name", (EConst (CString "Vasya")));
                          ("phone", (EConst (CInt 762042)))])
                  )),
               (EdgeData (Direct (None, (Some "LIVES_IN"), None))),
               (NodeData ((Some "c1"), None, None))));
             (Edge ((NodeData ((Some "u"), None, None)),
                (EdgeData (Direct (None, (Some "BORN_IN"), None))),
                (NodeData ((Some "c2"), None, None))))
             ])
         ]
       ));
    (CmdMatch (
       [(Node (NodeData ((Some "n"), None, None)));
         (Edge ((NodeData (None, None, None)),
            (EdgeData (UnDirect ((Some "r"), None, None))),
            (NodeData (None, None, None))))
         ],
       None, [(CMatchRet ([(EGetElm "n"); (EGetElm "r")], None))]))
    ]
  $ ./demoCypherParse.exe <<-"EOF"
  > CREATE (pam :Person {name: "Pam", age: 40}),
  > (tom :Person :Student {name: "Tom", age: 15}), 
  > (kate :Person {name: "Kate", age: 40});
  > MATCH (n {age: 40}) RETURN n;
  > MATCH (n: Student) RETURN n;
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
       None, [(CMatchRet ([(EGetElm "n")], None))]));
    (CmdMatch ([(Node (NodeData ((Some "n"), (Some ["Student"]), None)))],
       None, [(CMatchRet ([(EGetElm "n")], None))]))
    ]
  $ ./demoCypherParse.exe <<-"EOF"
  > CREATE (pam :Person {name: "Pam", age: 40}),
  > (tom :Person :Student {name: "Tom", age: 15}),
  > (ann :Person {name: "Ann", age: 25}),
  > (pam)-[:PARENT {role: "Mother"}]->(tom),
  > (ann)-[:PARENT {role: "Mother"}]->(jessica:Person{name:"Jessica", age: 5});
  > MATCH (tom {name: "Tom", age: 15}) 
  > CREATE (bob:Person {name: "Bob", age: 38})-[:PARENT {role: "Father"}]->(tom);
  > MATCH (p1 {name: "Pam"}), (p2 {name: "Ann"}) CREATE (p1)-[:SISTER {role: "Elder sister"}]->(p2);
  > MATCH ()-[r:SISTER]->() RETURN r;
  > MATCH (tom {name: "Tom", age: 15}) DETACH DELETE tom;
  > MATCH (n) RETURN n;
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
           (EdgeData
              (Direct (None, (Some "PARENT"),
                 (Some [("role", (EConst (CString "Mother")))])))),
           (NodeData ((Some "tom"), None, None))));
        (Edge ((NodeData ((Some "ann"), None, None)),
           (EdgeData
              (Direct (None, (Some "PARENT"),
                 (Some [("role", (EConst (CString "Mother")))])))),
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
       None,
       [(CMatchCrt
           [(Edge (
               (NodeData ((Some "bob"), (Some ["Person"]),
                  (Some [("name", (EConst (CString "Bob")));
                          ("age", (EConst (CInt 38)))])
                  )),
               (EdgeData
                  (Direct (None, (Some "PARENT"),
                     (Some [("role", (EConst (CString "Father")))])))),
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
       None,
       [(CMatchCrt
           [(Edge ((NodeData ((Some "p1"), None, None)),
               (EdgeData
                  (Direct (None, (Some "SISTER"),
                     (Some [("role", (EConst (CString "Elder sister")))])))),
               (NodeData ((Some "p2"), None, None))))
             ])
         ]
       ));
    (CmdMatch (
       [(Edge ((NodeData (None, None, None)),
           (EdgeData (Direct ((Some "r"), (Some "SISTER"), None))),
           (NodeData (None, None, None))))
         ],
       None, [(CMatchRet ([(EGetElm "r")], None))]));
    (CmdMatch (
       [(Node
           (NodeData ((Some "tom"), None,
              (Some [("name", (EConst (CString "Tom")));
                      ("age", (EConst (CInt 15)))])
              )))
         ],
       None, [(CMatchDelete ["tom"])]));
    (CmdMatch ([(Node (NodeData ((Some "n"), None, None)))], None,
       [(CMatchRet ([(EGetElm "n")], None))]))
    ]
