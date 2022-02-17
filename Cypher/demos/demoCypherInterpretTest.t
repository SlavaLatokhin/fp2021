  $ ./demoCypherInterpret.exe <<-"EOF"
  > CREATE (:City{name:"Saint Petersburg"}),(:City{name:"Moscow"});
  > MATCH (c1:City{name:"Saint Petersburg"}), (c2:City{name:"Moscow"}) 
  > CREATE (u:User{name:"Vasya", phone:762042})-[:LIVES_IN]->(c1), (u)-[:BORN_IN]->(c2);
  > MATCH (n), ()-[r]->() RETURN n, r;
  Vertex created
  Vertex created
  Vertex created
  Edge created
  Edge created
  Node: (3, ["User"], [("phone", (VInt 762042)); ("name", (VString "Vasya"))])
  ----------------------------------
  Node: (2, ["City"], [("name", (VString "Moscow"))])
  ----------------------------------
  Node: (1, ["City"], [("name", (VString "Saint Petersburg"))])
  ----------------------------------
  Edge: ((4, ["LIVES_IN"], []),
         ((Some (3, ["User"],
                 [("phone", (VInt 762042)); ("name", (VString "Vasya"))])),
          (Some (1, ["City"], [("name", (VString "Saint Petersburg"))]))))
  ----------------------------------
  Edge: ((5, ["BORN_IN"], []),
         ((Some (3, ["User"],
                 [("phone", (VInt 762042)); ("name", (VString "Vasya"))])),
          (Some (2, ["City"], [("name", (VString "Moscow"))]))))
  ----------------------------------
  $ ./demoCypherInterpret.exe <<-"EOF"
  > CREATE (pam :Person {name: "Pam", age: 40}),
  > (tom :Person :Student {name: "Tom", age: 15}), 
  > (kate :Person {name: "Kate", age: 40});
  > MATCH (n {age: 40}) RETURN n;
  > MATCH (n: Student) RETURN n;
  Vertex created
  Vertex created
  Vertex created
  Node: (3, ["Person"], [("age", (VInt 40)); ("name", (VString "Kate"))])
  ----------------------------------
  Node: (1, ["Person"], [("age", (VInt 40)); ("name", (VString "Pam"))])
  ----------------------------------
  Node: (2, ["Person"; "Student"],
         [("age", (VInt 15)); ("name", (VString "Tom"))])
  ----------------------------------
  $ ./demoCypherInterpret.exe <<-"EOF"
  > CREATE (pam :Person {name: "Pam", age: 40}),
  > (tom :Person :Student {name: "Tom", age: 15}),
  > (ann :Person {name: "Ann", age: 25}),
  > (pam)-[:PARENT {role: "Mother"}]->(tom),
  > (ann)-[:PARENT {role: "Mother"}]->(jessica:Person{name:"Jessica", age: 5});
  > MATCH (tom {name: "Tom", age: 15}) 
  > CREATE (bob:Person {name: "Bob", age: 38})-[:PARENT {role: "Father"}]->(tom);
  > MATCH (p1 {name: "Pam"}), (p2 {name: "Ann"}) CREATE (p1)-[:SISTER {role: "Elder sister"}]->(p2);
  > MATCH ()-[r:SISTER]->() RETURN r;
  > MATCH ()-[r {role: "Mother"}]->({name: "Tom"}) RETURN r;
  > MATCH (tom {name: "Tom", age: 15}) DETACH DELETE tom;
  > MATCH (n) RETURN n;
  Vertex created
  Vertex created
  Vertex created
  Edge created
  Vertex created
  Edge created
  Vertex created
  Edge created
  Edge created
  Edge: ((9, ["SISTER"], [("role", (VString "Elder sister"))]),
         ((Some (1, ["Person"], [("age", (VInt 40)); ("name", (VString "Pam"))])),
          (Some (3, ["Person"], [("age", (VInt 25)); ("name", (VString "Ann"))]))))
  ----------------------------------
  Edge: ((4, ["PARENT"], [("role", (VString "Mother"))]),
         ((Some (1, ["Person"], [("age", (VInt 40)); ("name", (VString "Pam"))])),
          (Some (2, ["Person"; "Student"],
                 [("age", (VInt 15)); ("name", (VString "Tom"))]))))
  ----------------------------------
  Node: (7, ["Person"], [("age", (VInt 38)); ("name", (VString "Bob"))])
  ----------------------------------
  Node: (5, ["Person"], [("age", (VInt 5)); ("name", (VString "Jessica"))])
  ----------------------------------
  Node: (3, ["Person"], [("age", (VInt 25)); ("name", (VString "Ann"))])
  ----------------------------------
  Node: (1, ["Person"], [("age", (VInt 40)); ("name", (VString "Pam"))])
  ----------------------------------
  $ ./demoCypherInterpret.exe <<-"EOF"
  > CREATE (pam :Person {name: "Pam", age: 40}),
  > (tom :Person :Student {name: "Tom", age: 15}),
  > (ann :Person {name: "Ann", age: 25}),
  > (pam)-[:PARENT {role: "Mother"}]->(tom),
  > (ann)-[:PARENT {role: "Mother"}]->(jessica:Person{name:"Jessica", age: 5});
  > MATCH (tom {name: "Tom", age: 15}) 
  > CREATE (bob:Person {name: "Bob", age: 38})-[:PARENT {role: "Father"}]->(tom);
  > MATCH ()-[r {role: "Mother"}]->({name: "Tom"}) RETURN r;
  > MATCH (:Person {name: "Bob", age: 38})-[r]->() DETACH DELETE r;
  > MATCH ()-[r]->() RETURN r;
  Vertex created
  Vertex created
  Vertex created
  Edge created
  Vertex created
  Edge created
  Vertex created
  Edge created
  Edge: ((4, ["PARENT"], [("role", (VString "Mother"))]),
         ((Some (1, ["Person"], [("age", (VInt 40)); ("name", (VString "Pam"))])),
          (Some (2, ["Person"; "Student"],
                 [("age", (VInt 15)); ("name", (VString "Tom"))]))))
  ----------------------------------
  Edge: ((4, ["PARENT"], [("role", (VString "Mother"))]),
         ((Some (1, ["Person"], [("age", (VInt 40)); ("name", (VString "Pam"))])),
          (Some (2, ["Person"; "Student"],
                 [("age", (VInt 15)); ("name", (VString "Tom"))]))))
  ----------------------------------
  Edge: ((6, ["PARENT"], [("role", (VString "Mother"))]),
         ((Some (3, ["Person"], [("age", (VInt 25)); ("name", (VString "Ann"))])),
          (Some (5, ["Person"],
                 [("age", (VInt 5)); ("name", (VString "Jessica"))]))))
  ----------------------------------
