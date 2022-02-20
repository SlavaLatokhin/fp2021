  $ ./demoCypherInterpret.exe <<-"EOF"
  > CREATE (pam :Person {name: "Pam", age: 40}),
  > (tom :Person  {name: "Tom", age: 15}), 
  > (kate :Person  {name: "Kate", age: 20}),
  > (jim :Person  {name: "Jim", age: 32}),
  > (ann :Person  {name: "Ann", age: 39});
  > MATCH (n) WHERE n.name < "Pal" RETURN n;
  > MATCH (n) WHERE n.age < 39 OR ((n.name = "Tom" OR n.name = "Kate") AND n.age >= 10+10) RETURN n;
  Vertex created
  Vertex created
  Vertex created
  Vertex created
  Vertex created
  Node: (3, ["Person"], [("age", (VInt 20)); ("name", (VString "Kate"))])
  ----------------------------------
  Node: (4, ["Person"], [("age", (VInt 32)); ("name", (VString "Jim"))])
  ----------------------------------
  Node: (5, ["Person"], [("age", (VInt 39)); ("name", (VString "Ann"))])
  ----------------------------------
  Node: (2, ["Person"], [("age", (VInt 15)); ("name", (VString "Tom"))])
  ----------------------------------
  Node: (3, ["Person"], [("age", (VInt 20)); ("name", (VString "Kate"))])
  ----------------------------------
  Node: (4, ["Person"], [("age", (VInt 32)); ("name", (VString "Jim"))])
  ----------------------------------
  $ ./demoCypherInterpret.exe <<-"EOF"
  > CREATE (pam :Person {name: "Pam", age: 40}),
  > (tom :Person :Student {name: "Tom", age: 15}), 
  > (kate :Person  {name: "Kate", age: 20});
  > MATCH (n) WHERE n.age >= 20 RETURN n.name;
  > MATCH (n: Person) WHERE n.name = "Tom" OR n.name ="Kate" RETURN n;
  Vertex created
  Vertex created
  Vertex created
  (VString "Pam")
  (VString "Kate")
  Node: (2, ["Person"; "Student"],
         [("age", (VInt 15)); ("name", (VString "Tom"))])
  ----------------------------------
  Node: (3, ["Person"], [("age", (VInt 20)); ("name", (VString "Kate"))])
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
  > MATCH ()-[r ]->() WHERE r.role = "Elder sister" RETURN r;
  > MATCH ()-[r:PARENT]->({name: "Tom"}) WHERE r.role = "Father" RETURN r;
  > MATCH (n) WHERE n.age <39 AND n.age > 24 DETACH DELETE n;
  > MATCH (n) RETURN n.name, n.age, n.age > 20;
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
  Edge: ((8, ["PARENT"], [("role", (VString "Father"))]),
         ((Some (7, ["Person"], [("age", (VInt 38)); ("name", (VString "Bob"))])),
          (Some (2, ["Person"; "Student"],
                 [("age", (VInt 15)); ("name", (VString "Tom"))]))))
  ----------------------------------
  (VString "Pam")
  (VString "Tom")
  (VString "Jessica")
  (VInt 40)
  (VInt 15)
  (VInt 5)
  (VBool true)
  (VBool false)
  (VBool false)
  $ ./demoCypherInterpret.exe <<-"EOF"
  > CREATE (pam :Person {name: "Pam", age: 40}),
  > (tom :Person :Student {name: "Tom", age: 15}),
  > (ann :Person {name: "Ann", age: 25}),
  > (pam)-[:PARENT {role: "Mother"}]->(tom),
  > (ann)-[:PARENT {role: "Mother"}]->(jessica:Person{name:"Jessica", age: 5});
  > MATCH (tom) WHERE tom.age >14 AND tom.age <16
  > CREATE (bob:Person {name: "Bob", age: 38})-[:PARENT {role: "Father"}]->(tom);
  > MATCH (n) WHERE n.age = 38 DETACH DELETE n;
  > MATCH ()-[r]->() RETURN r, r.role;
  Vertex created
  Vertex created
  Vertex created
  Edge created
  Vertex created
  Edge created
  Vertex created
  Edge created
  Edge: ((6, ["PARENT"], [("role", (VString "Mother"))]),
         ((Some (3, ["Person"], [("age", (VInt 25)); ("name", (VString "Ann"))])),
          (Some (5, ["Person"],
                 [("age", (VInt 5)); ("name", (VString "Jessica"))]))))
  ----------------------------------
  Edge: ((4, ["PARENT"], [("role", (VString "Mother"))]),
         ((Some (1, ["Person"], [("age", (VInt 40)); ("name", (VString "Pam"))])),
          (Some (2, ["Person"; "Student"],
                 [("age", (VInt 15)); ("name", (VString "Tom"))]))))
  ----------------------------------
  (VString "Mother")
  (VString "Mother")
  $ ./demoCypherInterpret.exe <<-"EOF"
  > CREATE (:City{name:"Saint Petersburg"}),(:City{name:"Moscow"});
  > MATCH (c1:City{name:"Saint Petersburg"}), (c2:City{name:"Moscow"}) 
  > CREATE (u:User{name:"Vasya", phone:762042})-[:LIVES_IN]->(c1), (u)-[:BORN_IN]->(c2);
  > MATCH (n), ()-[r]->() RETURN n, n.name, r;
  Vertex created
  Vertex created
  Vertex created
  Edge created
  Edge created
  Node: (1, ["City"], [("name", (VString "Saint Petersburg"))])
  ----------------------------------
  Node: (2, ["City"], [("name", (VString "Moscow"))])
  ----------------------------------
  Node: (3, ["User"], [("phone", (VInt 762042)); ("name", (VString "Vasya"))])
  ----------------------------------
  (VString "Saint Petersburg")
  (VString "Moscow")
  (VString "Vasya")
  Edge: ((5, ["BORN_IN"], []),
         ((Some (3, ["User"],
                 [("phone", (VInt 762042)); ("name", (VString "Vasya"))])),
          (Some (2, ["City"], [("name", (VString "Moscow"))]))))
  ----------------------------------
  Edge: ((4, ["LIVES_IN"], []),
         ((Some (3, ["User"],
                 [("phone", (VInt 762042)); ("name", (VString "Vasya"))])),
          (Some (1, ["City"], [("name", (VString "Saint Petersburg"))]))))
  ----------------------------------
