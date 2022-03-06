  $ ./tests.exe <<-"EOF"
  > CREATE (pam :Person {name: "Pam", age: 40}),
  > (kate :Person  {name: "Kate", age: 20}),
  > (jessica:Person{name:"Jessica", age: 5}),
  > (gar:Person{name:"Garfield", age: 12});
  > MATCH (z) WHERE z.name STARTS WITH "Gar" OR z.name CONTAINS "ssi" OR z.name ENDS WITH "te" 
  > RETURN z, z.name CONTAINS "i"; 
  Was created 4 nodes
  -------------------------------
  z
  {
    "identity": 2,
    "labels": [
      "Person",
    ],
    "properties": {
      "age": 20
      "name": "Kate"
     }
  }
  
  {
    "identity": 3,
    "labels": [
      "Person",
    ],
    "properties": {
      "age": 5
      "name": "Jessica"
     }
  }
  
  {
    "identity": 4,
    "labels": [
      "Person",
    ],
    "properties": {
      "age": 12
      "name": "Garfield"
     }
  }
  
  -------------------------------
  z.name CONTAINS i
  false
  true
  true
  $ ./tests.exe <<-"EOF"
  > CREATE (pam :Person {name: "Pam", age: 40}),
  > (albina :Person  {name: "Albina", age: 17}),
  > (alisa :Person  {name: "Alisa", age: 14}),
  > (jessica:Person{name:"Jessica", age: 5}),
  > (gar:Person{name:"Garfield", age: 12});
  > MATCH (z) WHERE z.name CONTAINS "Al" AND z.name ENDS WITH "sa" OR z.name STARTS WITH "Gar" 
  > RETURN z, z.name CONTAINS "i"; 
  Was created 5 nodes
  -------------------------------
  z
  {
    "identity": 3,
    "labels": [
      "Person",
    ],
    "properties": {
      "age": 14
      "name": "Alisa"
     }
  }
  
  {
    "identity": 5,
    "labels": [
      "Person",
    ],
    "properties": {
      "age": 12
      "name": "Garfield"
     }
  }
  
  -------------------------------
  z.name CONTAINS i
  true
  true
  $ ./tests.exe <<-"EOF"
  > CREATE (pam :Person {name: "Pam", age: 40}),
  > (tom :Person  {name: "Tom", age: 15}), 
  > (kate :Person  {name: "Kate", age: 20}),
  > (jim :Person  {name: "Jim", age: 32}),
  > (ann :Person  {name: "Ann", age: 39});
  > MATCH (x) WHERE x.name < "Pal" RETURN x;
  > MATCH (y) WHERE y.age < 39 OR ((y.name = "Tom" OR y.name = "Kate") AND y.age >= 10+10) RETURN y;
  Was created 5 nodes
  -------------------------------
  x
  {
    "identity": 3,
    "labels": [
      "Person",
    ],
    "properties": {
      "age": 20
      "name": "Kate"
     }
  }
  
  {
    "identity": 4,
    "labels": [
      "Person",
    ],
    "properties": {
      "age": 32
      "name": "Jim"
     }
  }
  
  {
    "identity": 5,
    "labels": [
      "Person",
    ],
    "properties": {
      "age": 39
      "name": "Ann"
     }
  }
  
  -------------------------------
  y
  {
    "identity": 2,
    "labels": [
      "Person",
    ],
    "properties": {
      "age": 15
      "name": "Tom"
     }
  }
  
  {
    "identity": 3,
    "labels": [
      "Person",
    ],
    "properties": {
      "age": 20
      "name": "Kate"
     }
  }
  
  {
    "identity": 4,
    "labels": [
      "Person",
    ],
    "properties": {
      "age": 32
      "name": "Jim"
     }
  }
  
  $ ./tests.exe <<-"EOF"
  > CREATE (pam :Person {name: "Pam", age: 40}),
  > (tom :Person :Student {name: "Tom", age: 15}), 
  > (kate :Person  {name: "Kate", age: 20});
  > MATCH (n) WHERE n.age >= 20 RETURN n.name;
  > MATCH (n: Person) WHERE n.name = "Tom" OR n.name ="Kate" RETURN n;
  Was created 3 nodes
  -------------------------------
  n.name
  "Pam"
  "Kate"
  -------------------------------
  n
  {
    "identity": 2,
    "labels": [
      "Person",
      "Student",
    ],
    "properties": {
      "age": 15
      "name": "Tom"
     }
  }
  
  {
    "identity": 3,
    "labels": [
      "Person",
    ],
    "properties": {
      "age": 20
      "name": "Kate"
     }
  }
  
  $ ./tests.exe <<-"EOF"
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
  > MATCH (n) RETURN n, n.name, n.age, n.age > 20, n.name CONTAINS "m";
  Was created 4 nodes
  Was created 2 edges
  Was created 1 nodes
  Was created 1 edges
  Was created 1 edges
  -------------------------------
  r
  {
    "identity": 9,
    "start": 1,
    "end": 3,
    "type": "SISTER",
    "properties": {
      "role": "Elder sister"
     }
  }
  
  -------------------------------
  r
  {
    "identity": 8,
    "start": 7,
    "end": 2,
    "type": "PARENT",
    "properties": {
      "role": "Father"
     }
  }
  
  -------------------------------
  n
  {
    "identity": 1,
    "labels": [
      "Person",
    ],
    "properties": {
      "age": 40
      "name": "Pam"
     }
  }
  
  {
    "identity": 2,
    "labels": [
      "Person",
      "Student",
    ],
    "properties": {
      "age": 15
      "name": "Tom"
     }
  }
  
  {
    "identity": 5,
    "labels": [
      "Person",
    ],
    "properties": {
      "age": 5
      "name": "Jessica"
     }
  }
  
  -------------------------------
  n.name
  "Pam"
  "Tom"
  "Jessica"
  -------------------------------
  n.age
  40
  15
  5
  -------------------------------
  n.age>20
  true
  false
  false
  -------------------------------
  n.name CONTAINS m
  true
  true
  false
  $ ./tests.exe <<-"EOF"
  > CREATE (pam :Person {name: "Pam", age: 40}),
  > (tom :Person :Student {name: "Tom", age: 15}),
  > (ann :Person {name: "Ann", age: 25}),
  > (pam)-[:PARENT {role: "Mother"}]->(tom),
  > (ann)-[:PARENT {role: "Mother"}]->(jessica:Person{name:"Jessica", age: 5});
  > MATCH (tom) WHERE tom.age >14 AND tom.age <16
  > CREATE (bob:Person {name: "Bob", age: 38})-[:PARENT {role: "Father"}]->(tom);
  > MATCH (n) WHERE n.age = 38 DETACH DELETE n;
  > MATCH ()-[r]->() RETURN r, r.role, type (r);
  Was created 4 nodes
  Was created 2 edges
  Was created 1 nodes
  Was created 1 edges
  -------------------------------
  r
  {
    "identity": 6,
    "start": 3,
    "end": 5,
    "type": "PARENT",
    "properties": {
      "role": "Mother"
     }
  }
  
  {
    "identity": 4,
    "start": 1,
    "end": 2,
    "type": "PARENT",
    "properties": {
      "role": "Mother"
     }
  }
  
  -------------------------------
  r.role
  "Mother"
  "Mother"
  -------------------------------
  type(r)
  "PARENT"
  "PARENT"
  $ ./tests.exe <<-"EOF"
  > CREATE (:City{name:"Saint Petersburg"}),(:City{name:"Moscow"});
  > MATCH (c1:City{name:"Saint Petersburg"}), (c2:City{name:"Moscow"}) 
  > CREATE (u:User{name:"Vasya", phone:762042})-[:LIVES_IN]->(c1), (u)-[:BORN_IN]->(c2);
  > MATCH (n), ()-[r]->() RETURN n, n.name, r, type (r);
  Was created 2 nodes
  Was created 1 nodes
  Was created 2 edges
  -------------------------------
  n
  {
    "identity": 1,
    "labels": [
      "City",
    ],
    "properties": {
      "name": "Saint Petersburg"
     }
  }
  
  {
    "identity": 2,
    "labels": [
      "City",
    ],
    "properties": {
      "name": "Moscow"
     }
  }
  
  {
    "identity": 3,
    "labels": [
      "User",
    ],
    "properties": {
      "phone": 762042
      "name": "Vasya"
     }
  }
  
  -------------------------------
  n.name
  "Saint Petersburg"
  "Moscow"
  "Vasya"
  -------------------------------
  r
  {
    "identity": 5,
    "start": 3,
    "end": 2,
    "type": "BORN_IN",
    "properties": {
     }
  }
  
  {
    "identity": 4,
    "start": 3,
    "end": 1,
    "type": "LIVES_IN",
    "properties": {
     }
  }
  
  -------------------------------
  type(r)
  "BORN_IN"
  "LIVES_IN"
  $ ./tests.exe <<-"EOF"
  > create (a:A),(b:B),(c:C),(d:D),(e:E), (a)-[:ab]->(b),(c)-[:cb]->(b), (b)-[:bd]->(d), (b)-[:be]->(e);
  > match (n)-->(:B) return n;
  Was created 5 nodes
  Was created 4 edges
  -------------------------------
  n
  {
    "identity": 3,
    "labels": [
      "C",
    ],
    "properties": {
     }
  }
  
  {
    "identity": 1,
    "labels": [
      "A",
    ],
    "properties": {
     }
  }
  
