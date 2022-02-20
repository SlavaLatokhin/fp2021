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
  -------------------------------
  n
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
  n
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
  
  $ ./demoCypherInterpret.exe <<-"EOF"
  > CREATE (pam :Person {name: "Pam", age: 40}),
  > (tom :Person :Student {name: "Tom", age: 15}), 
  > (kate :Person  {name: "Kate", age: 20});
  > MATCH (n) WHERE n.age >= 20 RETURN n.name;
  > MATCH (n: Person) WHERE n.name = "Tom" OR n.name ="Kate" RETURN n;
  Vertex created
  Vertex created
  Vertex created
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
  > MATCH (n) RETURN n, n.name, n.age, n.age > 20;
  Vertex created
  Vertex created
  Vertex created
  Edge created
  Vertex created
  Edge created
  Vertex created
  Edge created
  Edge created
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
  
