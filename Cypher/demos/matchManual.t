2.1. Get all nodes
  $ ./matchManual.exe <<-"EOF"
  > MATCH (n)
  > RETURN n;
  Was created 7 nodes
  Was created 7 edges
  -------------------------------
  n
  {
    "identity": 1,
    "labels": [
      "Person",
    ],
    "properties": {
      "name": "Oliver Stone"
     }
  }
  
  {
    "identity": 2,
    "labels": [
      "Person",
    ],
    "properties": {
      "name": "Michael Douglas"
     }
  }
  
  {
    "identity": 3,
    "labels": [
      "Person",
    ],
    "properties": {
      "name": "Charlie Sheen"
     }
  }
  
  {
    "identity": 4,
    "labels": [
      "Person",
    ],
    "properties": {
      "name": "Martin Sheen"
     }
  }
  
  {
    "identity": 5,
    "labels": [
      "Person",
    ],
    "properties": {
      "name": "Rob Reiner"
     }
  }
  
  {
    "identity": 6,
    "labels": [
      "Movie",
    ],
    "properties": {
      "title": "Wall Street"
     }
  }
  
  {
    "identity": 7,
    "labels": [
      "Movie",
    ],
    "properties": {
      "title": "The American President"
     }
  }
  
2.2. Get all nodes with a label
  $ ./matchManual.exe <<-"EOF"
  > MATCH (movie:Movie)
  > RETURN movie.title;
  Was created 7 nodes
  Was created 7 edges
  -------------------------------
  movie.title
  "Wall Street"
  "The American President"

2.3. Related nodes
  $ ./matchManual.exe <<-"EOF"
  > MATCH (director {name: 'Oliver Stone'})--(movie)
  > RETURN movie.title;
  Was created 7 nodes
  Was created 7 edges
  -------------------------------
  movie.title
  "Wall Street"

2.4. Match with labels
  $ ./matchManual.exe <<-"EOF"
  > MATCH (:Person {name: 'Oliver Stone'})--(movie:Movie)
  > RETURN movie.title;
  Was created 7 nodes
  Was created 7 edges
  -------------------------------
  movie.title
  "Wall Street"

3.1. Outgoing relationships
  $ ./matchManual.exe <<-"EOF"
  > MATCH (:Person {name: 'Oliver Stone'})-->(movie)
  > RETURN movie.title;
  Was created 7 nodes
  Was created 7 edges
  -------------------------------
  movie.title
  "Wall Street"

3.2. Directed relationships and variable
  $ ./matchManual.exe <<-"EOF"
  > MATCH (:Person {name: 'Oliver Stone'})-[r]->(movie)
  > RETURN type(r);
  Was created 7 nodes
  Was created 7 edges
  -------------------------------
  type(r)
  "DIRECTED"

3.3. Match on relationship type
MATCH (wallstreet:Movie {title: 'Wall Street'})<-[:ACTED_IN]-(actor)
RETURN actor.name
Стрелки связей влево не поддерживаются.
Для исполнения данного запроса необходимо направить стрелку связи вправо. 
  $ ./matchManual.exe <<-"EOF"
  > MATCH (actor)-[:ACTED_IN]->(wallstreet:Movie {title: 'Wall Street'})
  > RETURN actor.name;
  Was created 7 nodes
  Was created 7 edges
  -------------------------------
  actor.name
  "Martin Sheen"
  "Charlie Sheen"
  "Michael Douglas"

3.4. Match on multiple relationship types
MATCH (wallstreet {title: 'Wall Street'})<-[:ACTED_IN|:DIRECTED]-(person)
RETURN person.name
Не поддерживается поиск по нескольким меткам ребер.

3.5. Match on relationship type and use a variable
MATCH (wallstreet {title: 'Wall Street'})<-[r:ACTED_IN]-(actor)
RETURN r.role
Стрелки связей влево не поддерживаются.
Для исполнения данного запроса необходимо направить стрелку связи вправо.
  $ ./matchManual.exe <<-"EOF"
  > MATCH (actor)-[r:ACTED_IN]->(wallstreet {title: 'Wall Street'})
  > RETURN r.role;
  Was created 7 nodes
  Was created 7 edges
  -------------------------------
  r.role
  "Carl Fox"
  "Bud Fox"
  "Gordon Gekko"

4.1. Relationship types with uncommon characters
  $ ./matchManual.exe <<-"EOF"
  > MATCH (charlie:Person {name: 'Charlie Sheen'}), (rob:Person {name: 'Rob Reiner'})
  > CREATE (rob)-[:`TYPE INCLUDING A SPACE`]->(charlie);
  > MATCH (n {name: 'Rob Reiner'})-[r:`TYPE INCLUDING A SPACE`]->()
  > RETURN type(r);
  Was created 7 nodes
  Was created 7 edges
  Was created 1 edges
  -------------------------------
  type(r)
  "TYPE INCLUDING A SPACE"

Отношения переменной длины и поиск по шаблону связей 
в обе стороны 4.2-4.7 не поддерживаются.

4.8. Named paths
MATCH p = (michael {name: 'Michael Douglas'})-->()
RETURN p
Чтобы вывести данные о ребре можно переписать запрос в следующем виде
  $ ./matchManual.exe <<-"EOF"
  > MATCH (michael {name: 'Michael Douglas'})-[p]->()
  > RETURN p;
  Was created 7 nodes
  Was created 7 edges
  -------------------------------
  p
  {
    "identity": 11,
    "start": 2,
    "end": 7,
    "type": "ACTED_IN",
    "properties": {
      "role": "President Andrew Shepherd"
     }
  }
  
  {
    "identity": 9,
    "start": 2,
    "end": 6,
    "type": "ACTED_IN",
    "properties": {
      "role": "Gordon Gekko"
     }
  }
  
4.9. Matching on a bound relationship
  $ ./matchManual.exe <<-"EOF"
  > MATCH (a)-[r]-(b)
  > WHERE id (r) = 10
  > RETURN a ,b;
  Was created 7 nodes
  Was created 7 edges
  -------------------------------
  a
  {
    "identity": 6,
    "labels": [
      "Movie",
    ],
    "properties": {
      "title": "Wall Street"
     }
  }
  
  {
    "identity": 3,
    "labels": [
      "Person",
    ],
    "properties": {
      "name": "Charlie Sheen"
     }
  }
  
  -------------------------------
  b
  {
    "identity": 3,
    "labels": [
      "Person",
    ],
    "properties": {
      "name": "Charlie Sheen"
     }
  }
  
  {
    "identity": 6,
    "labels": [
      "Movie",
    ],
    "properties": {
      "title": "Wall Street"
     }
  }
  
5. Shortest path
Поиск кратчайших путей 5.1-5.3 не поддерживается 

6.1. Node by id
  $ ./matchManual.exe <<-"EOF"
  > MATCH (n)
  > WHERE id(n) = 3
  > RETURN n;
  Was created 7 nodes
  Was created 7 edges
  -------------------------------
  n
  {
    "identity": 3,
    "labels": [
      "Person",
    ],
    "properties": {
      "name": "Charlie Sheen"
     }
  }
  

6.2. Relationship by id
  $ ./matchManual.exe <<-"EOF"
  > MATCH ()-[r]->()
  > WHERE id(r) = 10
  > RETURN r;
  Was created 7 nodes
  Was created 7 edges
  -------------------------------
  r
  {
    "identity": 10,
    "start": 3,
    "end": 6,
    "type": "ACTED_IN",
    "properties": {
      "role": "Bud Fox"
     }
  }
  

6.3. Multiple nodes by id
MATCH (n)
WHERE id(n) IN [0, 3, 5]
RETURN n
Оператор IN не поддерживается.
