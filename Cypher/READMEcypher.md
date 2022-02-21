# Mini Cypher.

## `CREATE`
Позволяет создавать узлы и связи по различному количеству переданных данных (меток и свойств).

`Создание узла:`
    
    CREATE (:Person);
    CREATE ({name:"Jack"});
    CREATE (:Person :Student {name:"Jack", age 25});

`Создание связи`

работает по предварительной записи узла в переменную:
    
    CREATE (node1 :Person), (node2 :Person :Student)
    CREATE (node1)-[:PARENT]->(node2);

либо самостоятельно создает необходимые узлы, а затем связь:

    CREATE (:Person)-[:PARENT]->(:Person);

## `MATCH`
Позволяет искать узлы и связи по различному количеству переданных данных (меток и свойств). Работает в связках с DETACH DELETE, RETURN, CREATE.

`Поиск узла`

    MATCH (n) ...
    MATCH (n :Person) ...
    MATCH (n {name: "Jack"}) ...
    MATCH (n :Person :Student {name: "Jack", age: 25}) ...

`Поиск связей`

    MATCH ()-[r]-()
    MATCH (n1)-[]-(n2)
    MATCH (n1 :Person)-[r]-(n2 :Student)
    MATCH (n1 {name: "Jack"})-[r: Parent {role: "Father"}]->(n2 :Student)

### `WHERE`
Позволяет накладывать дополнительные ограничения на шаблоны поиска MATCH, при помощи операторов сравнения `<`, `<=`, `>`, `>=`, `<>`, `=`, логических операторов `AND`, `OR`, операторов поиска подстрок `STARTS WITH`, `ENDS WITH`, `CONTAINS`. Соответсвенно все это можно комбинировать.

    MATCH ... WHERE n.age > 20 AND n.name = "Jack" OR n.age = 35 ...
    MATCH ... n.name CONTAINS "Al" AND n.name ENDS WITH "sa" OR n.name STARTS WITH "Gar" ...

## `DETACH DELETE`
Позволяет удалять узлы и связи, предварительно записанные в переменную. Если узел имеет связи, то удаляются все исходящие и входящие ребра.

    ... DETACH DELETE n1, n2, r

## `RETURN`
Позволяет отображать узлы, связи, свойства, логические условия ввиде true и false. Печать реализована в максимально приближенном к Cypher'у виде.

    ... RETURN n
    
    n 
    {
    "identity": 0,
    "labels": [
      "Person",
    ],
    "properties": {
      "age": 25
      "name": "Jack"
     }
    }

---

    ... RETURN r

    r
    {
      "identity": 1,
      "start": 0,
      "end": 2,
      "type": "PARENT",
      "properties": {
      "role": "Father"
      }
    }

---

    ... RETURN n.name, n.age, n.age > 20
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
    n.age > 20
    true
    false
    false
    