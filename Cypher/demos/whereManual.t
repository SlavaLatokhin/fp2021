2.1. Boolean operations
MATCH (n:Person)
WHERE n.name = 'Peter' XOR (n.age < 30 AND n.name = 'Timothy') OR NOT (n.name = 'Timothy' OR n.name = 'Peter')
RETURN
n.name AS name,
n.age AS age
ORDER BY name
Присваивание переменной значения при помощи AS не поддерживается. 
Подобные запросы можно построить без AS.
  $ ./whereManual.exe <<-"EOF"
  > MATCH (n:Person)
  > WHERE n.name = 'Peter' XOR (n.age < 30 AND n.name = 'Timothy') OR NOT (n.name = 'Timothy' OR n.name = 'Peter')
  > RETURN
  >   n.name,
  >   n.age 
  > ORDER BY n.name;
  Was created 7 nodes
  Was created 6 edges
  -------------------------------
  n.name
  "Andy"
  "Peter"
  "Timothy"
  -------------------------------
  n.age
  36
  35
  25

2.2. Filter on node label
MATCH (n)
WHERE n:Swedish
RETURN n.name, n.age
Метки в WHERE не поддерживаются. Подобные запросы можно перестроить.
  $ ./whereManual.exe <<-"EOF"
  > MATCH (n:Swedish)
  > RETURN n.name, n.age;
  Was created 7 nodes
  Was created 6 edges
  -------------------------------
  n.name
  "Andy"
  -------------------------------
  n.age
  36

2.3. Filter on node property
  $ ./whereManual.exe <<-"EOF"
  > MATCH (n:Person)
  > WHERE n.age < 30
  > RETURN n.name, n.age;
  Was created 7 nodes
  Was created 6 edges
  -------------------------------
  n.name
  "Timothy"
  -------------------------------
  n.age
  25

2.4. Filter on relationship property
  $ ./whereManual.exe <<-"EOF"
  > MATCH (n:Person)-[k:KNOWS]->(f)
  > WHERE k.since < 2000
  > RETURN f.name, f.age, f.email;
  Was created 7 nodes
  Was created 6 edges
  -------------------------------
  f.name
  "Peter"
  -------------------------------
  f.age
  35
  -------------------------------
  f.email
  "peter_n@example.com"

2.5. Filter on dynamically-computed node property

WITH 'AGE' AS propname
MATCH (n:Person)
WHERE n[toLower(propname)] < 30
RETURN n.name, n.age

Динамически вычисляемые свойства не поддерживается, данный запрос можно перестроить.
  $ ./whereManual.exe <<-"EOF"
  > MATCH (n:Person)
  > WHERE n.age < 30
  > RETURN n.name, n.age;
  Was created 7 nodes
  Was created 6 edges
  -------------------------------
  n.name
  "Timothy"
  -------------------------------
  n.age
  25

2.6. Property existence checking
  $ ./whereManual.exe <<-"EOF"
  > MATCH (n:Person)
  > WHERE n.belt IS NOT NULL
  > RETURN n.name, n.belt;
  Was created 7 nodes
  Was created 6 edges
  -------------------------------
  n.name
  "Andy"
  -------------------------------
  n.belt
  "white"

3.1. Prefix string search using STARTS WITH
  $ ./whereManual.exe <<-"EOF"
  > MATCH (n:Person)
  > WHERE n.name STARTS WITH 'Pet'
  > RETURN n.name, n.age;
  Was created 7 nodes
  Was created 6 edges
  -------------------------------
  n.name
  "Peter"
  -------------------------------
  n.age
  35

3.2. Suffix string search using ENDS WITH
  $ ./whereManual.exe <<-"EOF"
  > MATCH (n:Person)
  > WHERE n.name ENDS WITH 'ter'
  > RETURN n.name, n.age;
  Was created 7 nodes
  Was created 6 edges
  -------------------------------
  n.name
  "Peter"
  -------------------------------
  n.age
  35

3.3. Substring search using CONTAINS
  $ ./whereManual.exe <<-"EOF"
  > MATCH (n:Person)
  > WHERE n.name CONTAINS 'ete'
  > RETURN n.name, n.age;
  Was created 7 nodes
  Was created 6 edges
  -------------------------------
  n.name
  "Peter"
  -------------------------------
  n.age
  35

3.4. String matching negation
  $ ./whereManual.exe <<-"EOF"
  > MATCH (n:Person)
  > WHERE NOT n.name ENDS WITH 'y'
  > RETURN n.name, n.age;
  Was created 7 nodes
  Was created 6 edges
  -------------------------------
  n.name
  "Peter"
  -------------------------------
  n.age
  35

4. Regular expressions
Регулярные выражения 4.1-4.3 не поддерживаются.

5. Using path patterns in WHERE
Запросы по шаблонам путей в WHERE 5.1-5.4 не поддерживаются.

6. Using existential subqueries in WHERE
Запросы WHERE EXISTS 6.1-6.3 не поддерживаются.

7. Lists
Оператор IN [...] 7.1 не поддерживается.

8.1. Default to false if property is missing
  $ ./whereManual.exe <<-"EOF"
  > MATCH (n:Person)
  > WHERE n.belt = 'white'
  > RETURN n.name, n.age, n.belt;
  Was created 7 nodes
  Was created 6 edges
  -------------------------------
  n.name
  "Andy"
  -------------------------------
  n.age
  36
  -------------------------------
  n.belt
  "white"

8.2. Default to true if property is missing
  $ ./whereManual.exe <<-"EOF"
  > MATCH (n:Person)
  > WHERE n.belt = 'white' OR n.belt IS NULL
  > RETURN n.name, n.age, n.belt
  > ORDER BY n.name;
  Was created 7 nodes
  Was created 6 edges
  -------------------------------
  n.name
  "Andy"
  "Peter"
  "Timothy"
  -------------------------------
  n.age
  36
  35
  25
  -------------------------------
  n.belt
  "white"
  null
  null

8.3. Filter on null
  $ ./whereManual.exe <<-"EOF"
  > MATCH (person:Person)
  > WHERE person.name = 'Peter' AND person.belt IS NULL
  > RETURN person.name, person.age, person.belt;
  Was created 7 nodes
  Was created 6 edges
  -------------------------------
  person.name
  "Peter"
  -------------------------------
  person.age
  35
  -------------------------------
  person.belt
  null

9.1. Simple range
  $ ./whereManual.exe <<-"EOF"
  > MATCH (a:Person)
  > WHERE a.name >= 'Peter'
  > RETURN a.name, a.age;
  Was created 7 nodes
  Was created 6 edges
  -------------------------------
  a.name
  "Peter"
  "Timothy"
  -------------------------------
  a.age
  35
  25

9.2. Composite range
  $ ./whereManual.exe <<-"EOF"
  > MATCH (a:Person)
  > WHERE a.name > 'Andy' AND a.name < 'Timothy'
  > RETURN a.name, a.age;
  Was created 7 nodes
  Was created 6 edges
  -------------------------------
  a.name
  "Peter"
  -------------------------------
  a.age
  35

9.3. Pattern element predicates
WHERE внутри шаблонов MATCH и RETURN 9.3.1 не поддерживается
