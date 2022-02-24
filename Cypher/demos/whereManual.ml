open Cypher_lib.Parser
open Cypher_lib.Interpreter

let () =
  let creategraph =
    {|   
    CREATE (d1:Dog{name: "Andy"}),
   (p2:Person{name: "Peter", email: "peter_n@example.com", age: 35}), (p3:Person{age:25, address: "Sweden/Malmo", name: "Timothy"}),
   (d2:Dog{name: "Fido"}),(p1:Swedish :Person{name: "Andy", age: 36, belt: "white"}), (d3:Dog{name: "Ozzy"}),
   (t1:Toy{name: "Banana"}), (p1)-[:HAS_DOG {since: 2016}]->(d1),
   (p1)-[:KNOWS {since: 1999}]->(p2), (p1)-[:KNOWS {since: 2021}]->(p3),
   (p2)-[:HAS_DOG {since: 2010}]->(d2),(p2)-[:HAS_DOG {since: 2018}]->(d3),
   (d2)-[:HAS_TOY ]->(t1);
   |}
  in
  let str = String.concat "" [ creategraph; Stdio.In_channel.input_all Caml.stdin ] in
  let parsed = parse_with pcmdssep str in
  let open Caml.Format in
  match parsed with
  | Error err -> printf "%s%!" err
  | Ok commands ->
    (match interpret_program commands with
    | Error err -> printf "%a%!" pp_err err
    | Ok (_, _) -> printf "")
;;
