open Cypher_lib.Parser
open Cypher_lib.Interpreter

let () =
  let creategraph =
    {|   
    CREATE (p1:Person{name: "Oliver Stone"}),(p2:Person{name: "Michael Douglas"}),
   (p3:Person{name: "Charlie Sheen"}), (p4:Person{name: "Martin Sheen"}),
   (p5:Person{name: "Rob Reiner"}), (m1:Movie{title: "Wall Street"}),
   (m2:Movie{title: "The American President"}), (p1)-[:DIRECTED]->(m1),
   (p2)-[:ACTED_IN {role: "Gordon Gekko" }]->(m1), (p3)-[:ACTED_IN {role: "Bud Fox" }]->(m1),
   (p2)-[:ACTED_IN {role: "President Andrew Shepherd" }]->(m2), (p4)-[:ACTED_IN {role: "Carl Fox" }]->(m1),
   (p4)-[:ACTED_IN {role: "A.J. Maelnerney" }]->(m2), (p5)-[:DIRECTED]->(m2);
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
