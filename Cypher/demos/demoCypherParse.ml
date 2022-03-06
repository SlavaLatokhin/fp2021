open Cypher_lib.Parser
open Cypher_lib.Ast

let () =
  let str = Stdio.In_channel.input_all Caml.stdin in
  let parsed = parse_with pcmdssep str in
  let open Caml.Format in
  match parsed with
  | Error err -> printf "%s%!" err
  | Ok commands -> printf "%a%!" pp_program commands
;;
