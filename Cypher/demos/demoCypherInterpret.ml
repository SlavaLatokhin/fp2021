open Cypher_lib.Parser
open Cypher_lib.Interpreter

let () =
  let str = Stdio.In_channel.input_all Caml.stdin in
  let parsed = parse_with pcmdssep str in
  let open Caml.Format in
  match parsed with
  | Error err -> printf "%s%!" err
  | Ok commands ->
    (match interpret_program commands with
    | Error err -> printf "%a%!" pp_err err
    | Ok (_, _) -> printf "")
;;
