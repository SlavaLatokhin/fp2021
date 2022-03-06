open Cypher_lib.Parser
open Cypher_lib.Ast
open DemoCypherInterpret

let () =
  let str = Stdio.In_channel.input_all Caml.stdin in
  strt_intrp str
;;
