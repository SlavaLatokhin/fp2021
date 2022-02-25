open Csharp_lib.Parser
open Csharp_lib.Test_interpreter

let () =
  let s = Stdio.In_channel.input_all stdin in
  let parse_s = Option.get (apply_parser parser s) in
  test_interp parse_s [] false
