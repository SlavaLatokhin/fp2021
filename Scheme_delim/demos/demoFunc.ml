open Scheme_delim_lib

let () =
  let s = Stdio.In_channel.input_all Caml.stdin in
  match Interpreter.run_program s with
  | Ok _ -> ()
  | Error _ -> Format.printf "Test failed"
;;
