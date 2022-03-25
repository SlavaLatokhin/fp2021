open Scheme_call_lib.Interpreter

let () =
  let scheme_code = Stdio.In_channel.input_all stdin in
  match parse_and_run_prog scheme_code with
  | Ok _ -> ()
  | Error (ERROR err) -> Printf.printf "%s" err
;;
