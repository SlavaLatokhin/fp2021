open Csharp_lib.Parser

open
  Csharp_lib.Interpret_classes.Interpret_classes
    (Csharp_lib.Interpret_classes.Result)

open Csharp_lib.Interpreter.Interpreter (Csharp_lib.Interpret_classes.Result)

let test_interp class_list_ast class_list =
  match interpret_classes class_list_ast class_list with
  | Error m -> print_endline m
  | Ok load_list -> (
    match start_interpreting load_list with
    | Error m -> print_endline m
    | Ok res_context -> print_endline (show_context res_context ^ "\n") )

let () =
  let s = Stdio.In_channel.input_all stdin in
  let parse_s = Option.get (apply_parser parser s) in
  test_interp parse_s []
