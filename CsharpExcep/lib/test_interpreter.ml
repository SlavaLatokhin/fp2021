open Parser
open Interpret_classes.Interpret_classes (Interpret_classes.Result)
open Interpreter.Interpreter (Interpret_classes.Result)

let print_ctx_res res_context tf =
  match tf with
  | true -> print_endline (show_context res_context ^ "\n")
  | false -> print_endline ""

let test_interp class_list_ast class_list tf =
  match interpret_classes class_list_ast class_list with
  | Error m -> print_endline m
  | Ok load_list -> (
    match start_interpreting load_list with
    | Error m -> print_endline m
    | Ok res_context -> print_ctx_res res_context tf )

let interpret s tf =
  let parse_s = Option.get (apply_parser parser s) in
  test_interp parse_s [] tf
