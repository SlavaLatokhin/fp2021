open Parser
open Interpret_classes.Interpret_classes (Interpret_classes.Result)
open Interpreter.Interpreter (Interpret_classes.Result)
open Value_types

let print_ctx_res res_context = function
  | true -> print_endline (show_context res_context ^ "\n")
  | false -> print_endline ""

let test_interp class_list_ast class_map tf =
  match interpret_classes class_list_ast class_map with
  | Error m -> print_endline m
  | Ok load_map -> (
    match start_interpreting load_map with
    | Error m -> print_endline m
    | Ok res_context -> print_ctx_res res_context tf )

let interpret s tf =
  let parse_s = Option.get (apply_parser parser s) in
  test_interp parse_s KeyMap.empty tf
