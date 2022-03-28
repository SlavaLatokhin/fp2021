open Interpreter

(* let () =
  let input_str = {|  (display '(^-^) )  |} in
  match parse_and_run_prog input_str with
  | Ok (_, _) ->
    ()
    (* let _ = Format.printf "Actual ctx: \n" in
    let _ =
      List.map
        (fun var -> Format.printf "%a = %a;\n " pp_variable var.name pp_value var.value)
        ctx.vars
    in
    Format.printf "\nActual ans: %a" pp_value ans *)
    (* () *)
  | Error (ERROR err) -> Printf.printf "%s" err
;; *)

(* open Opal
open Parser
open Ast

let () =
  let input =
    LazyStream.of_string {|  (display "(^-^)" )  |}
  in
  match parse prog input with
  | Some ans -> Format.printf "Actual: %a\n" pp_program ans
  | None -> print_endline "ERROR!"
;; *)

exception Test_failed

let test_suc input_str expr expected =
  match expr with
  | Error _ -> raise Test_failed
  | Ok (_, ans) ->
    let _ =
      if ans <> expected
      then
        Format.printf
          "Was interpreted: %s \nExpected: %a\nActual: %a\n\n"
          input_str
          pp_value
          expected
          pp_value
          ans
    in
    ans = expected
;;

let%test _ =
  let input_str = {| (+) |} in
  let expr = parse_and_run_prog input_str in
  test_suc input_str expr (VInt 0)
;;

let%test _ =
  let input_str = {| (+ 1 2 3 4 5) |} in
  let expr = parse_and_run_prog input_str in
  test_suc input_str expr (VInt 15)
;;

let%test _ =
  let input_str = {| (- 15 1 2 3 4 5) |} in
  let expr = parse_and_run_prog input_str in
  test_suc input_str expr (VInt 0)
;;

let%test _ =
  let input_str = {| ( * ) |} in
  let expr = parse_and_run_prog input_str in
  test_suc input_str expr (VInt 1)
;;

let%test _ =
  let input_str = {| ( * 1 2 3 4 5) |} in
  let expr = parse_and_run_prog input_str in
  test_suc input_str expr (VInt 120)
;;

let%test _ =
  let input_str = {| (not 1) |} in
  let expr = parse_and_run_prog input_str in
  test_suc input_str expr (VBool false)
;;

let%test _ =
  let input_str = {| (not #f) |} in
  let expr = parse_and_run_prog input_str in
  test_suc input_str expr (VBool true)
;;

let%test _ =
  let input_str = {| (and #t #t #t) |} in
  let expr = parse_and_run_prog input_str in
  test_suc input_str expr (VBool true)
;;

let%test _ =
  let input_str = {| (and #t #f #t) |} in
  let expr = parse_and_run_prog input_str in
  test_suc input_str expr (VBool false)
;;

let%test _ =
  let input_str = {| (or #f #f #f) |} in
  let expr = parse_and_run_prog input_str in
  test_suc input_str expr (VBool false)
;;

let%test _ =
  let input_str = {| (or #t #f #t) |} in
  let expr = parse_and_run_prog input_str in
  test_suc input_str expr (VBool true)
;;

let%test _ =
  let input_str = {| (apply (if #f + *) (list 1 2 3 4 5)) |} in
  let expr = parse_and_run_prog input_str in
  test_suc input_str expr (VInt 120)
;;

let%test _ =
  let input_str = {| (zero? 0) |} in
  let expr = parse_and_run_prog input_str in
  test_suc input_str expr (VBool true)
;;

let%test _ =
  let input_str = {| ((lambda (x y) (* (+ x y) x)) 12 13) |} in
  let expr = parse_and_run_prog input_str in
  test_suc input_str expr (VInt 300)
;;
