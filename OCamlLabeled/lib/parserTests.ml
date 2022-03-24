open Ast
open Parser

let test_parse ~label ~code ~expected =
  match parse prog code with
  | Error _ ->
    Printf.printf "[Parser test] %s -> PARSE ERROR" label;
    false
  | Result.Ok res when expected = res -> true
  | Result.Ok res ->
    let () =
      Printf.printf "[Parser test] %s failed.\nActual is:\n%s\n" label (show_prog res)
    in
    false
;;

let%test _ =
  test_parse
    ~label:"Fun with labeled args"
    ~code:{|

    let f x y = g ~x:None ~y:(Some 5)

    |}
    ~expected:
      [ DLet
          ( false
          , PVar "f"
          , EFun
              ( PVar "x"
              , EFun
                  ( PVar "y"
                  , EApp
                      ( EApp (EVar "g", EArg (Labeled ("x", ENone)))
                      , EArg (Labeled ("y", ESome (EConst (CInt 5)))) ) ) ) )
      ]
;;

let%test _ =
  test_parse
    ~label:"Test options"
    ~code:{|

    let f = function | Some x :: xs -> 28 | [None] -> 42 | _ -> 1337

    |}
    ~expected:
      [ DLet
          ( false
          , PVar "f"
          , EFun
              ( PVar "match"
              , EMatch
                  ( EVar "match"
                  , [ PCons (PSome (PVar "x"), PVar "xs"), EConst (CInt 28)
                    ; PCons (PNone, PNil), EConst (CInt 42)
                    ; PWild, EConst (CInt 1337)
                    ] ) ) )
      ]
;;

let%test _ =
  test_parse
    ~label:"Fun with labeled and optional args"
    ~code:{|

    let f ?(x = 5) ~y ?(z = 3/3)= x + y

    |}
    ~expected:
      [ DLet
          ( false
          , PVar "f"
          , EFun
              ( POptional ("x", EConst (CInt 5))
              , EFun
                  ( PLabeled "y"
                  , EFun
                      ( POptional ("z", EOp (Div, EConst (CInt 3), EConst (CInt 3)))
                      , EOp (Add, EVar "x", EVar "y") ) ) ) )
      ]
;;

let%test _ =
  test_parse
    ~label:"Ref test"
    ~code:{|

    let r = ref 0;;
    let _ = r := 50 + 10;;

    |}
    ~expected:
      [ DLet (false, PVar "r", EApp (EVar "ref", EArg (Expr (EConst (CInt 0)))))
      ; DLet
          ( false
          , PWild
          , EApp
              (EApp (EVar ":=", EVar "r"), EOp (Add, EConst (CInt 50), EConst (CInt 10)))
          )
      ]
;;

let%test _ =
  test_parse
    ~label:"Ref test"
    ~code:
      {|

    let f =
    let n = ref 0 in
    (fun m -> let _ = n := 1 + !n in !n);;
    let x1 = f 0;;
    let x2 = f 0;;
    let x3 = f 0;;
    let x4 = f 0;;
    let x5 = f 0;;

    |}
    ~expected:
      [ DLet
          ( false
          , PVar "f"
          , ELet
              ( [ false, PVar "n", EApp (EVar "ref", EArg (Expr (EConst (CInt 0)))) ]
              , EFun
                  ( PVar "m"
                  , ELet
                      ( [ ( false
                          , PWild
                          , EApp
                              ( EApp (EVar ":=", EVar "n")
                              , EOp (Add, EConst (CInt 1), EApp (EVar "!", EVar "n")) ) )
                        ]
                      , EApp (EVar "!", EVar "n") ) ) ) )
      ; DLet (false, PVar "x1", EApp (EVar "f", EArg (Expr (EConst (CInt 0)))))
      ; DLet (false, PVar "x2", EApp (EVar "f", EArg (Expr (EConst (CInt 0)))))
      ; DLet (false, PVar "x3", EApp (EVar "f", EArg (Expr (EConst (CInt 0)))))
      ; DLet (false, PVar "x4", EApp (EVar "f", EArg (Expr (EConst (CInt 0)))))
      ; DLet (false, PVar "x5", EApp (EVar "f", EArg (Expr (EConst (CInt 0)))))
      ]
;;
