open Ast
open Opal

let apply_parser p s = parse p (LazyStream.of_string s)

let reserved =
  [ "true"; "false"; "if"; "else"; "while"; "public"; "static"; "const"
  ; "override"; "try"; "catch"; "finally"; "when"; "void"; "string"; "char"
  ; "Console"; "namespace"; "using"; "int"; "bool"; "for"; "null"; "new"
  ; "return"; "break"; "continue"; "class" ]

let const = token "const" >> return Const
let parens = between (token "(") (token ")")
let braces = between (token "{") (token "}")

let modifier_list =
  many
    (choice
       [ token "public" >> return Public; token "static" >> return Static
       ; token "const" >> return Const; token "override" >> return Override ] )

let number = spaces >> many1 digit => implode
let integer = number => int_of_string

module Expression = struct
  open Ast

  let get_string =
    let string_of_chars chars =
      let buf = Buffer.create 16 in
      List.iter (Buffer.add_char buf) chars ;
      Buffer.contents buf in
    token "\""
    >> many (satisfy (fun x -> x <> '\"'))
    >>= fun list ->
    token "\"" >> return (ConstExpr (VString (string_of_chars list)))

  let plus_op = token "+" >> return (fun x y -> Plus (x, y))
  let min_op = token "-" >> return (fun x y -> Min (x, y))
  let mul_op = token "*" >> return (fun x y -> Mul (x, y))
  let div_op = token "/" >> return (fun x y -> Div (x, y))
  let mod_op = token "%" >> return (fun x y -> Mod (x, y))
  let and_op = token "&&" >> return (fun x y -> And (x, y))
  let or_op = token "||" >> return (fun x y -> Or (x, y))
  let less_op = token "<" >> return (fun x y -> Less (x, y))
  let more_op = token ">" >> return (fun x y -> More (x, y))
  let less_or_eq_op = token "<=" >> return (fun x y -> LessOrEqual (x, y))
  let more_or_eq_op = token ">=" >> return (fun x y -> MoreOrEqual (x, y))
  let eq_op = token "==" >> return (fun x y -> Equal (x, y))
  let neq_op = token "!=" >> return (fun x y -> NotEqual (x, y))
  let null = token "null" >> return Null

  let ident_object =
    spaces >> letter <~> many alpha_num => implode
    >>= function x when List.mem x reserved -> mzero | x -> return x

  let get_variable = ident_object => fun x -> IdentVar x

  let atomic =
    get_variable
    <|> (integer >>= fun n -> return (ConstExpr (VInt n)))
    <|> get_string
    <|> (token "false" >> return (ConstExpr (VBool false)))
    <|> (token "true" >> return (ConstExpr (VBool true)))
    <|> null

  let define_type =
    choice
      [ token "int" >> return Int; token "string" >> return String
      ; token "void" >> return Void; token "bool" >> return Bool
      ; (ident_object >>= fun class_name -> return (CsClass class_name)) ]

  let rec expr input = num_expr input
  and num_expr input = (chainl1 and_expr or_op) input
  and and_expr input = (chainl1 comp_expr and_op) input

  and comp_expr input =
    (chainl1 add_expr
       ( less_or_eq_op <|> more_or_eq_op <|> less_op <|> more_op <|> eq_op
       <|> neq_op ) )
      input

  and add_expr input = (chainl1 mul_expr (plus_op <|> min_op)) input

  and mul_expr input =
    (chainl1 unaric_expr (mul_op <|> div_op <|> mod_op)) input

  and unaric_expr input =
    choice
      [ (token "!" >> lexeme primar_expr >>= fun x -> return (Not x))
      ; ( token "-" >> lexeme primar_expr
        >>= fun x -> return (Min (ConstExpr (VInt 0), x)) )
      ; (token "++" >> lexeme primar_expr >>= fun x -> return (PrefInc x))
      ; (token "--" >> lexeme primar_expr >>= fun x -> return (PrefDec x))
      ; (lexeme primar_expr >>= fun x -> token "++" >> return (PostInc x))
      ; (lexeme primar_expr >>= fun x -> token "--" >> return (PostDec x))
      ; primar_expr ]
      input

  and primar_expr input =
    (init_instance <|> assign <|> call_method <|> parens expr <|> atomic) input

  and split_by_comma input = sep_by expr (token ",") input

  and call_method input =
    ( ident_object
    >>= fun name ->
    token "(" >> split_by_comma
    >>= fun args_list -> token ")" >> return (CallMethod (name, args_list)) )
      input

  and init_instance input =
    ( token "new" >> ident_object
    >>= fun name ->
    token "(" >> split_by_comma
    >>= fun args_list -> token ")" >> return (ClassCreate (name, args_list)) )
      input

  and assign input =
    let parse_left = call_method <|> get_variable in
    ( parse_left
    >>= fun left ->
    token "=" >> expr >>= fun right -> return (Assign (left, right)) )
      input
end

module Statement = struct
  open Expression

  let rec parse_statements input =
    choice
      [ continue; break; parse_expr; return_stat; if_stat; while_stat; throw
      ; var_declare; for_stat; statement_block; try_stat; print_cs ]
      input

  and if_stat input =
    ( token "if" >> parens expr
    >>= fun condition ->
    parse_statements
    >>= fun if_body ->
    choice
      [ ( token "else" >> parse_statements
        >>= fun else_body -> return (If (condition, if_body, Some else_body)) )
      ; return (If (condition, if_body, None)) ] )
      input

  and statement_block input =
    ( braces (sep_by parse_statements spaces)
    >>= fun stats -> return (StatementBlock stats) )
      input

  and while_stat input =
    ( token "while" >> parens expr
    >>= fun condition ->
    parse_statements >>= fun while_body -> return (While (condition, while_body))
    )
      input

  and var_declare input =
    let helper =
      ident_object
      >>= fun var_name ->
      token "=" >> expr
      >>= (fun var_value -> return (var_name, Some var_value))
      <|> return (var_name, None) in
    choice
      [ ( const
        >>= fun const ->
        define_type
        >>= fun var_type ->
        sep_by1 helper (token ",")
        >>= fun var_pair ->
        token ";" >> return (VarDeclare (Some const, var_type, var_pair)) )
      ; ( define_type
        >>= fun var_type ->
        sep_by1 helper (token ",")
        >>= fun var_pair ->
        token ";" >> return (VarDeclare (None, var_type, var_pair)) ) ]
      input

  and for_stat input =
    ( token "for" >> token "("
    >> choice
         [ (var_declare >>= fun var -> return (Some var))
         ; token ";" >> return None ]
    >>= fun declare ->
    choice
      [ (expr >>= fun expr -> token ";" >> return (Some expr))
      ; token ";" >> return None ]
    >>= fun condition ->
    sep_by expr (token ",")
    >>= fun after ->
    token ")" >> parse_statements
    >>= fun body -> return (For (declare, condition, after, body)) )
      input

  and throw input =
    ( token "throw" >> expr
    >>= fun throw_expr -> token ";" >> return (Throw throw_expr) )
      input

  and parse_expr input =
    (expr >>= fun express -> token ";" >> return (Expression express)) input

  and return_stat input =
    ( token "return"
    >> choice
         [ ( skip_many1 space >> expr
           >>= fun result -> token ";" >> return (Return (Some result)) )
         ; token ";" >> return (Return None) ] )
      input

  and continue input = (token "continue" >> token ";" >> return Continue) input
  and break input = (token "break" >> token ";" >> return Break) input

  and try_stat input =
    (*catch cases:
      catch {}
      catch (Exception) {}
    *)
    let catch =
      token "catch"
      >> choice
           [ ( token "(" >> Expression.define_type
             >>= fun excep_type ->
             token ")" >> parse_statements
             >>= fun catch_block -> return (Some excep_type, catch_block) )
           ; (parse_statements >>= fun catch_block -> return (None, catch_block))
           ] in
    ( token "try" >> parse_statements
    >>= fun try_stat ->
    many catch
    >>= fun catch_list ->
    match catch_list with
    | [] ->
        token "finally" >> parse_statements
        >>= fun finally_block ->
        return (Try (try_stat, catch_list, Some finally_block))
    | _ ->
        choice
          [ ( token "finally" >> parse_statements
            >>= fun finally_block ->
            return (Try (try_stat, catch_list, Some finally_block)) )
          ; return (Try (try_stat, catch_list, None)) ] )
      input

  and print_cs input =
    ( token "Console.WriteLine(" >> expr
    >>= fun print_expression -> token ");" >> return (Print print_expression) )
      input
end

let get_params =
  Expression.define_type
  >>= fun _type -> Expression.ident_object >>= fun name -> return (_type, name)

let class_method =
  Expression.define_type
  >>= fun method_type ->
  Expression.ident_object
  >>= fun method_name ->
  token "("
  >> sep_by get_params (token ",")
  >>= fun params_list ->
  token ")" >> Statement.statement_block
  >>= fun stat_block ->
  return (Method (method_type, method_name, params_list, stat_block))

let field =
  let helper =
    Expression.ident_object
    >>= fun name ->
    token "=" >> Expression.expr
    >>= (fun value -> return (name, Some value))
    <|> return (name, None) in
  Expression.define_type
  >>= fun f_type ->
  sep_by helper (token ",")
  >>= fun var_list -> token ";" >> return (VariableField (f_type, var_list))

let class_elements =
  modifier_list
  >>= fun modifiers ->
  field <|> class_method >>= fun class_elem -> return (modifiers, class_elem)

let parse_class =
  modifier_list
  >>= fun modifiers ->
  token "class" >> Expression.ident_object
  >>= fun name ->
  choice
    [ ( token ":" >> Expression.ident_object
      >>= fun parent -> return (Some parent) ); return None ]
  >>= fun _parent ->
  token "{"
  >> sep_by class_elements spaces
  >>= fun class_elements ->
  token "}" >> return (Class (modifiers, name, _parent, class_elements))

let parser = many parse_class

(* Tests *)

module Tests = struct
  open Expression
  open Ast

  let%test _ =
    apply_parser modifier_list "public static const override"
    = Some [Public; Static; Const; Override]

  let%test _ = apply_parser integer "4" = Some 4
  let%test _ = apply_parser null "  null" = Some Null
  let%test _ = apply_parser ident_object "   sth" = Some "sth"
  let%test _ = apply_parser ident_object "  Sth" = Some "Sth"
  let%test _ = apply_parser get_variable "   sth" = Some (IdentVar "sth")

  let%test _ =
    apply_parser get_string "\"sth\"" = Some (ConstExpr (VString "sth"))

  let%test _ = apply_parser atomic "      123" = Some (ConstExpr (VInt 123))
  let%test _ = apply_parser ident_object "  123sth" = None
  let%test _ = apply_parser atomic "\"sth\"" = Some (ConstExpr (VString "sth"))
  let%test _ = apply_parser atomic "   true" = Some (ConstExpr (VBool true))
  let%test _ = apply_parser atomic "   false" = Some (ConstExpr (VBool false))
  let%test _ = apply_parser atomic "   null" = Some Null
  let%test _ = apply_parser define_type "  int" = Some Int
  let%test _ = apply_parser define_type "  void" = Some Void
  let%test _ = apply_parser define_type "  string" = Some String
  let%test _ = apply_parser define_type "  Excep" = Some (CsClass "Excep")

  (*expression tests*)
  let%test _ =
    apply_parser expr "a = b = 1"
    = Some (Assign (IdentVar "a", Assign (IdentVar "b", ConstExpr (VInt 1))))

  let%test _ =
    apply_parser expr "1 + 2"
    = Some (Plus (ConstExpr (VInt 1), ConstExpr (VInt 2)))

  let%test _ =
    apply_parser expr "2/5 + 1 * (5 % 3)"
    = Some
        (Plus
           ( Div (ConstExpr (VInt 2), ConstExpr (VInt 5))
           , Mul
               (ConstExpr (VInt 1), Mod (ConstExpr (VInt 5), ConstExpr (VInt 3)))
           ) )

  let%test _ =
    apply_parser expr "x = true"
    = Some (Assign (IdentVar "x", ConstExpr (VBool true)))

  (*classes and statement tests*)
  let%test _ =
    apply_parser expr "new Person(19,\"Artem\")"
    = Some
        (ClassCreate
           ("Person", [ConstExpr (VInt 19); ConstExpr (VString "Artem")]) )

  let get_body =
    apply_parser Statement.statement_block
      {|
                      {
                        return message;
                      }
                  |}

  let%test _ =
    get_body = Some (StatementBlock [Return (Some (IdentVar "message"))])

  let get_dec =
    apply_parser parse_class
      {|
               public class Exception
               {
                 public string message;
                 public string ToString()
                 {
                     return message;
                 }
               }
           |}

  let%test _ =
    get_dec
    = Some
        (Class
           ( [Public]
           , "Exception"
           , None
           , [ ([Public], VariableField (String, [("message", None)]))
             ; ( [Public]
               , Method
                   ( String
                   , "ToString"
                   , []
                   , StatementBlock [Return (Some (IdentVar "message"))] ) ) ]
           ) )
end
