open Ast
open Printf
open Interpret_classes

module Interpreter (M : MONADERROR) = struct
  open M
  open Value_types

  type variable =
    { var_type: types
    ; var_key: key_t
    ; var_value: values
    ; is_const: bool
    ; assignment_count: int
    ; visibility_level: int }
  [@@deriving show {with_path= false}]

  type signal = WasBreak | WasContinue | WasReturn | WasThrown | NoSignal
  [@@deriving show {with_path= false}]

  and context =
    { variable_table: (key_t, variable) Hashtbl_der.t
    ; current_method_type: types
    ; last_expr_result: values
    ; runtime_signal: signal
    ; count_of_nested_cycles: int
    ; visibility_level: int }
  [@@deriving show {with_path= false}]

  let init_contex variable_table =
    return
      { variable_table
      ; current_method_type= Void
      ; last_expr_result= VVoid
      ; runtime_signal= NoSignal
      ; count_of_nested_cycles= 0
      ; visibility_level= 0 }

  let find_main_class (class_table : (key_t, class_t) Hashtbl.t) =
    let class_seq = Hashtbl.to_seq_values class_table in
    let rec iter_classes (class_s : class_t Seq.t) =
      match class_s () with
      | Seq.Nil -> error "There is no Main class!"
      | Seq.Cons (x, xs) -> (
        match Hashtbl.find_opt x.method_table "Main" with
        | None -> iter_classes xs
        | Some _ -> return x ) in
    iter_classes class_seq

  let rec check_expr_type cur_expr ctx class_table =
    match cur_expr with
    | Plus (left, right) -> (
        check_expr_type left ctx class_table
        >>= fun left_type ->
        match left_type with
        | Int -> (
            check_expr_type right ctx class_table
            >>= fun right_type ->
            match right_type with
            | Int -> return Int
            | String -> return String
            | _ ->
                error
                  "Incorrect type: the right side of the expression should be \
                   Int or String!" )
        | String -> (
            check_expr_type right ctx class_table
            >>= fun right_type ->
            match right_type with
            | Int | String -> return String
            | _ ->
                error
                  "Incorrect type: the right side of the expression should be \
                   Int or String!" )
        | _ ->
            error
              "Incorrect type: the expression Plus could only be with Int or \
               String type!" )
    | Min (left, right)
     |Div (left, right)
     |Mod (left, right)
     |Mul (left, right) -> (
        check_expr_type left ctx class_table
        >>= fun left_type ->
        match left_type with
        | Int -> (
            check_expr_type right ctx class_table
            >>= fun right_type ->
            match right_type with
            | Int -> return Int
            | _ -> error "Incorrect type: the type should be Int!" )
        | _ -> error "Incorrect type: the type should be Int!" )
    | PostDec value | PostInc value | PrefDec value | PrefInc value -> (
        check_expr_type value ctx class_table
        >>= fun value_type ->
        match value_type with
        | Int -> return Int
        | _ -> error "Incorrect type: the type should be Int!" )
    | And (left, right) | Or (left, right) -> (
        check_expr_type left ctx class_table
        >>= fun left_type ->
        match left_type with
        | Bool -> (
            check_expr_type right ctx class_table
            >>= fun right_type ->
            match right_type with
            | Bool -> return Bool
            | _ -> error "Incorrect type: the type should be Bool!" )
        | _ -> error "Incorrect type: the type should be Bool!" )
    | Not value -> (
        check_expr_type value ctx class_table
        >>= fun value_type ->
        match value_type with
        | Bool -> return Bool
        | _ -> error "Incorrect type: the type should be Bool!" )
    | Less (left, right)
     |More (left, right)
     |LessOrEqual (left, right)
     |MoreOrEqual (left, right) -> (
        check_expr_type left ctx class_table
        >>= fun left_type ->
        match left_type with
        | Int -> (
            check_expr_type right ctx class_table
            >>= fun right_type ->
            match right_type with
            | Int -> return Bool
            | _ -> error "Incorrect type: the type should be Int!" )
        | _ -> error "Incorrect type: the type should be Int!" )
    | Equal (left, right) | NotEqual (left, right) -> (
        check_expr_type left ctx class_table
        >>= fun left_type ->
        match left_type with
        | Int -> (
            check_expr_type right ctx class_table
            >>= fun right_type ->
            match right_type with
            | Int -> return Bool
            | _ ->
                error
                  "Incorrect type: the right side of the expression should be \
                   Int!" )
        | String -> (
            check_expr_type right ctx class_table
            >>= fun right_type ->
            match right_type with
            | String -> return Bool
            | _ ->
                error
                  "Incorrect type: the right side of the expression should be \
                   String!" )
        | Bool -> (
            check_expr_type right ctx class_table
            >>= fun right_type ->
            match right_type with
            | Bool -> return Bool
            | _ ->
                error
                  "Incorrect type: the right side of the expression should be \
                   Bool!" )
        | CsClass _ -> (
            check_expr_type right ctx class_table
            >>= fun right_type ->
            match right_type with
            | CsClass _ -> return Bool
            | _ -> error "Incorrect class type!" )
        | _ ->
            error
              "Incorrect type: the type should be Int, String, bool or CSharp \
               class!" )
    | Null -> return (CsClass "null")
    | CallMethod (method_name, _) ->
        let check_method_type meth_table meth_name =
          match Hashtbl.find_opt meth_table meth_name with
          | None ->
              error
                ("There is no given method " ^ meth_name ^ " in the Main class")
          | Some method_t -> return method_t in
        find_main_class class_table
        >>= fun curr_class ->
        check_method_type curr_class.method_table method_name
        >>= fun mth -> return mth.method_type
    | ClassCreate (class_name, _) -> (
      match Hashtbl.find_opt class_table class_name with
      | None -> error ("Class " ^ class_name ^ " wasn't found" ^ "\n")
      | Some _ -> return (CsClass class_name) )
    | IdentVar var_key -> (
      match Hashtbl.find_opt ctx.variable_table var_key with
      | None -> error ("There is no the variable " ^ var_key)
      | Some variable -> return variable.var_type )
    | ConstExpr value -> (
      match value with
      | VInt _ -> return Int
      | VBool _ -> return Bool
      | VString _ -> return String
      | VClass ObjNull -> return (CsClass "null")
      | VClass (ObjRef (class_key, _)) -> return (CsClass class_key)
      | _ -> error "Incorrect const expression type!" )
    | Assign (left, right) -> (
        check_expr_type left ctx class_table
        >>= fun left_type ->
        match left_type with
        | Void -> error "incorrect type: cannot assign to void"
        | CsClass left_key -> (
            check_expr_type right ctx class_table
            >>= fun right_type ->
            match right_type with
            | CsClass "null" -> return (CsClass left_key)
            | CsClass right_key -> return (CsClass right_key)
            | _ -> error "Incorrect type assignment!" )
        | _ ->
            check_expr_type right ctx class_table
            >>= fun right_type ->
            if left_type = right_type then return right_type
            else error "Incorrect type assignment!" )

  let expr_in_stat = function
    | PostDec _ | PostInc _ | PrefDec _ | PrefInc _
     |CallMethod (_, _)
     |Assign (_, _) ->
        true
    | _ -> false

  let rec eval_stat stat in_ctx class_table =
    match stat with
    | Expression expr ->
        if expr_in_stat expr then
          eval_expr expr in_ctx class_table >>= fun new_ctx -> return new_ctx
        else error "Incorrect expression for statement"
    | StatementBlock stat_list ->
        let rec eval_stat_bl : statement list -> context -> context M.t =
         fun stl ctx ->
          match stl with
          | [] -> return ctx
          | st :: tail -> (
            match st with
            | (Break | Continue | Return _) when tail <> [] ->
                error "Unreachable code"
            | _
              when ctx.count_of_nested_cycles >= 1
                   && ctx.runtime_signal = WasBreak ->
                return ctx
            | _
              when ctx.count_of_nested_cycles >= 1
                   && ctx.runtime_signal = WasContinue ->
                return ctx
            | _ when ctx.runtime_signal = WasReturn -> return ctx
            | _ when ctx.runtime_signal = WasThrown -> return ctx
            | _ ->
                eval_stat st ctx class_table
                >>= fun head_ctx -> eval_stat_bl tail head_ctx ) in
        eval_stat_bl stat_list in_ctx >>= fun new_ctx -> return new_ctx
    | If (expr, then_stat, else_stat_opt) -> (
        eval_expr expr in_ctx class_table
        >>= fun if_ctx ->
        match if_ctx.last_expr_result with
        | VBool true -> (
          match then_stat with
          | StatementBlock _ ->
              eval_stat then_stat
                {if_ctx with visibility_level= if_ctx.visibility_level + 1}
                class_table
              >>= fun in_ctx ->
              return {in_ctx with visibility_level= in_ctx.visibility_level - 1}
          | _ -> eval_stat then_stat if_ctx class_table )
        | VBool false -> (
          match else_stat_opt with
          | Some (StatementBlock _ as else_stat) ->
              eval_stat else_stat
                {if_ctx with visibility_level= if_ctx.visibility_level + 1}
                class_table
              >>= fun else_ctx ->
              return
                {else_ctx with visibility_level= else_ctx.visibility_level - 1}
          | Some else_stat -> eval_stat else_stat if_ctx class_table
          | None -> return in_ctx )
        | _ -> error "Incorrect type for condition statement" )
    | While (expr, stat) -> (
        let rec eval_loop loop_stat ctx =
          (* Check the break, whether it happened, happened - we exit the cycle *)
          if ctx.runtime_signal = WasBreak then
            match loop_stat with
            (* If there was a StatemetntBlock, then we still need to lower the visibility level *)
            | StatementBlock _ ->
                return
                  { ctx with
                    runtime_signal= NoSignal
                  ; count_of_nested_cycles= ctx.count_of_nested_cycles - 1
                  ; visibility_level= ctx.visibility_level - 1 }
            | _ ->
                return
                  { ctx with
                    runtime_signal= NoSignal
                  ; count_of_nested_cycles= ctx.count_of_nested_cycles - 1 }
          else
            eval_expr expr ctx class_table
            >>= fun new_ctx ->
            match new_ctx.last_expr_result with
            | VBool false -> (
              match loop_stat with
              | StatementBlock _ ->
                  return
                    { new_ctx with
                      count_of_nested_cycles= ctx.count_of_nested_cycles - 1
                    ; visibility_level= ctx.visibility_level - 1 }
              | _ ->
                  return
                    { new_ctx with
                      count_of_nested_cycles= ctx.count_of_nested_cycles - 1 } )
            | VBool true -> (
                eval_stat loop_stat new_ctx class_table
                >>= fun loop_ctx ->
                match loop_ctx.runtime_signal with
                (* if we have return  - we interrupt everything and we return the context *)
                | WasReturn -> return loop_ctx
                (* we can have continue - so we cycle again*)
                | WasContinue ->
                    eval_loop loop_stat {loop_ctx with runtime_signal= NoSignal}
                | _ -> eval_loop loop_stat loop_ctx )
            | _ -> error "Incorrect expression type for while stametent" in
        match stat with
        | StatementBlock _ ->
            eval_loop stat
              { in_ctx with
                count_of_nested_cycles= in_ctx.count_of_nested_cycles + 1
              ; visibility_level= in_ctx.visibility_level + 1 }
        | _ ->
            eval_loop stat
              { in_ctx with
                count_of_nested_cycles= in_ctx.count_of_nested_cycles + 1 } )
    | For (stat_opt, expr_opt, after_list, body_stat) ->
        (* With a loop for visibility_level always increases, despite the presence/absence of a body block *)
        ( match stat_opt with
        | None ->
            return {in_ctx with visibility_level= in_ctx.visibility_level + 1}
        | Some dec_stat ->
            eval_stat dec_stat
              {in_ctx with visibility_level= in_ctx.visibility_level + 1}
              class_table )
        >>= fun new_ctx ->
        let delete_loop_vars : context -> context M.t =
         fun ctx ->
          let delete : key_t -> variable -> unit =
           fun key element ->
            if element.visibility_level = ctx.visibility_level then
              Hashtbl.remove ctx.variable_table key in
          Hashtbl.iter delete ctx.variable_table ;
          return ctx in
        let rec eval_loop body_st af_list ctx =
          (* Standard: we look at the result of the boolean expression, if true - calculate
             the body and increments after *)
          ( match expr_opt with
          | None -> return {ctx with last_expr_result= VBool true}
          | Some expr_t -> eval_expr expr_t ctx class_table )
          >>= fun cond_ctx ->
          match cond_ctx.last_expr_result with
          (* If false, it means we are no longer cycling, we return the context with a reduced
             counter of nested_cycle and visibility_level*)
          | VBool false ->
              delete_loop_vars
                { cond_ctx with
                  count_of_nested_cycles= cond_ctx.count_of_nested_cycles - 1
                ; visibility_level= cond_ctx.visibility_level - 1 }
          | VBool true -> (
              let rec interpret_expr_list e_list as_ctx =
                match e_list with
                | [] -> return as_ctx
                | x :: xs ->
                    if expr_in_stat x then
                      eval_expr x as_ctx class_table
                      >>= fun next_ctx -> interpret_expr_list xs next_ctx
                    else error "Incorrect expression for after body list" in
              (* Variables inside the block itself will be in a larger visibility_level than
                 from the initializer *)
              eval_stat body_st
                {cond_ctx with visibility_level= new_ctx.visibility_level + 1}
                class_table
              >>= fun body_ctx ->
              match body_ctx.runtime_signal with
              (* If we have return  - we interrupt everything and we return the context *)
              | WasReturn -> return body_ctx
              (* We can have continue - so we cycle again*)
              | WasContinue ->
                  interpret_expr_list af_list body_ctx
                  >>= fun after_ctx ->
                  eval_loop body_st af_list
                    {after_ctx with runtime_signal= NoSignal}
              (* Check the break, whether it happened, happened - we exit the cycle *)
              | WasBreak ->
                  delete_loop_vars
                    { ctx with
                      runtime_signal= NoSignal
                    ; count_of_nested_cycles= ctx.count_of_nested_cycles - 1
                    ; visibility_level= ctx.visibility_level - 1 }
              | _ ->
                  interpret_expr_list af_list body_ctx
                  >>= fun after_ctx -> eval_loop body_st af_list after_ctx )
          | _ -> error "Incorrect condition type in for statement" in
        eval_loop body_stat after_list
          { new_ctx with
            count_of_nested_cycles= in_ctx.count_of_nested_cycles + 1 }
    | Break ->
        if in_ctx.count_of_nested_cycles <= 0 then
          error "There is no loop to do break"
        else return {in_ctx with runtime_signal= WasBreak}
    | Continue ->
        if in_ctx.count_of_nested_cycles <= 0 then
          error "There is no loop to do continue"
        else return {in_ctx with runtime_signal= WasContinue}
    | Return None when in_ctx.current_method_type = Void ->
        (* If the type is Void, we exit with the Void value set by the signal that was return *)
        return {in_ctx with last_expr_result= VVoid; runtime_signal= WasReturn}
    | Return None -> error "Return value type mismatch"
    | Return (Some expr) ->
        check_expr_type expr in_ctx class_table
        >>= fun ret_type ->
        if ret_type <> in_ctx.current_method_type then
          error "Return value type mismatch"
        else
          (* We return the context in which there is the result of the expression
             and set the signal that was return *)
          eval_expr expr in_ctx class_table
          >>= fun new_ctx -> return {new_ctx with runtime_signal= WasReturn}
    | VarDeclare (modifier, vars_type, var_list) ->
        let is_const : modifier option -> bool = function
          | Some Const -> true
          | _ -> false in
        let get_base_value = function
          | Int -> VInt 0
          | String -> VString ""
          | CsClass _ -> VNull
          | Bool -> VBool false
          | Void -> VVoid in
        let rec interpret_var var_list var_ctx =
          match var_list with
          | [] -> return var_ctx
          | (var_name, var_expr_opt) :: tail ->
              ( match var_expr_opt with
              (* If there is nothing, initialize with the base value *)
              | None ->
                  Hashtbl.add var_ctx.variable_table var_name
                    { var_key= var_name
                    ; var_type= vars_type
                    ; var_value= get_base_value vars_type
                    ; is_const= is_const modifier
                    ; assignment_count= 0
                    ; visibility_level= var_ctx.visibility_level } ;
                  return var_ctx
              (* If there is something, we assign the value calculated on the right *)
              | Some var_expr -> (
                  check_expr_type var_expr var_ctx class_table
                  >>= fun var_expr_type ->
                  (* Add to the context variables table what is in the variable expression on the right *)
                  let add_var new_var =
                    eval_expr new_var var_ctx class_table
                    >>= fun expr_ctx ->
                    Hashtbl.add expr_ctx.variable_table var_name
                      { var_key= var_name
                      ; var_type= var_expr_type
                      ; var_value= expr_ctx.last_expr_result
                      ; is_const= is_const modifier
                      ; assignment_count= 1
                      ; visibility_level= expr_ctx.visibility_level } ;
                    return expr_ctx in
                  match var_expr_type with
                  | _ when var_expr_type = vars_type -> add_var var_expr
                  | _ ->
                      error
                        ( "Incorrect value type for declared variable:"
                        ^ show_types var_expr_type ) ) )
              >>= fun next_ctx -> interpret_var tail next_ctx in
        interpret_var var_list in_ctx
    | Throw expr -> (
        eval_expr expr in_ctx class_table
        >>= fun new_ctx ->
        match new_ctx.last_expr_result with
        | VClass ex_cl -> (
          match ex_cl with
          | ObjNull -> error "NullReferenceException"
          | ObjRef (class_key, parent_key) -> (
            match parent_key with
            | Some "Exception" -> return {new_ctx with runtime_signal= WasThrown}
            | None -> (
              match class_key with
              | "Exception" -> return {new_ctx with runtime_signal= WasThrown}
              | _ ->
                  error
                    "The thrown class is not inherited from Exception class \
                     and not Exception class itself" )
            | _ ->
                error
                  "The thrown class is not inherited from the exception class" )
          )
        | _ -> error "Can't throw exceptions not of type VClass" )
    | Try (try_stat, catch_list, finally_stat_opt) -> (
        let eval_try = function
          | StatementBlock _ ->
              eval_stat try_stat
                {in_ctx with visibility_level= in_ctx.visibility_level + 1}
                class_table
              >>= fun t_ctx ->
              return {t_ctx with visibility_level= t_ctx.visibility_level - 1}
          | _ -> error "Expected { } in try block!" in
        let eval_finally finally_ctx =
          match finally_stat_opt with
          | None -> return finally_ctx
          | Some (StatementBlock _ as finally_stat)
            when finally_ctx.runtime_signal = WasReturn ->
              let return_value = finally_ctx.last_expr_result in
              eval_stat finally_stat
                { finally_ctx with
                  visibility_level= finally_ctx.visibility_level + 1 }
                class_table
              >>= fun f_ctx ->
              return
                { f_ctx with
                  last_expr_result= return_value
                ; visibility_level= f_ctx.visibility_level - 1 }
          | Some (StatementBlock _ as finally_stat) ->
              (*It is important to save the signal so that finally is executed in
                any case, even when catches fail*)
              let saved_signal = finally_ctx.runtime_signal in
              eval_stat finally_stat
                { finally_ctx with
                  runtime_signal= NoSignal
                ; visibility_level= finally_ctx.visibility_level + 1 }
                class_table
              >>= fun f_ctx ->
              return
                { f_ctx with
                  runtime_signal= saved_signal
                ; visibility_level= f_ctx.visibility_level - 1 }
          | _ -> error "Expected { } in finally block!" in
        eval_try try_stat
        >>= fun after_try_ctx ->
        match after_try_ctx.runtime_signal = WasThrown with
        | true ->
            let check_catch_stat = function
              | StatementBlock _ -> return ()
              | _ -> error "Expected { } in catch block!" in
            let eval_catch catch_ctx = function
              (* catch cases:
                 catch {}
                 catch (Exception) {}
              *)
              | None, catch_stat ->
                  check_catch_stat catch_stat
                  >> eval_stat catch_stat
                       { catch_ctx with
                         runtime_signal= NoSignal
                       ; visibility_level= catch_ctx.visibility_level + 1 }
                       class_table
                  >>= fun catch_ctx ->
                  return
                    { catch_ctx with
                      visibility_level= catch_ctx.visibility_level - 1 }
              | Some (CsClass cl_name), catch_stat -> (
                match catch_ctx.last_expr_result with
                | VClass (ObjRef (thrown_name, _)) ->
                    if thrown_name = cl_name || cl_name = "Exception" then
                      check_catch_stat catch_stat
                      >> eval_stat catch_stat
                           { catch_ctx with
                             runtime_signal= NoSignal
                           ; visibility_level= catch_ctx.visibility_level + 1 }
                           class_table
                      >>= fun catch_ctx ->
                      return
                        { catch_ctx with
                          visibility_level= catch_ctx.visibility_level - 1 }
                    else return catch_ctx
                | _ -> error "Incorrect type of result" )
              | _ -> error "Incorrect catch statement" in
            let rec eval_catch_list = function
              | [] -> return after_try_ctx
              | exc :: xs -> (
                  eval_catch after_try_ctx exc
                  >>= fun new_ctx ->
                  match new_ctx.runtime_signal = WasThrown with
                  | false -> return new_ctx
                  | true -> eval_catch_list xs ) in
            eval_catch_list catch_list
            >>= fun after_catches_ctx ->
            eval_finally after_catches_ctx >>= fun end_ctx -> return end_ctx
        | false -> eval_finally after_try_ctx >>= fun end_ctx -> return end_ctx
        )
    | Print print_expr ->
        eval_expr print_expr in_ctx class_table
        >>= fun new_ctx ->
        let eval_printer = function
          | VInt value -> return (printf "%d\n" value)
          | VBool value -> return (printf "%b\n" value)
          | VString value -> return (printf "%s\n" value)
          | VClass value -> (
            match value with
            | ObjNull -> error "NullReferenceException"
            | ObjRef (class_key, _) -> return (printf "%s\n" class_key) )
          | VVoid -> error "void"
          | VNull -> error "null" in
        eval_printer new_ctx.last_expr_result >> return new_ctx

  and eval_expr in_expr in_ctx class_table =
    let eval_helper e_expr ctx =
      match e_expr with
      | Plus (left, right) -> (
          eval_expr left ctx class_table
          >>= fun left_ctx ->
          eval_expr right left_ctx class_table
          >>= fun right_ctx ->
          match (left_ctx.last_expr_result, right_ctx.last_expr_result) with
          | VInt x, VInt y ->
              return {right_ctx with last_expr_result= VInt (x + y)}
          | VString x, VString y ->
              return {right_ctx with last_expr_result= VString (x ^ y)}
          | VInt x, VString y ->
              return
                {right_ctx with last_expr_result= VString (string_of_int x ^ y)}
          | VString x, VInt y ->
              return
                {right_ctx with last_expr_result= VString (x ^ string_of_int y)}
          | _, _ -> error "Incorrect argument types for adding" )
      | Min (left, right) -> (
          eval_expr left ctx class_table
          >>= fun left_ctx ->
          eval_expr right left_ctx class_table
          >>= fun right_ctx ->
          match (left_ctx.last_expr_result, right_ctx.last_expr_result) with
          | VInt x, VInt y ->
              return {right_ctx with last_expr_result= VInt (x - y)}
          | _, _ -> error "Incorrect argument types for subtraction!" )
      | Mul (left, right) -> (
          eval_expr left ctx class_table
          >>= fun left_ctx ->
          eval_expr right left_ctx class_table
          >>= fun right_ctx ->
          match (left_ctx.last_expr_result, right_ctx.last_expr_result) with
          | VInt x, VInt y ->
              return {right_ctx with last_expr_result= VInt (x * y)}
          | _, _ -> error "Incorrect argument types for multiplication!" )
      | Div (left, right) -> (
          eval_expr left ctx class_table
          >>= fun left_ctx ->
          eval_expr right left_ctx class_table
          >>= fun right_ctx ->
          match (left_ctx.last_expr_result, right_ctx.last_expr_result) with
          | VInt _, VInt y when y = 0 -> error "Division by zero!"
          | VInt x, VInt y ->
              return {right_ctx with last_expr_result= VInt (x / y)}
          | _, _ -> error "Incorrect argument types for division!" )
      | Mod (left, right) -> (
          eval_expr left ctx class_table
          >>= fun left_ctx ->
          eval_expr right left_ctx class_table
          >>= fun right_ctx ->
          match (left_ctx.last_expr_result, right_ctx.last_expr_result) with
          | VInt _, VInt y when y = 0 -> error "Division by zero!"
          | VInt x, VInt y ->
              return {right_ctx with last_expr_result= VInt (x mod y)}
          | _, _ -> error "Incorrect argument types for mod operator!" )
      | And (left, right) -> (
          eval_expr left ctx class_table
          >>= fun left_ctx ->
          eval_expr right left_ctx class_table
          >>= fun right_ctx ->
          match (left_ctx.last_expr_result, right_ctx.last_expr_result) with
          | VBool x, VBool y ->
              return {right_ctx with last_expr_result= VBool (x && y)}
          | _, _ -> error "Incorrect types for logical and operator!" )
      | Or (left, right) -> (
          eval_expr left ctx class_table
          >>= fun left_ctx ->
          eval_expr right left_ctx class_table
          >>= fun right_ctx ->
          match (left_ctx.last_expr_result, right_ctx.last_expr_result) with
          | VBool x, VBool y ->
              return {right_ctx with last_expr_result= VBool (x || y)}
          | _, _ -> error "Incorrect types for logical or operator!" )
      | Not not_expr -> (
          eval_expr not_expr ctx class_table
          >>= fun new_ctx ->
          match new_ctx.last_expr_result with
          | VBool x -> return {new_ctx with last_expr_result= VBool (not x)}
          | _ -> error "Incorrect types for logical not operator!" )
      | Less (left, right) -> (
          eval_expr left ctx class_table
          >>= fun left_ctx ->
          eval_expr right left_ctx class_table
          >>= fun right_ctx ->
          match (left_ctx.last_expr_result, right_ctx.last_expr_result) with
          | VInt x, VInt y ->
              return {right_ctx with last_expr_result= VBool (x < y)}
          | _ -> error "Incorrect type for comparison operator!" )
      | More (left, right) -> eval_expr (Less (right, left)) ctx class_table
      | LessOrEqual (left, right) ->
          eval_expr (Not (More (left, right))) ctx class_table
      | MoreOrEqual (left, right) ->
          eval_expr (Not (Less (left, right))) ctx class_table
      | Equal (left, right) -> (
          eval_expr left ctx class_table
          >>= fun left_ctx ->
          eval_expr right left_ctx class_table
          >>= fun right_ctx ->
          match (left_ctx.last_expr_result, right_ctx.last_expr_result) with
          | VInt x, VInt y ->
              return {right_ctx with last_expr_result= VBool (x = y)}
          | VBool x, VBool y ->
              return {right_ctx with last_expr_result= VBool (x = y)}
          | VVoid, VVoid -> return {right_ctx with last_expr_result= VBool true}
          | VString x, VString y ->
              return {right_ctx with last_expr_result= VBool (x = y)}
          | VClass x, VClass y -> (
            match (x, y) with
            | ObjNull, ObjNull ->
                return {right_ctx with last_expr_result= VBool true}
            | ObjNull, _ | _, ObjNull ->
                return {right_ctx with last_expr_result= VBool false}
            | ObjRef (key1, _), ObjRef (key2, _) ->
                return {right_ctx with last_expr_result= VBool (key1 = key2)} )
          | _ -> error "Incorrect types for equality!" )
      | NotEqual (left, right) ->
          eval_expr (Not (Equal (left, right))) ctx class_table
      | ConstExpr value -> return {ctx with last_expr_result= value}
      | IdentVar var_key -> (
        match Hashtbl.find_opt ctx.variable_table var_key with
        | Some var -> return {ctx with last_expr_result= var.var_value}
        | None -> error ("The varibale " ^ var_key ^ " is not found") )
      | Null -> return {ctx with last_expr_result= VClass ObjNull}
      | CallMethod (method_name, args) -> (
          find_main_class class_table
          >>= fun main_class ->
          let check_method_type meth_table meth_name =
            match Hashtbl.find_opt meth_table meth_name with
            | None ->
                error
                  ( "There is no given method " ^ meth_name
                  ^ " in the Main class" )
            | Some method_t -> return method_t in
          check_method_type main_class.method_table method_name
          >>= fun meth ->
          fill_var_table (Hashtbl.create 128) ctx args meth.args class_table
          >>= fun (new_table, new_ctx) ->
          eval_stat meth.body
            { variable_table= new_table
            ; current_method_type= meth.method_type
            ; last_expr_result= VVoid
            ; runtime_signal= NoSignal
            ; count_of_nested_cycles= 0
            ; visibility_level= 0 }
            class_table
          >>= fun res_ctx ->
          match res_ctx.runtime_signal with
          | WasThrown ->
              return
                { new_ctx with
                  last_expr_result= res_ctx.last_expr_result
                ; runtime_signal= WasThrown }
          | _ ->
              return
                { new_ctx with
                  last_expr_result=
                    ( if meth.method_type = Void then VVoid
                    else res_ctx.last_expr_result ) } )
      | Assign (IdentVar var_key, val_expr) -> (
          eval_expr val_expr ctx class_table
          >>= fun assign_ctx ->
          match Hashtbl.find_opt assign_ctx.variable_table var_key with
          | None -> error "Variable not found"
          | Some old_var ->
              check_const_assign_variable old_var
              >>= fun _ ->
              Hashtbl.replace assign_ctx.variable_table var_key
                { old_var with
                  var_value= assign_ctx.last_expr_result
                ; assignment_count= old_var.assignment_count + 1 } ;
              return assign_ctx )
      | PostInc (IdentVar var_key) | PrefInc (IdentVar var_key) ->
          eval_expr
            (Assign
               (IdentVar var_key, Plus (IdentVar var_key, ConstExpr (VInt 1)))
            )
            ctx class_table
      | PostDec (IdentVar var_key) | PrefDec (IdentVar var_key) ->
          eval_expr
            (Assign
               (IdentVar var_key, Min (IdentVar var_key, ConstExpr (VInt 1))) )
            ctx class_table
      | ClassCreate (class_name, _) ->
          let get_elem_ht table key =
            match Hashtbl.find_opt table key with
            | None -> error "The element wasn't found in the table!"
            | Some elem -> return elem in
          get_elem_ht class_table class_name
          >>= fun curr_class ->
          let class_key = curr_class.class_key in
          let parent_key = curr_class.parent_key in
          return
            {ctx with last_expr_result= VClass (ObjRef (class_key, parent_key))}
      | _ -> error "Incorrect expression!" in
    eval_helper in_expr in_ctx

  and fill_var_table ht ctx args meth_args class_table =
    let update_var (var_ht, var_ctx) arg meth_arg =
      match meth_arg with
      | var_type, var_key ->
          eval_expr arg var_ctx class_table
          >>= fun new_ctx ->
          Hashtbl.add var_ht var_key
            { var_type
            ; var_key
            ; is_const= false
            ; assignment_count= 1
            ; var_value= new_ctx.last_expr_result
            ; visibility_level= 0 } ;
          return (var_ht, new_ctx) in
    let rec iter_vars (var_ht, var_ctx) var_args var_meth_args =
      match (var_args, var_meth_args) with
      | [], [] -> return (var_ht, var_ctx)
      | x :: xs, y :: ys ->
          update_var (var_ht, var_ctx) x y
          >>= fun (new_ht, new_ctx) -> iter_vars (new_ht, new_ctx) xs ys
      | _, _ -> error "Incorect var list in the method!" in
    iter_vars (ht, ctx) args meth_args

  and check_const_assign_variable : variable -> unit M.t =
   fun var ->
    match var.assignment_count with
    | 0 -> return ()
    | _ when not var.is_const -> return ()
    | _ -> error "Assigment to a constant variable"

  let start_interpreting : (key_t, class_t) Hashtbl.t -> context M.t =
   fun class_table ->
    find_main_class class_table
    >>= fun main_class ->
    init_contex (Hashtbl.create 32)
    >>= fun ctx ->
    let main = Hashtbl.find main_class.method_table "Main" in
    eval_stat main.body ctx class_table
    >>= fun final_ctx ->
    match final_ctx.runtime_signal = WasThrown with
    | false -> return final_ctx
    | true -> error "Unhandled exception"
end
