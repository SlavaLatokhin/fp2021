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
    { variable_list: variable list
    ; current_method_type: types
    ; last_expr_result: values
    ; runtime_signal: signal
    ; count_of_nested_cycles: int
    ; visibility_level: int
    ; post_inc: key_t list
    ; post_dec: key_t list }
  [@@deriving show {with_path= false}]

  let init_contex variable_list =
    return
      { variable_list
      ; current_method_type= Void
      ; last_expr_result= VVoid
      ; runtime_signal= NoSignal
      ; count_of_nested_cycles= 0
      ; visibility_level= 0
      ; post_inc= []
      ; post_dec= [] }

  let find_main_class (class_list : class_t list) =
    let rec iter_classes = function
      | [] -> error "There is no Main class!"
      | x :: xs -> (
        match find_opt_method x.method_list "Main" with
        | None -> iter_classes xs
        | Some _ -> return x ) in
    iter_classes class_list

  let add_var ctx var = {ctx with variable_list= var :: ctx.variable_list}

  let remove_key_post_inc ctx var_key =
    let check_var_key x = if var_key = x then None else Some x in
    let rec change_var_key il =
      match il with
      | [] -> il
      | x :: xs -> (
        match check_var_key x with
        | None -> change_var_key xs
        | Some el -> el :: change_var_key xs ) in
    {ctx with post_inc= change_var_key ctx.post_inc}

  let remove_key_post_dec ctx var_key =
    let check_var_key x = if var_key = x then None else Some x in
    let rec change_var_key dl =
      match dl with
      | [] -> dl
      | x :: xs -> (
        match check_var_key x with
        | None -> change_var_key xs
        | Some el -> el :: change_var_key xs ) in
    {ctx with post_dec= change_var_key ctx.post_dec}

  let remove_var ctx var_key =
    let check_var_key (x : variable) =
      if var_key = x.var_key then None else Some x in
    let rec change_vars (vl : variable list) =
      match vl with
      | [] -> vl
      | x :: xs -> (
        match check_var_key x with
        | None -> change_vars xs
        | Some el -> el :: change_vars xs ) in
    {ctx with variable_list= change_vars ctx.variable_list}

  let replace_var ctx var_key var =
    let rec change_vars (vl : variable list) =
      match vl with
      | [] -> vl
      | x :: xs ->
          (fun (el : variable) -> if var_key = el.var_key then var else el) x
          :: change_vars xs in
    {ctx with variable_list= change_vars ctx.variable_list}

  let find_opt_var var_list var_key =
    List.find_opt (fun (x : variable) -> var_key = x.var_key) var_list

  let find_method_monad meth_list meth_key =
    match find_opt_method meth_list meth_key with
    | None ->
        error
          (String.concat ""
             ["There is no given method "; meth_key; " in the Main class!"] )
    | Some method_t -> return method_t

  let rec check_expr_type cur_expr ctx class_list =
    match cur_expr with
    | Plus (left, right) -> (
        check_expr_type left ctx class_list
        >>= fun left_type ->
        match left_type with
        | Int -> (
            check_expr_type right ctx class_list
            >>= fun right_type ->
            match right_type with
            | Int -> return Int
            | String -> return String
            | _ ->
                error
                  "Incorrect type: the right side of the expression should be \
                   Int or String!" )
        | String -> (
            check_expr_type right ctx class_list
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
        check_expr_type left ctx class_list
        >>= fun left_type ->
        match left_type with
        | Int -> (
            check_expr_type right ctx class_list
            >>= fun right_type ->
            match right_type with
            | Int -> return Int
            | _ -> error "Incorrect type: the type should be Int!" )
        | _ -> error "Incorrect type: the type should be Int!" )
    | PostDec value | PostInc value | PrefDec value | PrefInc value -> (
        check_expr_type value ctx class_list
        >>= fun value_type ->
        match value_type with
        | Int -> return Int
        | _ -> error "Incorrect type: the type should be Int!" )
    | And (left, right) | Or (left, right) -> (
        check_expr_type left ctx class_list
        >>= fun left_type ->
        match left_type with
        | Bool -> (
            check_expr_type right ctx class_list
            >>= fun right_type ->
            match right_type with
            | Bool -> return Bool
            | _ -> error "Incorrect type: the type should be Bool!" )
        | _ -> error "Incorrect type: the type should be Bool!" )
    | Not value -> (
        check_expr_type value ctx class_list
        >>= fun value_type ->
        match value_type with
        | Bool -> return Bool
        | _ -> error "Incorrect type: the type should be Bool!" )
    | Less (left, right)
     |More (left, right)
     |LessOrEqual (left, right)
     |MoreOrEqual (left, right) -> (
        check_expr_type left ctx class_list
        >>= fun left_type ->
        match left_type with
        | Int -> (
            check_expr_type right ctx class_list
            >>= fun right_type ->
            match right_type with
            | Int -> return Bool
            | _ -> error "Incorrect type: the type should be Int!" )
        | _ -> error "Incorrect type: the type should be Int!" )
    | Equal (left, right) | NotEqual (left, right) -> (
        check_expr_type left ctx class_list
        >>= fun left_type ->
        match left_type with
        | Int -> (
            check_expr_type right ctx class_list
            >>= fun right_type ->
            match right_type with
            | Int -> return Bool
            | _ ->
                error
                  "Incorrect type: the right side of the expression should be \
                   Int!" )
        | String -> (
            check_expr_type right ctx class_list
            >>= fun right_type ->
            match right_type with
            | String -> return Bool
            | _ ->
                error
                  "Incorrect type: the right side of the expression should be \
                   String!" )
        | Bool -> (
            check_expr_type right ctx class_list
            >>= fun right_type ->
            match right_type with
            | Bool -> return Bool
            | _ ->
                error
                  "Incorrect type: the right side of the expression should be \
                   Bool!" )
        | CsClass _ -> (
            check_expr_type right ctx class_list
            >>= fun right_type ->
            match right_type with
            | CsClass _ -> return Bool
            | _ -> error "Incorrect class type!" )
        | _ ->
            error
              "Incorrect type: the type should be Int, String, bool or CSharp \
               class!" )
    | Null -> return (CsClass "null")
    | CallMethod (method_key, _) ->
        find_main_class class_list
        >>= fun curr_class ->
        find_method_monad curr_class.method_list method_key
        >>= fun mth -> return mth.method_type
    | ClassCreate (class_name, _) -> (
      match find_opt_class class_list class_name with
      | None ->
          error (String.concat "" ["Class "; class_name; " wasn't found!"])
      | Some _ -> return (CsClass class_name) )
    | IdentVar var_key -> (
      match find_opt_var ctx.variable_list var_key with
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
        check_expr_type left ctx class_list
        >>= fun left_type ->
        match left_type with
        | Void -> error "incorrect type: cannot assign to void"
        | CsClass left_key -> (
            check_expr_type right ctx class_list
            >>= fun right_type ->
            match right_type with
            | CsClass "null" -> return (CsClass left_key)
            | CsClass right_key -> return (CsClass right_key)
            | _ -> error "Incorrect type assignment!" )
        | _ ->
            check_expr_type right ctx class_list
            >>= fun right_type ->
            if left_type = right_type then return right_type
            else error "Incorrect type assignment!" )

  let expr_in_stat = function
    | PostDec _ | PostInc _ | PrefDec _ | PrefInc _
     |CallMethod (_, _)
     |Assign (_, _) ->
        true
    | _ -> false

  let rec eval_stat stat in_ctx class_list =
    match stat with
    | Expression expr ->
        eval_post_operation in_ctx
        >>= fun in_ctx ->
        if expr_in_stat expr then
          eval_expr expr in_ctx class_list >>= fun new_ctx -> return new_ctx
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
                eval_stat st ctx class_list
                >>= fun head_ctx -> eval_stat_bl tail head_ctx ) in
        eval_stat_bl stat_list in_ctx
        >>= fun new_ctx ->
        eval_post_operation new_ctx >>= fun new_ctx -> return new_ctx
    | If (expr, then_stat, else_stat_opt) -> (
        eval_expr expr in_ctx class_list
        >>= fun if_ctx ->
        eval_post_operation if_ctx
        >>= fun if_ctx ->
        match if_ctx.last_expr_result with
        | VBool true -> (
          match then_stat with
          | StatementBlock _ ->
              eval_stat then_stat
                {if_ctx with visibility_level= if_ctx.visibility_level + 1}
                class_list
              >>= fun in_ctx ->
              return {in_ctx with visibility_level= in_ctx.visibility_level - 1}
          | _ -> eval_stat then_stat if_ctx class_list )
        | VBool false -> (
          match else_stat_opt with
          | Some (StatementBlock _ as else_stat) ->
              eval_stat else_stat
                {if_ctx with visibility_level= if_ctx.visibility_level + 1}
                class_list
              >>= fun else_ctx ->
              return
                {else_ctx with visibility_level= else_ctx.visibility_level - 1}
          | Some else_stat -> eval_stat else_stat if_ctx class_list
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
            eval_expr expr ctx class_list
            >>= fun new_ctx ->
            eval_post_operation new_ctx
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
                eval_stat loop_stat new_ctx class_list
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
              class_list )
        >>= fun new_ctx ->
        eval_post_operation new_ctx
        >>= fun new_ctx ->
        let remove_loop_vars ctx =
          let check_visibility_level (x : variable) =
            if x.visibility_level = ctx.visibility_level then None else Some x
          in
          let rec change_vars (vl : variable list) =
            match vl with
            | [] -> vl
            | xs :: ys -> (
              match check_visibility_level xs with
              | None -> change_vars ys
              | Some x -> x :: change_vars ys ) in
          return {ctx with variable_list= change_vars ctx.variable_list} in
        let rec eval_loop body_st af_list ctx =
          (* Standard: we look at the result of the boolean expression, if true - calculate
             the body and increments after *)
          ( match expr_opt with
          | None -> return {ctx with last_expr_result= VBool true}
          | Some expr_t -> eval_expr expr_t ctx class_list )
          >>= fun cond_ctx ->
          eval_post_operation cond_ctx
          >>= fun cond_ctx ->
          match cond_ctx.last_expr_result with
          (* If false, it means we are no longer cycling, we return the context with a reduced
             counter of nested_cycle and visibility_level*)
          | VBool false ->
              remove_loop_vars
                { cond_ctx with
                  count_of_nested_cycles= cond_ctx.count_of_nested_cycles - 1
                ; visibility_level= cond_ctx.visibility_level - 1 }
          | VBool true -> (
              let rec interpret_expr_list e_list as_ctx =
                match e_list with
                | [] ->
                    eval_post_operation as_ctx >>= fun as_ctx -> return as_ctx
                | x :: xs ->
                    if expr_in_stat x then
                      eval_expr x as_ctx class_list
                      >>= fun next_ctx -> interpret_expr_list xs next_ctx
                    else error "Incorrect expression for after body list" in
              (* Variables inside the block itself will be in a larger visibility_level than
                 from the initializer *)
              eval_stat body_st
                {cond_ctx with visibility_level= new_ctx.visibility_level + 1}
                class_list
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
                  remove_loop_vars
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
        eval_post_operation in_ctx
        >>= fun in_ctx ->
        (* If the type is Void, we exit with the Void value set by the signal that was return *)
        return {in_ctx with last_expr_result= VVoid; runtime_signal= WasReturn}
    | Return None -> error "Return value type mismatch"
    | Return (Some expr) ->
        check_expr_type expr in_ctx class_list
        >>= fun ret_type ->
        if ret_type <> in_ctx.current_method_type then
          error "Return value type mismatch"
        else
          (* We return the context in which there is the result of the expression
             and set the signal that was return *)
          eval_expr expr in_ctx class_list
          >>= fun new_ctx ->
          eval_post_operation new_ctx
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
                  let var =
                    { var_key= var_name
                    ; var_type= vars_type
                    ; var_value= get_base_value vars_type
                    ; is_const= is_const modifier
                    ; assignment_count= 0
                    ; visibility_level= var_ctx.visibility_level } in
                  return (add_var var_ctx var)
              (* If there is something, we assign the value calculated on the right *)
              | Some var_expr -> (
                  check_expr_type var_expr var_ctx class_list
                  >>= fun var_expr_type ->
                  (* Add to the context variables list what is in the variable expression on the right *)
                  match var_expr_type with
                  | _ when var_expr_type = vars_type ->
                      eval_expr var_expr var_ctx class_list
                      >>= fun expr_ctx ->
                      eval_post_operation expr_ctx
                      >>= fun expr_ctx ->
                      let var =
                        { var_key= var_name
                        ; var_type= var_expr_type
                        ; var_value= expr_ctx.last_expr_result
                        ; is_const= is_const modifier
                        ; assignment_count= 1
                        ; visibility_level= expr_ctx.visibility_level } in
                      return (add_var expr_ctx var)
                  | _ ->
                      error
                        ( "Incorrect value type for declared variable:"
                        ^ show_types var_expr_type ) ) )
              >>= fun next_ctx -> interpret_var tail next_ctx in
        interpret_var var_list in_ctx
    | Throw expr -> (
        eval_expr expr in_ctx class_list
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
                class_list
              >>= fun t_ctx ->
              return {t_ctx with visibility_level= t_ctx.visibility_level - 1}
          | _ -> error "Expected { } in try block!" in
        let eval_finally finally_ctx =
          let eval_finally_stat stat class_list =
            eval_stat stat
              { finally_ctx with
                runtime_signal= NoSignal
              ; visibility_level= finally_ctx.visibility_level + 1 }
              class_list
            >>= fun f_ctx -> return f_ctx in
          match finally_stat_opt with
          | None -> return finally_ctx
          | Some (StatementBlock _ as finally_stat)
            when finally_ctx.runtime_signal = WasReturn ->
              let return_value = finally_ctx.last_expr_result in
              eval_finally_stat finally_stat class_list
              >>= fun f_ctx ->
              return
                { f_ctx with
                  runtime_signal= WasReturn
                ; last_expr_result= return_value
                ; visibility_level= f_ctx.visibility_level - 1 }
          | Some (StatementBlock _ as finally_stat) ->
              (*It is important to save the signal so that finally is executed in
                any case, even when catches fail*)
              let saved_signal = finally_ctx.runtime_signal in
              eval_finally_stat finally_stat class_list
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
            let eval_catch_stat catch_ctx catch_stat =
              check_catch_stat catch_stat
              >> eval_stat catch_stat
                   { catch_ctx with
                     runtime_signal= NoSignal
                   ; visibility_level= catch_ctx.visibility_level + 1 }
                   class_list
              >>= fun catch_ctx ->
              return
                {catch_ctx with visibility_level= catch_ctx.visibility_level - 1}
            in
            let eval_catch catch_ctx = function
              (* catch cases:
                 catch {}
                 catch (Exception) {}
              *)
              | None, catch_stat -> eval_catch_stat catch_ctx catch_stat
              | Some (CsClass cl_name), catch_stat -> (
                match catch_ctx.last_expr_result with
                | VClass (ObjRef (thrown_name, _)) ->
                    if thrown_name = cl_name || cl_name = "Exception" then
                      eval_catch_stat catch_ctx catch_stat
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
        eval_expr print_expr in_ctx class_list
        >>= fun new_ctx ->
        eval_post_operation new_ctx
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

  and eval_expr in_expr in_ctx class_list =
    let eval_helper e_expr ctx =
      let eval_left_right left_e right_e eval_f =
        eval_post_operation ctx
        >>= fun ctx ->
        eval_expr left_e ctx class_list
        >>= fun left_ctx ->
        eval_expr right_e left_ctx class_list
        >>= fun right_ctx ->
        try
          let ret_v =
            eval_f left_ctx.last_expr_result right_ctx.last_expr_result in
          return {right_ctx with last_expr_result= ret_v}
        with Invalid_argument m -> error m in
      match e_expr with
      | Plus (left, right) ->
          eval_left_right left right (fun leftv rightv ->
              match (leftv, rightv) with
              | VInt x, VInt y -> VInt (x + y)
              | VString x, VString y -> VString (x ^ y)
              | VInt x, VString y -> VString (string_of_int x ^ y)
              | VString x, VInt y -> VString (x ^ string_of_int y)
              | _, _ ->
                  raise (Invalid_argument "Incorrect argument types for adding") )
      | Min (left, right) ->
          eval_left_right left right (fun leftv rightv ->
              match (leftv, rightv) with
              | VInt x, VInt y -> VInt (x - y)
              | _, _ ->
                  raise
                    (Invalid_argument
                       "Incorrect argument types for subtraction!" ) )
      | Mul (left, right) ->
          eval_left_right left right (fun leftv rightv ->
              match (leftv, rightv) with
              | VInt x, VInt y -> VInt (x * y)
              | _, _ ->
                  raise
                    (Invalid_argument
                       "Incorrect argument types for multiplication!" ) )
      | Div (left, right) ->
          eval_left_right left right (fun leftv rightv ->
              match (leftv, rightv) with
              | VInt _, VInt y when y = 0 ->
                  raise (Invalid_argument "Division by zero!")
              | VInt x, VInt y -> VInt (x / y)
              | _, _ ->
                  raise
                    (Invalid_argument "Incorrect argument types for division!") )
      | Mod (left, right) ->
          eval_left_right left right (fun leftv rightv ->
              match (leftv, rightv) with
              | VInt _, VInt y when y = 0 ->
                  raise (Invalid_argument "Division by zero!")
              | VInt x, VInt y -> VInt (x mod y)
              | _, _ ->
                  raise
                    (Invalid_argument
                       "Incorrect argument types for mod operator!" ) )
      | And (left, right) ->
          eval_left_right left right (fun leftv rightv ->
              match (leftv, rightv) with
              | VBool x, VBool y -> VBool (x && y)
              | _, _ ->
                  raise
                    (Invalid_argument
                       "Incorrect types for logical and operator!" ) )
      | Or (left, right) ->
          eval_left_right left right (fun leftv rightv ->
              match (leftv, rightv) with
              | VBool x, VBool y -> VBool (x || y)
              | _, _ ->
                  raise
                    (Invalid_argument "Incorrect types for logical or operator!") )
      | Not not_expr -> (
          eval_post_operation ctx
          >>= fun ctx ->
          eval_expr not_expr ctx class_list
          >>= fun new_ctx ->
          match new_ctx.last_expr_result with
          | VBool x -> return {new_ctx with last_expr_result= VBool (not x)}
          | _ -> error "Incorrect types for logical not operator!" )
      | Less (left, right) ->
          eval_left_right left right (fun leftv rightv ->
              match (leftv, rightv) with
              | VInt x, VInt y -> VBool (x < y)
              | _ ->
                  raise
                    (Invalid_argument "Incorrect type for comparison operator!") )
      | More (left, right) -> eval_expr (Less (right, left)) ctx class_list
      | LessOrEqual (left, right) ->
          eval_expr (Not (More (left, right))) ctx class_list
      | MoreOrEqual (left, right) ->
          eval_expr (Not (Less (left, right))) ctx class_list
      | Equal (left, right) ->
          eval_left_right left right (fun leftv rightv ->
              match (leftv, rightv) with
              | VInt x, VInt y -> VBool (x = y)
              | VBool x, VBool y -> VBool (x = y)
              | VVoid, VVoid -> VBool true
              | VString x, VString y -> VBool (x = y)
              | VClass x, VClass y -> (
                match (x, y) with
                | ObjNull, ObjNull -> VBool true
                | ObjNull, _ | _, ObjNull -> VBool false
                | ObjRef (key1, _), ObjRef (key2, _) -> VBool (key1 = key2) )
              | _ -> raise (Invalid_argument "Incorrect types for equality!") )
      | NotEqual (left, right) ->
          eval_expr (Not (Equal (left, right))) ctx class_list
      | ConstExpr value ->
          eval_post_operation ctx
          >>= fun ctx -> return {ctx with last_expr_result= value}
      | IdentVar var_key -> (
          eval_post_operation ctx
          >>= fun ctx ->
          match find_opt_var ctx.variable_list var_key with
          | Some var -> return {ctx with last_expr_result= var.var_value}
          | None ->
              error
                (String.concat "" ["The varibale "; var_key; " is not found"]) )
      | Null -> return {ctx with last_expr_result= VClass ObjNull}
      | CallMethod (method_key, args) -> (
          find_main_class class_list
          >>= fun main_class ->
          find_method_monad main_class.method_list method_key
          >>= fun meth ->
          fill_var_list [] ctx args meth.args class_list
          >>= fun (new_var_list, new_ctx) ->
          eval_post_operation new_ctx
          >>= fun new_ctx ->
          eval_stat meth.body
            { variable_list= new_var_list
            ; current_method_type= meth.method_type
            ; last_expr_result= VVoid
            ; runtime_signal= NoSignal
            ; count_of_nested_cycles= 0
            ; visibility_level= 0
            ; post_inc= []
            ; post_dec= [] }
            class_list
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
          eval_post_operation ctx
          >>= fun ctx ->
          eval_expr val_expr ctx class_list
          >>= fun assign_ctx ->
          match find_opt_var assign_ctx.variable_list var_key with
          | None -> error "Variable not found"
          | Some old_var ->
              check_const_assign_variable old_var
              >>= fun _ ->
              let var =
                { old_var with
                  var_value= assign_ctx.last_expr_result
                ; assignment_count= old_var.assignment_count + 1 } in
              return (replace_var assign_ctx var_key var) )
      | PrefInc (IdentVar var_key) ->
          eval_expr
            (Assign
               (IdentVar var_key, Plus (IdentVar var_key, ConstExpr (VInt 1)))
            )
            ctx class_list
      | PrefDec (IdentVar var_key) ->
          eval_expr
            (Assign
               (IdentVar var_key, Min (IdentVar var_key, ConstExpr (VInt 1))) )
            ctx class_list
      | PostInc (IdentVar var_key) ->
          eval_expr
            (Assign
               (IdentVar var_key, Plus (IdentVar var_key, ConstExpr (VInt 0)))
            )
            ctx class_list
          >>= fun ctx -> return {ctx with post_inc= var_key :: ctx.post_inc}
      | PostDec (IdentVar var_key) ->
          eval_expr
            (Assign
               (IdentVar var_key, Min (IdentVar var_key, ConstExpr (VInt 0))) )
            ctx class_list
          >>= fun ctx -> return {ctx with post_dec= var_key :: ctx.post_dec}
      | ClassCreate (class_name, _) ->
          let get_elem_list class_list key =
            match find_opt_class class_list key with
            | None -> error "The element wasn't found in the class list!"
            | Some elem -> return elem in
          get_elem_list class_list class_name
          >>= fun curr_class ->
          let class_key = curr_class.class_key in
          let parent_key = curr_class.parent_key in
          return
            {ctx with last_expr_result= VClass (ObjRef (class_key, parent_key))}
      | _ -> error "Incorrect expression!" in
    eval_helper in_expr in_ctx

  and eval_post_operation ctx =
    let rec eval_post_inc_dec post_ctx inc_v remove_f = function
      | [] -> return post_ctx
      | x :: xs -> (
        match find_opt_var post_ctx.variable_list x with
        | None -> error "Variable not found"
        | Some old_var ->
            check_const_assign_variable old_var
            >>= fun _ ->
            let change_value =
              match old_var.var_value with
              | VInt v -> return (VInt (v + inc_v))
              | _ -> error "Variable in post inc shoud be integer!" in
            change_value
            >>= fun value ->
            let var =
              { old_var with
                var_value= value
              ; assignment_count= old_var.assignment_count + 1 } in
            return (replace_var post_ctx x var)
            >>= fun new_ctx ->
            eval_post_inc_dec (remove_f new_ctx x) inc_v remove_f xs ) in
    eval_post_inc_dec ctx 1 remove_key_post_inc ctx.post_inc
    >>= fun inc_ctx ->
    eval_post_inc_dec inc_ctx (-1) remove_key_post_dec inc_ctx.post_dec
    >>= fun dec_ctx -> return dec_ctx

  and fill_var_list vl ctx args meth_args class_list =
    let update_var var_ctx arg = function
      | var_type, var_key ->
          eval_expr arg var_ctx class_list
          >>= fun new_ctx ->
          let var =
            { var_type
            ; var_key
            ; is_const= false
            ; assignment_count= 1
            ; var_value= new_ctx.last_expr_result
            ; visibility_level= 0 } in
          let add_ctx = add_var new_ctx var in
          return (add_ctx.variable_list, add_ctx) in
    let rec iter_vars (var_list, var_ctx) var_args var_meth_args =
      match (var_args, var_meth_args) with
      | [], [] -> return (var_list, var_ctx)
      | x :: xs, y :: ys ->
          update_var var_ctx x y
          >>= fun (new_vl, new_ctx) -> iter_vars (new_vl, new_ctx) xs ys
      | _, _ -> error "Incorect var list in the method!" in
    iter_vars (vl, ctx) args meth_args

  and check_const_assign_variable : variable -> unit M.t =
   fun var ->
    match var.assignment_count with
    | 0 -> return ()
    | _ when not var.is_const -> return ()
    | _ -> error "Assigment to a constant variable"

  let start_interpreting : class_t list -> context M.t =
   fun class_list ->
    find_main_class class_list
    >>= fun main_class ->
    init_contex []
    >>= fun ctx ->
    let main = find_method main_class.method_list "Main" in
    eval_stat main.body ctx class_list
    >>= fun final_ctx ->
    match final_ctx.runtime_signal = WasThrown with
    | false -> return final_ctx
    | true -> error "Unhandled exception"
end
