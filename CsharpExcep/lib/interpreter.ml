open Ast
open Parser
open Hashtbl_der
open Printf

module type MONAD = sig
  type 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val ( >> ) : 'a t -> 'b t -> 'b t
end

module type MONADERROR = sig
  include MONAD

  val error : string -> 'a t
end

module Result = struct
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind
  let return = Result.ok
  let error = Result.error
  let ( >> ) x f = x >>= fun _ -> f
end

type key_t = string [@@deriving show]

type constructor_t = {key: key_t; args: (types * string) list; body: statement}
[@@deriving show {with_path= false}]

type field_t =
  {field_type: types; field_key: key_t; is_const: bool; sub_tree: expr option}
[@@deriving show {with_path= false}]

type method_t =
  { method_type: types
  ; has_override: bool
  ; has_static_mod: bool
  ; method_key: key_t
  ; args: (types * string) list
  ; body: statement }
[@@deriving show {with_path= false}]

type class_t =
  { class_key: key_t
  ; field_table: (key_t, field_t) Hashtbl_der.t
  ; method_table: (key_t, method_t) Hashtbl_der.t
  ; constructor_table: (key_t, constructor_t) Hashtbl_der.t
  ; parent_key: key_t option
  ; children_keys: key_t list
  ; dec_tree: class_dec }
[@@deriving show {with_path= false}]

let startswith all_str sub_str =
  if String.length sub_str > String.length all_str then false
  else
    let sub = String.sub all_str 0 (String.length sub_str) in
    String.equal sub sub_str

let convert_table_to_seq = Hashtbl.to_seq_values

let seq_hd_exn s =
  match s () with Seq.Nil -> raise Not_found | Seq.Cons (x, _) -> x

let get_type_list = List.map fst

let make_method_key m_name args =
  String.concat "" (m_name :: List.map show_types (get_type_list args))

let make_constructor_key cl args =
  String.concat "" (cl :: List.map show_types (get_type_list args))

let to_string_key = make_method_key "ToString" []

module ClassLoader (M : MONADERROR) = struct
  open M

  let is_public = List.mem Public
  let is_override = List.mem Override
  let is_static = List.mem Static
  let is_const = List.mem Const

  let get_elem ht key =
    match Hashtbl_der.get_elem_if_present ht key with
    | None -> error "The element wasn't found in the table!"
    | Some elem -> return elem

  let monadic_update_hash_table ht key value =
    Hashtbl.replace ht key value ;
    return ht

  let rec monadic_list_iter list action base =
    match list with
    | [] -> return base
    | x :: xs -> action x >> monadic_list_iter xs action base

  let rec monadic_seq_iter seq action base =
    match seq () with
    | Seq.Nil -> return base
    | Seq.Cons (x, del_tail) ->
        action x >> monadic_seq_iter del_tail action base

  let system_exception_init ht =
    let constructor_table = Hashtbl.create 16 in
    let field_table = Hashtbl.create 16 in
    let method_table = Hashtbl.create 16 in
    let get_body =
      match
        apply_parser Statement.statement_block
          {|
                {
                  return message;
                }
            |}
      with
      | None -> error "Error in parsing ToString method in Exception class"
      | Some bd -> return bd in
    get_body
    >>= fun body ->
    let to_string : method_t =
      { method_type= String
      ; has_override= true
      ; has_static_mod= false
      ; method_key= to_string_key
      ; args= []
      ; body } in
    let message : field_t =
      {field_type= String; field_key= "message"; is_const= false; sub_tree= None}
    in
    let get_dec =
      match
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
      with
      | None -> error "Error in parsing Exception class"
      | Some tree -> return tree in
    get_dec
    >>= fun dec_tree ->
    Hashtbl.add method_table to_string_key to_string ;
    Hashtbl.add field_table "message" message ;
    Hashtbl.add ht "Exception"
      { class_key= "Exception"
      ; field_table
      ; method_table
      ; constructor_table
      ; parent_key= None
      ; children_keys= []
      ; dec_tree } ;
    return ht

  (*A function for checking fields, methods, and constructors for incorrect modifiers*)
  let check_modifiers_field pair =
    match pair with
    | l, f -> (
      match f with
      | Method (Void, "Main", [], _)
        when is_static l && (not (is_const l)) && not (is_override l) ->
          return ()
      | Method (_, "Main", _, _) ->
          error "Only one main method can be in program!"
      | Method (_, _, _, _) when is_const l -> error "Method can not be const"
      | Method (_, _, _, _) -> return ()
      | VariableField (_, _) when (not (is_static l)) && not (is_override l) ->
          return ()
      | VariableField (_, _) -> error "Wrong modifiers"
      | Constructor (_, _, _)
        when (not (is_static l))
             && (not (is_const l))
             && (not (is_override l))
             && is_public l ->
          return ()
      | Constructor (_, _, _) -> error "Wrong constructor modifiers" )

  (*A function for checking a class for the presence of delusional modifiers*)
  let check_modifiers_class = function
    | Class (ml, _, _, _)
      when (not (is_static ml)) && (not (is_override ml)) && not (is_const ml)
      ->
        return ()
    | Class (_, _, _, _) -> error "Wrong class modifiers"

  let add_default_constructor ht =
    Hashtbl.iter
      (fun key some_class ->
        if Hashtbl.length some_class.constructor_table = 0 then
          let cl_key = make_constructor_key key [] in
          Hashtbl.add some_class.constructor_table cl_key
            {key= cl_key; args= []; body= StatementBlock []} )
      ht ;
    return ht

  let class_adding class_list hastable =
    (*function for adding to a table with a check for existence*)
    let add_with_check ht key value message =
      match get_elem_if_present ht key with
      | None -> Hashtbl.add ht key value ; return ht
      | _ -> error message in
    let add_class_table ht adding_class =
      match adding_class with
      | Class (_, class_key, parent, fields) ->
          (* Initialize tables *)
          let method_table = Hashtbl.create 1024 in
          let field_table = Hashtbl.create 1024 in
          let constructor_table = Hashtbl.create 1024 in
          check_modifiers_class adding_class
          >>
          (* Function of adding a class element to the corresponding table *)
          let add_class_elem : modifier list * field -> unit M.t =
           fun field_elem ->
            match field_elem with
            | mod_list, VariableField (field_type, arg_list) ->
                let rec helper_add_var = function
                  | [] -> return ()
                  | (field_key, sub_tree) :: ps ->
                      let is_const = is_const mod_list in
                      add_with_check field_table field_key
                        {field_type; field_key; is_const; sub_tree}
                        "Similar fields"
                      >> helper_add_var ps in
                check_modifiers_field field_elem >> helper_add_var arg_list
            | mod_list, Method (method_type, m_name, args, body) ->
                let method_key = make_method_key m_name args in
                let has_override = is_override mod_list in
                let has_static_mod = is_static mod_list in
                check_modifiers_field field_elem
                >> add_with_check method_table method_key
                     { method_type
                     ; has_override
                     ; has_static_mod
                     ; method_key
                     ; args
                     ; body }
                     "Method with this type exists"
                >> return ()
            | _, Constructor (name, args, body) ->
                let constr_key = make_constructor_key name args in
                let check_name =
                  if name = class_key then return ()
                  else error "Constructor name error" in
                check_name
                >> check_modifiers_field field_elem
                >> add_with_check constructor_table constr_key
                     {key= constr_key; args; body}
                     "Constructor with this type exists"
                >> return () in
          let add_parent p = match p with None -> None | _ -> p in
          let parent_key = add_parent parent in
          monadic_list_iter fields add_class_elem ()
          >> add_with_check ht class_key
               { class_key
               ; field_table
               ; method_table
               ; constructor_table
               ; parent_key
               ; children_keys= []
               ; dec_tree= adding_class }
               "Similar Classes" in
    monadic_list_iter class_list (add_class_table hastable) hastable

  let update_exception_class_childs ht =
    let update_helper : class_t -> class_t M.t =
     fun some_class ->
      match some_class.parent_key with
      (* There is no parent key - go ahead *)
      | None -> return some_class
      (* If there is, we are trying to get the parent by the key
         (or crash with an error), if it is possible to inherit,
          we update the hash table *)
      | Some key -> (
          let parent = get_elem_if_present ht key in
          let exception_is_parent = String.compare "Exception" in
          match parent with
          | None ->
              error "The class can only be inherited from the Exception class!"
          | Some parent_o when exception_is_parent parent_o.class_key = 0 ->
              let new_val =
                { parent_o with
                  children_keys= some_class.class_key :: parent_o.children_keys
                } in
              monadic_update_hash_table ht key new_val >> return new_val
          | Some _ ->
              error "The class can only be inherited from the Exception class!"
          ) in
    monadic_seq_iter (convert_table_to_seq ht) update_helper ht

  (*Small functions for processing individual parts for transfert*)
  (* Processing of the parent field *)
  let transfer_fields parent children =
    let exception_transfer_field : class_t -> field_t -> unit t =
     fun child_class p_field ->
      (* See if there is such a field in the child's table*)
      match get_elem_if_present child_class.field_table p_field.field_key with
      (* No - just adding a child to the table *)
      | None ->
          return (Hashtbl.add child_class.field_table p_field.field_key p_field)
      (* There is - well, okay, skip *)
      | _ -> return () in
    monadic_seq_iter
      (convert_table_to_seq parent.field_table)
      (exception_transfer_field children)
      ()

  (* Method transfer*)
  let transfer_methods parent children =
    let exception_transfer_method : class_t -> method_t -> unit t =
     fun child_class p_method ->
      match
        get_elem_if_present child_class.method_table p_method.method_key
      with
      | None ->
          return
            (Hashtbl.add child_class.method_table p_method.method_key p_method)
      | _ -> return () in
    monadic_seq_iter
      (convert_table_to_seq parent.method_table)
      (exception_transfer_method children)
      ()

  (* Checking that the child's override modifiers are only for overridden methods*)
  let check_override_mod parent children =
    let check : class_t -> method_t -> unit t =
     fun parent child_method ->
      match child_method.has_override with
      (* No annotation - skip *)
      | false -> return ()
      (* If there is - see if there is such a method in the parent, if there is
         - everything is ok, if not - an error *)
      | true -> (
        match
          get_elem_if_present parent.method_table child_method.method_key
        with
        | None ->
            error "Not overriden method or parent does not exist this method!"
        | _ -> return () ) in
    monadic_seq_iter
      (convert_table_to_seq children.method_table)
      (check parent) ()

  (* A separate function that takes parent and child,
     the parent's properties are passed to the child with the necessary checks*)
  let transfer_to_child parent children =
    transfer_fields parent children
    >> transfer_methods parent children
    >> check_override_mod parent children

  let begin_inheritance_from_exception ht =
    get_elem ht "Exception"
    >>= fun ex_cl ->
    let helper child_key =
      get_elem ht child_key >>= fun child_c -> transfer_to_child ex_cl child_c
    in
    monadic_list_iter ex_cl.children_keys helper ht

  let load_classes class_list class_table =
    match class_list with
    | [] -> error "No class found, syntax error or empty file"
    | _ ->
        system_exception_init class_table
        >>= fun table ->
        class_adding class_list table
        >>= fun table_with_classes ->
        add_default_constructor table_with_classes
        >>= fun table_update ->
        update_exception_class_childs table_update
        >>= fun new_table -> begin_inheritance_from_exception new_table
end

module Interpreter (M : MONADERROR) = struct
  open M

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

  type context =
    { current_o: object_references
    ; variable_table: (key_t, variable) Hashtbl_der.t
    ; current_method_type: types
    ; last_expr_result: values
    ; runtime_signal: signal
    ; is_main: bool
    ; curr_constructor: key_t option
    ; count_of_nested_cycle: int
    ; visibility_level: int
    ; prev_ctx: context option
    ; count_of_obj: int
    ; is_creation: bool }
  [@@deriving show {with_path= false}]

  let init_contex current_o variable_table =
    return
      { current_o
      ; variable_table
      ; current_method_type= Void
      ; last_expr_result= VVoid
      ; runtime_signal= NoSignal
      ; is_main= true
      ; curr_constructor= None
      ; count_of_nested_cycle= 0
      ; visibility_level= 0
      ; prev_ctx= None
      ; count_of_obj= 0
      ; is_creation= false }

  let rec fold_left2 func acc l1 l2 =
    match (l1, l2) with
    | [], [] -> return acc
    | x :: xs, y :: ys -> func acc x y >>= fun res -> fold_left2 func res xs ys
    | _, _ -> error "Wrong lists for fold_left2"

  let get_elem ht key =
    match Hashtbl_der.get_elem_if_present ht key with
    | None -> error "The element wasn't found in the table!"
    | Some elem -> return elem

  let find_main_class ht =
    Hashtbl_der.filter ht (fun _ cl -> Hashtbl.mem cl.method_table "Main")
    |> fun filter_hasht ->
    match Hashtbl.length filter_hasht with
    | 0 -> error "There is no main method"
    | 1 -> return (seq_hd_exn (convert_table_to_seq filter_hasht))
    | _ -> error "Must be one main method"

  let obj_num obj =
    try get_obj_num obj |> fun n -> return n
    with Invalid_argument m -> error m

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
            | String -> return String (*for example: string a = 11 + "abc";*)
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
        | CsClass left_name -> (
            check_expr_type right ctx class_table
            >>= fun right_type ->
            match right_type with
            | CsClass right_name when left_name = right_name -> return Bool
            | CsClass "null" -> return Bool
            | _ -> error "Incorrect class type!" )
        | _ ->
            error
              "Incorrect type: the type should be Int, String, bool or CSharp \
               class!" )
    | Null -> return (CsClass "null")
    | CallMethod (method_name, args) ->
        let obj_key =
          match ctx.current_o with
          | ObjNull -> "null"
          | ObjRef {class_key= key; _} -> key in
        get_elem class_table obj_key
        >>= fun curr_class ->
        check_method_type curr_class method_name args ctx class_table
        >>= fun t_mth -> return t_mth.method_type
    | Access (calling_obj, IdentVar var_name) -> (
        check_expr_type calling_obj ctx class_table
        >>= fun obj_type ->
        match obj_type with
        | CsClass "null" -> error "NullReferenceException"
        | CsClass obj_key -> (
            get_elem class_table obj_key
            >>= fun found_obj ->
            let find_field =
              get_elem_if_present found_obj.field_table var_name in
            match find_field with
            | None -> error "No matching field found"
            | Some found_field -> return found_field.field_type )
        | _ -> error "Invalid type: type must be reference" )
    | Access (calling_obj, CallMethod (m_name, args)) -> (
        check_expr_type calling_obj ctx class_table
        >>= fun obj_type ->
        match obj_type with
        | CsClass "null" -> error "NullReferenceException"
        | CsClass obj_key ->
            get_elem class_table obj_key
            >>= fun found_obj ->
            check_method_type found_obj m_name args ctx class_table
            >>= fun found_mth -> return found_mth.method_type
        | _ -> error "Invalid type: type must be reference" )
    | ClassCreate (class_name, args) -> (
      match get_elem_if_present class_table class_name with
      | None -> error ("Class not found" ^ class_name ^ "\n")
      | Some t_class -> (
        match args with
        | [] -> return (CsClass class_name)
        | _ ->
            check_constructor_type t_class args ctx class_table
            >> return (CsClass class_name) ) )
    | IdentVar var_key -> (
        let find_variable = get_elem_if_present ctx.variable_table var_key in
        match find_variable with
        | None -> (
          match ctx.current_o with
          | ObjRef {class_table= table; _} -> (
            match get_elem_if_present table var_key with
            | None -> error "There is no such variable or field with that name!"
            | Some field -> return field.field_type )
          | ObjNull -> error "NullReferenceException" )
        | Some variable -> return variable.var_type )
    | ConstExpr value -> (
      match value with
      | VInt _ -> return Int
      | VBool _ -> return Bool
      | VString _ -> return String
      | VClass ObjNull -> return (CsClass "null")
      | VClass (ObjRef {class_key= key; _}) -> return (CsClass key)
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
            | CsClass right_key ->
                check_assign_types left_key right_key class_table
            | _ -> error "Incorrect type assignment!" )
        | _ ->
            check_expr_type right ctx class_table
            >>= fun right_type ->
            if left_type = right_type then return right_type
            else error "Incorrect type assignment!" )
    | _ -> error "Incorrect expression"

  and check_assign_types left_key right_key class_table =
    match classname_verify_polymorphic left_key right_key class_table with
    | false -> error "Incorrect assign type"
    | true -> return (CsClass right_key)

  and check_constructor_type t_class args g_ctx class_table =
    let check_type : int -> types -> key_t -> constructor_t -> bool =
     fun position curr_type _ value ->
      match List.nth_opt value.args position with
      | None -> false
      | Some (n_type, _) -> (
        match curr_type with
        | CsClass "null" -> (
          match n_type with CsClass _ -> true | _ -> false )
        | CsClass cl_key -> (
          match n_type with
          | CsClass this_key ->
              classname_verify_polymorphic this_key cl_key class_table
          | _ -> false )
        | _ -> n_type = curr_type ) in
    let rec check_helper ht pos expr_list ctx =
      match Hashtbl.length ht with
      | 0 -> error "Constructor not found"
      | other -> (
        match expr_list with
        | [] -> (
          match other with
          | 1 -> return (seq_hd_exn (convert_table_to_seq ht))
          | _ -> error "Constructor not recognized" )
        | x :: xs ->
            check_expr_type x ctx class_table
            >>= fun x_type ->
            check_helper
              (Hashtbl_der.filter ht (check_type pos x_type))
              (pos + 1) xs ctx ) in
    check_helper
      (Hashtbl_der.filter t_class.constructor_table (fun _ cr ->
           List.length cr.args = List.length args ) )
      0 args g_ctx

  and classname_verify_polymorphic left_key right_key class_table =
    let rec check_par key =
      match get_elem_if_present class_table key with
      | None -> false
      | Some get_p -> (
          if get_p.class_key = left_key then true
          else
            match get_p.parent_key with
            | None -> false
            | Some pk -> check_par pk ) in
    check_par right_key

  and check_method_type t_class m_name args g_ctx class_table =
    let check_type : int -> types -> key_t -> method_t -> bool =
     fun position curr_type _ value ->
      match List.nth_opt value.args position with
      | None -> false
      | Some (f_type, _) -> (
        match curr_type with
        | CsClass "null" -> (
          match f_type with CsClass _ -> true | _ -> false )
        | CsClass cl_key -> (
          match f_type with
          | CsClass this_key ->
              classname_verify_polymorphic this_key cl_key class_table
          | _ -> false )
        | _ -> f_type = curr_type ) in
    let rec helper ht pos expr_list ctx =
      match Hashtbl.length ht with
      | 0 -> error "Method not found"
      | other -> (
        match expr_list with
        | [] -> (
          match other with
          | 1 -> return (seq_hd_exn (convert_table_to_seq ht))
          (* There are more than one methods with exactly the same or polymorphic types.
             Means cannot resolve *)
          | _ -> error "Cannot resolve method" )
        | x :: xs ->
            check_expr_type x ctx class_table
            >>= fun x_type ->
            helper
              (Hashtbl_der.filter ht (check_type pos x_type))
              (pos + 1) xs ctx ) in
    (* First we filter by name, and then by the number of arguments *)
    Hashtbl_der.filter t_class.method_table (fun _ mr ->
        startswith mr.method_key m_name )
    |> fun filter_name ->
    helper
      (Hashtbl_der.filter filter_name (fun _ mr ->
           List.length mr.args = List.length args ) )
      0 args g_ctx

  let make_list_of_elem el =
    let rec helper acc curr =
      match curr with 0 -> acc | x -> helper (el :: acc) (x - 1) in
    helper []

  let inc_visibility_level ctx =
    {ctx with visibility_level= ctx.visibility_level + 1}

  let dec_visibility_level ctx =
    {ctx with visibility_level= ctx.visibility_level - 1}

  let delete_var_visibility : context -> context M.t =
   fun ctx ->
    let delete : key_t -> variable -> unit =
     fun key element ->
      if element.visibility_level = ctx.visibility_level then
        Hashtbl.remove ctx.variable_table key in
    Hashtbl.iter delete ctx.variable_table ;
    return ctx

  let check_assign_field : field_references -> unit M.t =
   fun field ->
    match field.assignment_count with
    | 0 -> return ()
    | _ when not field.is_const -> return ()
    | _ -> error "Assigment to a constant field"

  let check_assign_variable : variable -> unit M.t =
   fun var ->
    match var.assignment_count with
    | 0 -> return ()
    | _ when not var.is_const -> return ()
    | _ -> error "Assigment to a constant variable"

  let expr_in_stat = function
    | PostDec _ | PostInc _ | PrefDec _ | PrefInc _
     |CallMethod (_, _)
     |Access (_, CallMethod (_, _))
     |Assign (_, _) ->
        true
    | _ -> false

  let rec eval_stat stat input_ctx class_table =
    match stat with
    | StatementBlock stat_list ->
        let rec eval_stat_bl : statement list -> context -> context M.t =
         fun stl hctx ->
          match stl with
          | [] -> return hctx
          | st :: tail -> (
            match st with
            | (Break | Continue | Return _) when tail <> [] ->
                error "Unreachable code"
            | _
              when hctx.count_of_nested_cycle >= 1
                   && hctx.runtime_signal = WasBreak ->
                return hctx
            | _
              when hctx.count_of_nested_cycle >= 1
                   && hctx.runtime_signal = WasContinue ->
                return hctx
            | _ when hctx.runtime_signal = WasReturn -> return hctx
            | _ when hctx.runtime_signal = WasThrown -> return hctx
            | _ ->
                eval_stat st hctx class_table
                >>= fun head_ctx -> eval_stat_bl tail head_ctx ) in
        eval_stat_bl stat_list input_ctx
        >>= fun new_ctx ->
        if new_ctx.is_main then return new_ctx
        else delete_var_visibility new_ctx
    | While (smexpr, stat) -> (
        let was_main = input_ctx.is_main in
        let rec loop l_stat ctx =
          (* Immediately check the break, whether it happened, happened - we exit the cycle *)
          if ctx.runtime_signal = WasBreak then
            match l_stat with
            (* If there was a StatemetntBlock, then we still need to lower the visibility level *)
            | StatementBlock _ ->
                return
                  (dec_visibility_level
                     { ctx with
                       runtime_signal= NoSignal
                     ; count_of_nested_cycle= ctx.count_of_nested_cycle - 1 } )
            | _ ->
                return
                  { ctx with
                    runtime_signal= NoSignal
                  ; count_of_nested_cycle= ctx.count_of_nested_cycle - 1 }
          else
            eval_expr smexpr ctx class_table
            >>= fun new_ctx ->
            match new_ctx.last_expr_result with
            | VBool false -> (
              match l_stat with
              | StatementBlock _ ->
                  return
                    (dec_visibility_level
                       { new_ctx with
                         count_of_nested_cycle= ctx.count_of_nested_cycle - 1
                       ; is_main= was_main } )
              | _ ->
                  return
                    { new_ctx with
                      count_of_nested_cycle= ctx.count_of_nested_cycle - 1 } )
            | VBool true -> (
                eval_stat l_stat new_ctx class_table
                >>= fun l_ctx ->
                match l_ctx.runtime_signal with
                (* if we have return  - we interrupt everything and we return the context *)
                | WasReturn -> return l_ctx
                (* we can have continue - so we cycle again*)
                | WasContinue ->
                    loop l_stat {l_ctx with runtime_signal= NoSignal}
                | _ -> loop l_stat l_ctx )
            | _ -> error "Incorrect expression type for while stametent" in
        match stat with
        | StatementBlock _ ->
            loop stat
              (inc_visibility_level
                 { input_ctx with
                   count_of_nested_cycle= input_ctx.count_of_nested_cycle + 1
                 ; is_main= false } )
        | _ ->
            loop stat
              { input_ctx with
                count_of_nested_cycle= input_ctx.count_of_nested_cycle + 1 } )
    | Break ->
        if input_ctx.count_of_nested_cycle <= 0 then
          error "There is no loop to do break"
        else return {input_ctx with runtime_signal= WasBreak}
    | Continue ->
        if input_ctx.count_of_nested_cycle <= 0 then
          error "There is no loop to do continue"
        else return {input_ctx with runtime_signal= WasContinue}
    | If (smexpr, then_stat, else_stat) -> (
        eval_expr smexpr input_ctx class_table
        >>= fun new_ctx ->
        let was_main = new_ctx.is_main in
        match new_ctx.last_expr_result with
        | VBool true -> (
          match then_stat with
          | StatementBlock _ ->
              eval_stat then_stat
                (inc_visibility_level {new_ctx with is_main= false})
                class_table
              >>= fun in_ctx ->
              return (dec_visibility_level {in_ctx with is_main= was_main})
          | _ -> eval_stat then_stat new_ctx class_table )
        | VBool false -> (
          match else_stat with
          | Some (StatementBlock _ as else_st) ->
              eval_stat else_st
                (inc_visibility_level {new_ctx with is_main= false})
                class_table
              >>= fun el_ctx ->
              return (dec_visibility_level {el_ctx with is_main= was_main})
          | Some else_st -> eval_stat else_st new_ctx class_table
          | None -> return input_ctx )
        | _ -> error "Incorrect type for condition statement" )
    | For (dec_stat_o, smexpr_o, after_list, body_stat_o) ->
        (* With a loop for visibility_level always increases, despite the presence/absence of a body block *)
        let was_main = input_ctx.is_main in
        ( match dec_stat_o with
        | None -> return (inc_visibility_level {input_ctx with is_main= false})
        | Some dec_stat ->
            eval_stat dec_stat
              (inc_visibility_level {input_ctx with is_main= false})
              class_table )
        >>= fun new_ctx ->
        let rec loop body_stat af_list ctx =
          (* Immediately check the break, whether it happened, happened - we exit the cycle *)
          if ctx.runtime_signal = WasBreak then
            delete_var_visibility
              { ctx with
                runtime_signal= NoSignal
              ; count_of_nested_cycle= ctx.count_of_nested_cycle - 1
              ; visibility_level= ctx.visibility_level - 1
              ; is_main= was_main }
          else
            (* Standard: we look at the result of the boolean expression, if true - calculate
               the body and increments after *)
            ( match smexpr_o with
            | None -> return {ctx with last_expr_result= VBool true}
            | Some expr_t -> eval_expr expr_t ctx class_table )
            >>= fun cond_ctx ->
            match cond_ctx.last_expr_result with
            (* If false, it means we are no longer cycling, we return the context with a reduced
               counter of nested_cycle and visibility_level*)
            | VBool false ->
                delete_var_visibility
                  { cond_ctx with
                    count_of_nested_cycle= cond_ctx.count_of_nested_cycle - 1
                  ; visibility_level= cond_ctx.visibility_level - 1
                  ; is_main= was_main }
            | VBool true -> (
                let rec inter_expr_list e_list as_ctx =
                  match e_list with
                  | [] -> return as_ctx
                  | x :: xs ->
                      if expr_in_stat x then
                        eval_expr x as_ctx class_table
                        >>= fun z_ctx -> inter_expr_list xs z_ctx
                      else error "Incorrect expression for after body list"
                in
                (* Variables inside the block itself will be in a larger visibility_level than
                   from the initializer *)
                eval_stat body_stat
                  {cond_ctx with visibility_level= new_ctx.visibility_level + 1}
                  class_table
                >>= fun body_ctx ->
                match body_ctx.runtime_signal with
                (* if we have return  - we interrupt everything and we return the context *)
                | WasReturn -> return {body_ctx with is_main= was_main}
                (* we can have continue - so we cycle again*)
                | WasContinue ->
                    inter_expr_list af_list body_ctx
                    >>= fun after_ctx ->
                    loop body_stat af_list
                      {after_ctx with runtime_signal= NoSignal}
                | _ ->
                    inter_expr_list af_list body_ctx
                    >>= fun after_ctx -> loop body_stat af_list after_ctx )
            | _ -> error "Incorrect condition type in for statement" in
        loop body_stat_o after_list
          { new_ctx with
            count_of_nested_cycle= input_ctx.count_of_nested_cycle + 1 }
    | Return None when input_ctx.current_method_type = Void ->
        (* If the type is Void, we exit with the Void value set by the signal that was return *)
        return
          {input_ctx with last_expr_result= VVoid; runtime_signal= WasReturn}
    | Return None -> error "Return value type mismatch"
    | Return (Some rexpr) ->
        check_expr_type rexpr input_ctx class_table
        >>= fun ret_type ->
        if ret_type <> input_ctx.current_method_type then
          error "Return value type mismatch"
        else
          (* We return the context in which there is the result of the expression,
             but do not forget to set the signal that was return *)
          eval_expr rexpr input_ctx class_table
          >>= fun new_ctx -> return {new_ctx with runtime_signal= WasReturn}
    | Expression sexpr ->
        if expr_in_stat sexpr then
          eval_expr sexpr input_ctx class_table
          >>= fun new_ctx -> return new_ctx
        else error "Incorrect expression for statement"
    | VarDeclare (modifier, vars_type, var_list) ->
        let is_const : modifier option -> bool = function
          | Some Const -> true
          | _ -> false in
        let rec var_declarator var_list var_ctx =
          match var_list with
          | [] -> return var_ctx
          | (var_name, var_expr_o) :: tail -> (
            match var_ctx.current_o with
            | ObjNull -> error "NullReferenceException"
            | ObjRef {class_table= table; _} ->
                ( if
                  (* We make sure that there is no such name either among local variables
                     or among class fields *)
                  Hashtbl.mem var_ctx.variable_table var_name
                  || Hashtbl.mem table var_name
                then error "Variable with this name is already defined"
                else
                  match var_expr_o with
                  (* If there is nothing, initialize with the base value *)
                  | None ->
                      Hashtbl.add var_ctx.variable_table var_name
                        { var_key= var_name
                        ; var_type= vars_type
                        ; var_value= get_default_value vars_type
                        ; is_const= is_const modifier
                        ; assignment_count= 0
                        ; visibility_level= var_ctx.visibility_level } ;
                      return var_ctx
                  (* If there is something, we assign the value calculated on the right *)
                  | Some var_expr_e -> (
                      check_expr_type var_expr_e var_ctx class_table
                      >>= fun var_expr_type ->
                      (* Add to the context variables table what is in the variable expression on the right *)
                      let add_var new_var =
                        eval_expr new_var var_ctx class_table
                        >>= fun ctx_aft_ad ->
                        Hashtbl.add ctx_aft_ad.variable_table var_name
                          { var_key= var_name
                          ; var_type= var_expr_type
                          ; var_value= ctx_aft_ad.last_expr_result
                          ; is_const= is_const modifier
                          ; assignment_count= 1
                          ; visibility_level= ctx_aft_ad.visibility_level } ;
                        return ctx_aft_ad in
                      match var_expr_type with
                      (* Null can be assigned to any object *)
                      | CsClass "null" -> (
                        match vars_type with
                        | CsClass _ -> add_var var_expr_e
                        | _ ->
                            error
                              "Incorrect assign type in variable declaration" )
                      (* If the type on the right is a class, then we need to check the
                         type, observing inheritance *)
                      | CsClass class_right -> (
                        match vars_type with
                        | CsClass class_left ->
                            check_assign_types class_left class_right
                              class_table
                            (* The type will be checked normally - then just add *)
                            >>= fun _ -> add_var var_expr_e
                        | _ ->
                            error
                              "Incorrect assign type in variable declaration" )
                      | _ when var_expr_type = vars_type -> add_var var_expr_e
                      | _ ->
                          error
                            ( "Incorrect value type for declared variable:"
                            ^ show_types var_expr_type ) ) )
                >>= fun head_ctx -> var_declarator tail head_ctx ) in
        var_declarator var_list input_ctx
    | Throw sexpr -> (
        eval_expr sexpr input_ctx class_table
        >>= fun new_ctx ->
        match new_ctx.last_expr_result with
        | VClass ex_cl -> (
          match ex_cl with
          | ObjNull -> error "NullReferenceException"
          | ObjRef ex_obj -> (
            match ex_obj.parent_key with
            | Some "Exception" -> return {new_ctx with runtime_signal= WasThrown}
            | None -> (
              match ex_obj.class_key with
              | "Exception" -> return {new_ctx with runtime_signal= WasThrown}
              | _ -> error "Cannot implicitly convert type to System.Exception"
              )
            | _ -> error "Cannot implicitly convert type to System.Exception" )
          )
        | _ -> error "Can't throw exceptions not of type VClass" )
    | Try (try_stat, catch_list, finally_stat_o) -> (
        let was_main = input_ctx.is_main in
        let eval_try = function
          | StatementBlock _ ->
              eval_stat try_stat
                (inc_visibility_level {input_ctx with is_main= false})
                class_table
              >>= fun t_ctx ->
              return (dec_visibility_level {t_ctx with is_main= was_main})
          | _ -> error "Expected { } in try block!" in
        let eval_finally finally_ctx =
          match finally_stat_o with
          | None -> return finally_ctx
          | Some (StatementBlock _ as finally_stat)
            when finally_ctx.runtime_signal = WasReturn ->
              let get_return_v = finally_ctx.last_expr_result in
              eval_stat finally_stat
                (inc_visibility_level {finally_ctx with is_main= false})
                class_table
              >>= fun f_ctx ->
              return
                (dec_visibility_level
                   {f_ctx with is_main= was_main; last_expr_result= get_return_v} )
          | Some (StatementBlock _ as finally_stat) ->
              let save_flag = finally_ctx.runtime_signal in
              eval_stat finally_stat
                (inc_visibility_level
                   {finally_ctx with is_main= false; runtime_signal= NoSignal} )
                class_table
              >>= fun f_ctx ->
              return
                (dec_visibility_level
                   {f_ctx with is_main= was_main; runtime_signal= save_flag} )
          | _ -> error "Expected { } in finally block!" in
        eval_try try_stat
        >>= fun after_try_ctx ->
        match after_try_ctx.runtime_signal = WasThrown with
        | true ->
            let check_catch_stat = function
              | StatementBlock _ -> return ()
              | _ -> error "Expected { } in catch block!" in
            (* We consider all the cases that were in the parser*)
            let eval_catch eval_ctx = function
              (*catch cases:
                catch {}
                catch (Exception) {}
                catch (Exception ex) {}
              *)
              | None, None, catch_stat ->
                  check_catch_stat catch_stat
                  >> eval_stat catch_stat
                       (inc_visibility_level
                          { eval_ctx with
                            runtime_signal= NoSignal
                          ; is_main= false } )
                       class_table
                  >>= fun catch_ctx ->
                  return
                    (dec_visibility_level {catch_ctx with is_main= was_main})
              | Some (CsClass cl_name, None), None, catch_stat -> (
                match eval_ctx.last_expr_result with
                | VClass (ObjRef {class_key= throw_name; _}) ->
                    if throw_name = cl_name || cl_name = "Exception" then
                      check_catch_stat catch_stat
                      >> eval_stat catch_stat
                           (inc_visibility_level
                              { eval_ctx with
                                runtime_signal= NoSignal
                              ; is_main= false } )
                           class_table
                      >>= fun catch_ctx ->
                      return
                        (dec_visibility_level
                           {catch_ctx with is_main= was_main} )
                    else return eval_ctx
                | _ -> error "Incorrect type of result" )
              | ( Some (CsClass cl_name, Some (IdentVar th_ex_name))
                , None
                , catch_stat ) -> (
                match eval_ctx.last_expr_result with
                | VClass (ObjRef {class_key= throw_name; _}) ->
                    if throw_name = cl_name || cl_name = "Exception" then
                      let adder =
                        Hashtbl.add eval_ctx.variable_table th_ex_name
                          { var_key= th_ex_name
                          ; var_type= CsClass cl_name
                          ; var_value= eval_ctx.last_expr_result
                          ; is_const= false
                          ; assignment_count= 0
                          ; visibility_level= eval_ctx.visibility_level + 1 } ;
                        return eval_ctx in
                      adder
                      >>= fun adder_ctx ->
                      check_catch_stat catch_stat
                      >> eval_stat catch_stat
                           (inc_visibility_level
                              { adder_ctx with
                                runtime_signal= NoSignal
                              ; is_main= false } )
                           class_table
                      >>= fun catch_ctx ->
                      return
                        (dec_visibility_level
                           {catch_ctx with is_main= was_main} )
                    else return eval_ctx
                | _ -> error "Incorrect type of result" )
              | _ -> error "Incorrect catch statement" in
            let rec eval_catch_list = function
              | [] -> return after_try_ctx
              | x :: xs -> (
                  eval_catch after_try_ctx x
                  >>= fun new_ctx ->
                  match new_ctx.runtime_signal = WasThrown with
                  | false -> return new_ctx
                  | true -> eval_catch_list xs ) in
            eval_catch_list catch_list
            >>= fun handle_ctx ->
            eval_finally handle_ctx >>= fun end_ctx -> return end_ctx
        | false -> eval_finally after_try_ctx >>= fun end_ctx -> return end_ctx
        )
    | Print print_expr ->
        eval_expr print_expr input_ctx class_table
        >>= fun new_ctx ->
        let printer = function
          | VInt value -> return (printf "%d\n" value)
          | VBool value -> return (printf "%b\n" value)
          | VString value -> return (printf "%s\n" value)
          | VClass value -> (
            match value with
            | ObjNull -> error "NullReferenceException"
            | ObjRef ob -> return (printf "%s\n" ob.class_key) )
          | VVoid -> error "Impossible to print void"
          | VNull -> error "Impossible to print null" in
        printer new_ctx.last_expr_result >> return new_ctx

  and eval_expr in_expr in_ctx class_table =
    let eval_helper e_expr ctx =
      let eval_op left_expr right_expr operator =
        eval_expr left_expr ctx class_table
        >>= fun left_ctx ->
        eval_expr right_expr left_ctx class_table
        >>= fun right_ctx ->
        let get_left_value = left_ctx.last_expr_result in
        let get_right_value = right_ctx.last_expr_result in
        match operator get_left_value get_right_value with
        | cal_value -> return {right_ctx with last_expr_result= cal_value}
        | exception Invalid_argument m -> error m
        | exception Division_by_zero -> error "Division by zero!" in
      let eval_unar ex_operand operator =
        eval_expr ex_operand ctx class_table
        >>= fun new_ctx ->
        let get_value = new_ctx.last_expr_result in
        match operator get_value with
        | cal_unar_v -> return {new_ctx with last_expr_result= cal_unar_v}
        | exception Invalid_argument m -> error m in
      let ( ++ ) left right =
        match (left, right) with
        | VInt x, VInt y -> VInt (x + y)
        | VString x, VString y -> VString (x ^ y)
        | VInt x, VString y -> VString (string_of_int x ^ y)
        | VString x, VInt y -> VString (x ^ string_of_int y)
        | _, _ -> raise (Invalid_argument "Incorrect argument types for adding")
      in
      let ( -- ) left right =
        match (left, right) with
        | VInt x, VInt y -> VInt (x - y)
        | _, _ ->
            raise (Invalid_argument "Incorrect argument types for subtraction!")
      in
      let ( ** ) left right =
        match (left, right) with
        | VInt x, VInt y -> VInt (x * y)
        | _, _ ->
            raise
              (Invalid_argument "Incorrect argument types for multiplication!")
      in
      let ( // ) left right =
        match (left, right) with
        | VInt _, VInt y when y = 0 -> raise Division_by_zero
        | VInt x, VInt y -> VInt (x / y)
        | _, _ ->
            raise (Invalid_argument "Incorrect argument types for division!")
      in
      let ( %% ) left right =
        match (left, right) with
        | VInt _, VInt y when y = 0 -> raise Division_by_zero
        | VInt x, VInt y -> VInt (x mod y)
        | _, _ ->
            raise (Invalid_argument "Incorrect argument types for mod operator!")
      in
      let ( >>> ) left right =
        match (left, right) with
        | VInt x, VInt y -> VBool (x > y)
        | _ -> raise (Invalid_argument "Incorrect type for >!") in
      let ( <<< ) left right =
        match (left, right) with
        | VInt x, VInt y -> VBool (x < y)
        | _ -> raise (Invalid_argument "Incorrect type for <!") in
      let ( <<== ) left right =
        match (left, right) with
        | VInt x, VInt y -> VBool (x <= y)
        | _ -> raise (Invalid_argument "Incorrect type for <=!") in
      let ( >>== ) left right =
        match (left, right) with
        | VInt x, VInt y -> VBool (x >= y)
        | _ -> raise (Invalid_argument "Incorrect type for >=!") in
      let ( &&& ) left right =
        match (left, right) with
        | VBool x, VBool y -> VBool (x && y)
        | _, _ -> raise (Invalid_argument "Incorrect types for && operator!")
      in
      let ( ||| ) left right =
        match (left, right) with
        | VBool x, VBool y -> VBool (x || y)
        | _, _ -> raise (Invalid_argument "Incorrect types for || operator!")
      in
      let not_op = function
        | VBool x -> VBool (not x)
        | _ -> raise (Invalid_argument "Incorrect types for NOT operator!")
      in
      let ( === ) left right =
        match (left, right) with
        | VInt x, VInt y -> VBool (x = y)
        | VBool x, VBool y -> VBool (x = y)
        | VVoid, VVoid -> VBool true
        | VString s, VString t -> VBool (s = t)
        | VClass x, VClass y -> (
          match (x, y) with
          | ObjNull, ObjNull -> VBool true
          | ObjNull, _ | _, ObjNull -> VBool false
          | ObjRef {number= xn; _}, ObjRef {number= yn; _} -> VBool (xn = yn) )
        | _ -> raise (Invalid_argument "Incorrect types for equality!") in
      let ( !=! ) left right = not_op (left === right) in
      match e_expr with
      | Plus (left, right) -> eval_op left right ( ++ )
      | Min (left, right) -> eval_op left right ( -- )
      | Mul (left, right) -> eval_op left right ( ** )
      | Div (left, right) -> eval_op left right ( // )
      | Mod (left, right) -> eval_op left right ( %% )
      | And (left, right) -> eval_op left right ( &&& )
      | Or (left, right) -> eval_op left right ( ||| )
      | Not not_exp -> eval_unar not_exp not_op
      | Less (left, right) -> eval_op left right ( <<< )
      | More (left, right) -> eval_op left right ( >>> )
      | LessOrEqual (left, right) -> eval_op left right ( <<== )
      | MoreOrEqual (left, right) -> eval_op left right ( >>== )
      | Equal (left, right) -> eval_op left right ( === )
      | NotEqual (left, right) -> eval_op left right ( !=! )
      | ConstExpr value -> return {ctx with last_expr_result= value}
      | IdentVar var_id -> (
        match get_elem_if_present ctx.variable_table var_id with
        | Some id -> return {ctx with last_expr_result= id.var_value}
        | None -> (
          try
            get_obj_info ctx.current_o
            |> fun (_, table, _) ->
            match get_elem_if_present table var_id with
            | Some field -> return {ctx with last_expr_result= field.field_value}
            | None -> error "Field not found"
          with Failure m | Invalid_argument m -> error m ) )
      | Null -> return {ctx with last_expr_result= VClass ObjNull}
      | Access (obj_expr, IdentVar field_key) -> (
          eval_expr obj_expr ctx class_table
          >>= fun ob_ctx ->
          let get_obj = ob_ctx.last_expr_result in
          match get_obj with
          | VClass (ObjRef {class_table= table; _}) ->
              get_elem table field_key
              >>= fun get_field ->
              return {ob_ctx with last_expr_result= get_field.field_value}
          | _ -> error "Cannot access a field of non-reference type" )
      | Access (obj_expr, CallMethod (m_name, args)) -> (
          eval_expr obj_expr ctx class_table
          >>= fun ob_ctx ->
          match ob_ctx.last_expr_result with
          | VClass ObjNull -> error "NullReferenceException"
          | VClass
              (ObjRef
                { class_key= cl_k
                ; class_table= table
                ; number= num
                ; parent_key= par_k } ) -> (
            match get_elem_if_present class_table cl_k with
            | None -> error "Class not found"
            | Some found_class -> (
                check_method_type found_class m_name args ob_ctx class_table
                >>= fun meth ->
                let create_var_table : (key_t, variable) Hashtbl_der.t =
                  Hashtbl.create 128 in
                ( try
                    update_table create_var_table args meth.args ob_ctx
                      class_table
                  with Invalid_argument m -> error m )
                >>= fun (new_table, new_ctx) ->
                eval_stat meth.body
                  { current_o=
                      ObjRef
                        { class_key= cl_k
                        ; class_table= table
                        ; number= num
                        ; parent_key= par_k }
                  ; variable_table= new_table
                  ; current_method_type= meth.method_type
                  ; last_expr_result= VVoid
                  ; runtime_signal= NoSignal
                  ; is_main= false
                  ; curr_constructor= None
                  ; count_of_nested_cycle= 0
                  ; visibility_level= 0
                  ; prev_ctx= Some ctx
                  ; count_of_obj= ctx.count_of_obj
                  ; is_creation= false }
                  class_table
                >>= fun res_ctx ->
                match res_ctx.runtime_signal with
                | WasThrown ->
                    return
                      { new_ctx with
                        last_expr_result= res_ctx.last_expr_result
                      ; count_of_obj= res_ctx.count_of_obj
                      ; runtime_signal= WasThrown
                      ; is_creation= false }
                | _ ->
                    return
                      { new_ctx with
                        last_expr_result=
                          ( if meth.method_type = Void then VVoid
                          else res_ctx.last_expr_result )
                      ; count_of_obj= res_ctx.count_of_obj
                      ; is_creation= false } ) )
          | _ -> error "Cannot access a field of non-reference type" )
      | CallMethod (m_name, args) -> (
          let get_curr_o =
            return {ctx with last_expr_result= VClass ctx.current_o} in
          get_curr_o
          >>= fun ob_ctx ->
          match ob_ctx.last_expr_result with
          | VClass ObjNull -> error "NullReferenceException"
          | VClass
              (ObjRef
                { class_key= cl_k
                ; class_table= table
                ; number= num
                ; parent_key= par_k } ) -> (
            match get_elem_if_present class_table cl_k with
            | None -> error "Class not found"
            | Some found_class -> (
                check_method_type found_class m_name args ob_ctx class_table
                >>= fun meth ->
                let create_var_table : (key_t, variable) Hashtbl_der.t =
                  Hashtbl.create 128 in
                ( try
                    update_table create_var_table args meth.args ob_ctx
                      class_table
                  with Invalid_argument m -> error m )
                >>= fun (new_table, new_ctx) ->
                eval_stat meth.body
                  { current_o=
                      ObjRef
                        { class_key= cl_k
                        ; class_table= table
                        ; number= num
                        ; parent_key= par_k }
                  ; variable_table= new_table
                  ; current_method_type= meth.method_type
                  ; last_expr_result= VVoid
                  ; runtime_signal= NoSignal
                  ; is_main= false
                  ; curr_constructor= None
                  ; count_of_nested_cycle= 0
                  ; visibility_level= 0
                  ; prev_ctx= Some ctx
                  ; count_of_obj= ctx.count_of_obj
                  ; is_creation= false }
                  class_table
                >>= fun res_ctx ->
                match res_ctx.runtime_signal with
                | WasThrown ->
                    return
                      { new_ctx with
                        last_expr_result= res_ctx.last_expr_result
                      ; count_of_obj= res_ctx.count_of_obj
                      ; runtime_signal= WasThrown
                      ; is_creation= false }
                | _ ->
                    return
                      { new_ctx with
                        last_expr_result=
                          ( if meth.method_type = Void then VVoid
                            (*чтобы не сохранять в last_expr_result значение не типа VVoid, если метод Void  *)
                          else res_ctx.last_expr_result )
                      ; count_of_obj= res_ctx.count_of_obj
                      ; is_creation= false } ) )
          | _ -> error "Cannot access a field of non-reference type" )
      | Assign (IdentVar var_key, val_expr) ->
          eval_expr val_expr ctx class_table
          >>= fun eval_ctx ->
          update_identifier var_key eval_ctx.last_expr_result eval_ctx
      | Assign (Access (obj, IdentVar field_name), val_expr) ->
          eval_expr val_expr ctx class_table
          >>= fun eval_ctx -> update_field obj field_name eval_ctx class_table
      | PostInc (Access (obj, IdentVar field))
       |PrefInc (Access (obj, IdentVar field)) ->
          eval_expr
            (Assign
               ( Access (obj, IdentVar field)
               , Plus (Access (obj, IdentVar field), ConstExpr (VInt 1)) ) )
            ctx class_table
      | PostInc (IdentVar var_key) | PrefInc (IdentVar var_key) ->
          eval_expr
            (Assign
               (IdentVar var_key, Plus (IdentVar var_key, ConstExpr (VInt 1)))
            )
            ctx class_table
      | PostDec (Access (obj_expr, IdentVar field))
       |PrefDec (Access (obj_expr, IdentVar field)) ->
          eval_expr
            (Assign
               ( Access (obj_expr, IdentVar field)
               , Min (Access (obj_expr, IdentVar field), ConstExpr (VInt 1)) )
            )
            ctx class_table
      | PostDec (IdentVar var_key) | PrefDec (IdentVar var_key) ->
          eval_expr
            (Assign
               (IdentVar var_key, Min (IdentVar var_key, ConstExpr (VInt 1))) )
            ctx class_table
      | ClassCreate (class_name, c_args) ->
          get_elem class_table class_name
          >>= fun get_obj ->
          check_constructor_type get_obj c_args ctx class_table
          >>= fun found_constr ->
          let rec create_obj par_class init_ctx =
            let field_pairs = Ast.get_field_pairs par_class.dec_tree in
            let rec create_helper inhelp_ht rec_ctx = function
              | [] -> return rec_ctx
              | (cur_field_type, field_name, field_expr_o) :: tail ->
                  let is_const f_key =
                    get_elem get_obj.field_table f_key
                    >>= fun find_t_field -> return find_t_field.is_const in
                  ( match field_expr_o with
                  | Some f_expr -> (
                      check_expr_type f_expr rec_ctx class_table
                      >>= fun expr_type ->
                      is_const field_name
                      >>= fun const_res ->
                      let add_field fe =
                        eval_expr fe rec_ctx class_table
                        >>= fun fe_ctx ->
                        Hashtbl.add inhelp_ht field_name
                          { key= field_name
                          ; field_type= cur_field_type
                          ; field_value= fe_ctx.last_expr_result
                          ; is_const= const_res
                          ; assignment_count= 1 } ;
                        return (fe_ctx, inhelp_ht) in
                      match expr_type with
                      | CsClass "null" -> (
                        match cur_field_type with
                        | CsClass _ -> add_field f_expr
                        | _ -> error "Incorrect type of variable assignment" )
                      | CsClass cright -> (
                        match cur_field_type with
                        | CsClass cleft ->
                            check_assign_types cleft cright class_table
                            >>= fun _ -> add_field f_expr
                        | _ -> error "Incorrect type of variable assignment" )
                      | _ when expr_type = cur_field_type -> add_field f_expr
                      | _ -> error "Incorrect type of variable assignment" )
                  | None ->
                      is_const field_name
                      >>= fun const_res ->
                      Hashtbl.add inhelp_ht field_name
                        { key= field_name
                        ; field_type= cur_field_type
                        ; field_value= get_default_value cur_field_type
                        ; is_const= const_res
                        ; assignment_count= 0 } ;
                      return (rec_ctx, inhelp_ht) )
                  >>= fun (head_ctx, head_ht) ->
                  obj_num head_ctx.current_o
                  >>= fun num ->
                  create_helper head_ht
                    { head_ctx with
                      current_o=
                        ObjRef
                          { class_key= class_name
                          ; parent_key= get_obj.parent_key
                          ; class_table= head_ht
                          ; number= num } }
                    tail in
            match par_class.parent_key with
            | None -> create_helper (Hashtbl.create 128) init_ctx field_pairs
            | Some par_key ->
                get_elem class_table par_key
                >>= fun parent_r ->
                create_obj parent_r init_ctx
                >>= fun par_ctx ->
                create_helper
                  (get_obj_fields par_ctx.current_o)
                  par_ctx field_pairs in
          let new_object =
            ObjRef
              { class_key= class_name
              ; parent_key= get_obj.parent_key
              ; class_table= Hashtbl.create 100
              ; number= ctx.count_of_obj + 1 } in
          create_obj get_obj
            { current_o= new_object
            ; variable_table= Hashtbl.create 100
            ; last_expr_result= VVoid
            ; runtime_signal= NoSignal
            ; current_method_type= Void
            ; is_main= false
            ; count_of_nested_cycle= 0
            ; visibility_level= 0
            ; prev_ctx= Some ctx
            ; count_of_obj= ctx.count_of_obj + 1
            ; curr_constructor= None
            ; is_creation= false }
          >>= fun initres_ctx ->
          let get_new_var_table =
            try
              update_table (Hashtbl.create 128) c_args found_constr.args ctx
                class_table
            with Invalid_argument m -> error m in
          get_new_var_table
          >>= fun (vt, _) ->
          prepare_constructor_block found_constr.body get_obj
          >>= fun c_body ->
          eval_stat c_body
            { initres_ctx with
              variable_table= vt
            ; is_creation= true
            ; is_main= false
            ; curr_constructor= Some found_constr.key }
            class_table
          >>= fun c_ctx ->
          return
            { ctx with
              last_expr_result= VClass c_ctx.current_o
            ; runtime_signal= NoSignal
            ; count_of_obj= c_ctx.count_of_obj }
      | _ -> error "Incorrect expression!" in
    check_expr_type in_expr in_ctx class_table
    >>= fun _ -> eval_helper in_expr in_ctx

  (* At the same time, we run through two lists: the list of expressions passed
     to the method and the list of parameters in the method entry of the class.*)
  and update_table ht args meth_args ctx class_table =
    fold_left2
      (fun (new_ht, hctx) arg pair ->
        match pair with
        | h_type, h_name ->
            eval_expr arg hctx class_table
            >>= fun new_ctx ->
            Hashtbl.add new_ht h_name
              { var_type= h_type
              ; var_key= h_name
              ; is_const= false
              ; assignment_count= 1
              ; var_value= new_ctx.last_expr_result
              ; visibility_level= 0 } ;
            return (new_ht, new_ctx) )
      (ht, ctx) args meth_args

  and update_identifier var_key value var_ctx =
    if Hashtbl.mem var_ctx.variable_table var_key then (
      get_elem var_ctx.variable_table var_key
      >>= fun get_old ->
      check_assign_variable get_old
      >>= fun _ ->
      Hashtbl.replace var_ctx.variable_table var_key
        { get_old with
          var_value= value
        ; assignment_count= get_old.assignment_count + 1 } ;
      return var_ctx )
    else
      match var_ctx.current_o with
      | ObjNull -> error "NullReferenceException"
      | ObjRef {class_table= table; _} ->
          if Hashtbl.mem table var_key then
            get_elem table var_key
            >>= fun get_old ->
            check_assign_field get_old
            >>= fun _ ->
            if var_ctx.is_creation then
              Hashtbl.replace table var_key
                {get_old with field_value= var_ctx.last_expr_result}
              |> fun _ -> return var_ctx
            else
              try
                update_object var_ctx.current_o var_key var_ctx.last_expr_result
                  var_ctx
                |> fun _ -> return var_ctx
              with
              | Invalid_argument m -> error m
              | Not_found -> error "Variable not found"
          else error "Variable not found"

  and update_field obj_expr field_name field_ctx class_table =
    eval_expr obj_expr field_ctx class_table
    >>= fun obj_evaled_ctx ->
    let get_obj = get_obj_value obj_evaled_ctx.last_expr_result in
    let cal_new_val = field_ctx.last_expr_result in
    try
      get_obj_info get_obj
      |> fun (_, fl_table, _) ->
      if Hashtbl.mem fl_table field_name then
        get_elem fl_table field_name
        >>= fun old_field ->
        check_assign_field old_field
        >>= fun _ ->
        if obj_evaled_ctx.is_creation then
          Hashtbl.replace fl_table field_name
            {old_field with field_value= field_ctx.last_expr_result}
          |> fun _ -> return obj_evaled_ctx
        else
          update_object get_obj field_name cal_new_val obj_evaled_ctx
          |> fun _ -> return obj_evaled_ctx
      else error "Field not found in class!"
    with
    | Invalid_argument m | Failure m -> error m
    | Not_found -> error "Field not found!"

  and update_object obj field_key value up_ctx =
    let rec refresh field_hashtable f_key new_val ref_num count_assign =
      Hashtbl.iter
        (fun _ field_ref ->
          match field_ref with
          | {field_value= f_val; _} -> (
            match f_val with
            | VClass (ObjRef {class_table= fl_table; number= fnum; _}) ->
                ( if fnum = ref_num then
                  match get_elem_if_present fl_table f_key with
                  | None -> raise Not_found
                  | Some old_field ->
                      Hashtbl.replace fl_table f_key
                        { old_field with
                          field_value= new_val
                        ; assignment_count= count_assign } ) ;
                refresh fl_table f_key new_val ref_num count_assign
            | _ -> () ) )
        field_hashtable in
    let rec helper_update f_key new_val upd_ctx up_num assign_cnt =
      Hashtbl.iter
        (fun _ var ->
          match var.var_value with
          | VClass (ObjRef {class_table= field_table; number= fnum; _}) ->
              if up_num = fnum then (
                match get_elem_if_present field_table f_key with
                | None -> raise Not_found
                | Some old_field ->
                    Hashtbl.replace field_table f_key
                      { old_field with
                        field_value= new_val
                      ; assignment_count= assign_cnt } ;
                    refresh field_table f_key new_val up_num assign_cnt )
              else refresh field_table f_key new_val up_num assign_cnt
          | _ -> () )
        upd_ctx.variable_table
      |> fun () ->
      match upd_ctx.prev_ctx with
      | None -> ()
      | Some prev_ctx -> helper_update f_key new_val prev_ctx up_num assign_cnt
    in
    get_obj_info obj
    |> fun (_, object_frt, object_number) ->
    ( match get_elem_if_present object_frt field_key with
    | None -> raise Not_found
    | Some f_assign -> f_assign.assignment_count + 1 )
    |> fun assign_cnt ->
    helper_update field_key value up_ctx object_number assign_cnt

  and prepare_constructor_block curr_body curr_class =
    match (curr_body, curr_class.parent_key) with
    | StatementBlock _, _ -> return curr_body
    | _ -> error "Must be statement block!"

  let start_interpreting : (key_t, class_t) Hashtbl.t -> context M.t =
   fun ht ->
    find_main_class ht
    >>= fun main_class ->
    init_contex
      (ObjRef
         { class_key= main_class.class_key
         ; parent_key= None
         ; class_table= Hashtbl.create 32
         ; number= 0 } )
      (Hashtbl.create 32)
    >>= fun ctx ->
    let main = Hashtbl.find main_class.method_table "Main" in
    eval_stat main.body ctx ht
    >>= fun final_ctx ->
    match final_ctx.runtime_signal = WasThrown with
    | false -> return final_ctx
    | true -> error "Unhandled exception"
end
