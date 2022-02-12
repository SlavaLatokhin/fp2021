open Ast

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

module Interpret_classes (M : MONADERROR) = struct
  open M
  open Value_types

  let get_elem_ht t k =
    match Hashtbl.find_opt t k with
    | None -> error "The element wasn't found in the table!"
    | Some elem -> return elem

  let rec monadic_list_iter list action base =
    match list with
    | [] -> return base
    | x :: xs -> action x >> monadic_list_iter xs action base

  let monadic_update_hash_table ht key value =
    Hashtbl.replace ht key value ;
    return ht

  let rec monadic_seq_iter seq action base =
    match seq () with
    | Seq.Nil -> return base
    | Seq.Cons (x, del_tail) ->
        action x >> monadic_seq_iter del_tail action base

  let system_exception_init class_table =
    let constructor_table = Hashtbl.create 16 in
    let field_table = Hashtbl.create 16 in
    let method_table = Hashtbl.create 16 in
    let body = StatementBlock [Return (Some (IdentVar "message"))] in
    let to_string : method_t =
      { method_type= String
      ; has_override= true
      ; has_static_mod= false
      ; method_key=
          String.concat "" ("ToString" :: List.map show_types (List.map fst []))
      ; args= []
      ; body } in
    let message : field_t =
      {field_type= String; field_key= "message"; is_const= false; sub_tree= None}
    in
    let dec_tree =
      Class
        ( [Public]
        , "Exception"
        , None
        , [ ([Public], VariableField (String, [("message", None)]))
          ; ( [Public]
            , Method
                ( String
                , "ToString"
                , []
                , StatementBlock [Return (Some (IdentVar "message"))] ) ) ] )
    in
    Hashtbl.add method_table
      (String.concat "" ("ToString" :: List.map show_types (List.map fst [])))
      to_string ;
    Hashtbl.add field_table "message" message ;
    Hashtbl.add class_table "Exception"
      { class_key= "Exception"
      ; field_table
      ; method_table
      ; constructor_table
      ; parent_key= None
      ; children_keys= []
      ; dec_tree } ;
    return class_table

  let add_default_constructor class_table =
    Hashtbl.iter
      (fun key some_class ->
        if Hashtbl.length some_class.constructor_table = 0 then
          let cl_key =
            String.concat "" (key :: List.map show_types (List.map fst []))
          in
          Hashtbl.add some_class.constructor_table cl_key
            {key= cl_key; args= []; body= StatementBlock []} )
      class_table ;
    return class_table

  let class_adding class_list class_table =
    (* function for adding to a table with a check for existence*)
    let add_with_check ht key value message =
      match Hashtbl.find_opt ht key with
      | None -> Hashtbl.add ht key value ; return ht
      | _ -> error message in
    let add_class_table ht adding_class =
      match adding_class with
      | Class (_, class_key, parent, fields) ->
          (* Initialize tables *)
          let method_table = Hashtbl.create 1024 in
          let field_table = Hashtbl.create 1024 in
          let constructor_table = Hashtbl.create 1024 in
          (* Function of adding a class element to the corresponding table *)
          let add_class_elem : modifier list * field -> unit M.t =
           fun field_elem ->
            match field_elem with
            | mod_list, VariableField (field_type, arg_list) ->
                let rec helper_add_var = function
                  | [] -> return ()
                  | (field_key, sub_tree) :: ps ->
                      let is_const = List.mem Const mod_list in
                      add_with_check field_table field_key
                        {field_type; field_key; is_const; sub_tree}
                        ( "The field with this key: " ^ field_key
                        ^ " already exists" )
                      >> helper_add_var ps in
                helper_add_var arg_list
            | mod_list, Method (method_type, m_name, args, body) ->
                let method_key =
                  String.concat ""
                    (m_name :: List.map show_types (List.map fst args)) in
                let has_override = List.mem Override mod_list in
                let has_static_mod = List.mem Static mod_list in
                add_with_check method_table method_key
                  { method_type
                  ; has_override
                  ; has_static_mod
                  ; method_key
                  ; args
                  ; body }
                  ("The method with this key: " ^ method_key ^ " already exists")
                >> return ()
            | _, Constructor (name, args, body) ->
                let constr_key =
                  String.concat ""
                    (name :: List.map show_types (List.map fst args)) in
                let check_name =
                  if name = class_key then return ()
                  else error "Constructor name error" in
                check_name
                >> add_with_check constructor_table constr_key
                     {key= constr_key; args; body}
                     ( "The constructor with this key: " ^ constr_key
                     ^ " already exists" )
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
               ("The class with this key: " ^ class_key ^ " already exists")
    in
    monadic_list_iter class_list (add_class_table class_table) class_table

  let inherit_exception_class class_table =
    let update_helper : class_t -> class_t M.t =
     fun some_class ->
      match some_class.parent_key with
      (* There is no parent key - go ahead *)
      | None -> return some_class
      (* If there is, we are trying to get the parent by the key
         (or crash with an error), if it is possible to inherit,
          we update the hash table *)
      | Some key -> (
          let parent = Hashtbl.find_opt class_table key in
          match parent with
          | None ->
              error "The class can only be inherited from the Exception class!"
          | Some parent_o when String.compare "Exception" parent_o.class_key = 0
            ->
              let new_val =
                { parent_o with
                  children_keys= some_class.class_key :: parent_o.children_keys
                } in
              monadic_update_hash_table class_table key new_val
              >> return new_val
          | Some _ ->
              error "The class can only be inherited from the Exception class!"
          ) in
    monadic_seq_iter
      (Hashtbl.to_seq_values class_table)
      update_helper class_table

  (* Inheritance of the parent field *)
  let inherit_fields parent children =
    let exception_inherit_field : class_t -> field_t -> unit t =
     fun child_class p_field ->
      (* See if there is such a field in the child's table*)
      match Hashtbl.find_opt child_class.field_table p_field.field_key with
      (* No - just adding a child to the table *)
      | None ->
          return (Hashtbl.add child_class.field_table p_field.field_key p_field)
      (* There is - well, okay, skip *)
      | _ -> return () in
    monadic_seq_iter
      (Hashtbl.to_seq_values parent.field_table)
      (exception_inherit_field children)
      ()

  (** Inheritance of the parent methods*)
  let inherit_methods parent children =
    let exception_inherit_method : class_t -> method_t -> unit t =
     fun child_class p_method ->
      (* See if there is such a method in the child's table*)
      match Hashtbl.find_opt child_class.method_table p_method.method_key with
      (* No - just adding a child to the table *)
      | None ->
          return
            (Hashtbl.add child_class.method_table p_method.method_key p_method)
      (* There is - well, okay, skip *)
      | _ -> return () in
    monadic_seq_iter
      (Hashtbl.to_seq_values parent.method_table)
      (exception_inherit_method children)
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
        match Hashtbl.find_opt parent.method_table child_method.method_key with
        | None ->
            error "Not overriden method or parent does not exist this method!"
        | _ -> return () ) in
    monadic_seq_iter
      (Hashtbl.to_seq_values children.method_table)
      (check parent) ()

  let inherit_from_parent parent children =
    inherit_fields parent children
    >> inherit_methods parent children
    >> check_override_mod parent children

  let inherit_from_exception class_table =
    get_elem_ht class_table "Exception"
    >>= fun ex_cl ->
    let helper child_key =
      get_elem_ht class_table child_key
      >>= fun child_c -> inherit_from_parent ex_cl child_c in
    monadic_list_iter ex_cl.children_keys helper class_table

  let interpret_classes class_list class_table =
    match class_list with
    | [] -> error "No classes found, incorrect syntax or empty file"
    | _ ->
        system_exception_init class_table
        >>= fun table_with_ex ->
        class_adding class_list table_with_ex
        >>= fun table_with_classes ->
        add_default_constructor table_with_classes
        >>= fun table_with_constr ->
        inherit_exception_class table_with_constr
        >>= fun new_table -> inherit_from_exception new_table
end
