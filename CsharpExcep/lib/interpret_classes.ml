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

  let rec monadic_list_iter action list ret =
    match list with
    | [] -> return ret
    | x :: xs -> action x >> monadic_list_iter action xs ret

  let system_exception_init class_table =
    let field_table = Hashtbl.create 16 in
    let method_table = Hashtbl.create 16 in
    let body = StatementBlock [Return (Some (IdentVar "message"))] in
    let to_string : method_t =
      {method_type= String; method_key= "ToString"; args= []; body} in
    let message : field_t =
      {field_type= String; field_key= "message"; is_const= false; sub_tree= None}
    in
    let dec_class =
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
      ; parent_key= None
      ; dec_class } ;
    return class_table

  let add_classes class_list class_table =
    let add_class_in_table cl_table adding_class =
      match adding_class with
      | Class (_, class_key, parent, fields) -> (
          (* Initialize tables *)
          let method_table = Hashtbl.create 1024 in
          let field_table = Hashtbl.create 1024 in
          (* Function of adding a class element to the corresponding table *)
          let add_class_elem : modifier list * field -> unit M.t =
           fun field_elem ->
            match field_elem with
            | mod_list, VariableField (field_type, arg_list) ->
                let rec helper_add_var = function
                  | [] -> return ()
                  | (field_key, sub_tree) :: ps ->
                      let is_const = List.mem Const mod_list in
                      ( match Hashtbl.find_opt field_table field_key with
                      | None ->
                          Hashtbl.add field_table field_key
                            {field_type; field_key; is_const; sub_tree} ;
                          return field_table
                      | _ ->
                          error
                            ( "The field with this key: " ^ field_key
                            ^ " already exists" ) )
                      >> helper_add_var ps in
                helper_add_var arg_list
            | _, Method (method_type, method_key, args, body) ->
                ( match Hashtbl.find_opt method_table method_key with
                | None ->
                    Hashtbl.add method_table method_key
                      {method_type; method_key; args; body} ;
                    return method_table
                | _ ->
                    error
                      ( "The method with this key: " ^ method_key
                      ^ " already exists" ) )
                >> return () in
          monadic_list_iter add_class_elem fields ()
          >>
          let parent_key = parent in
          match Hashtbl.find_opt cl_table class_key with
          | None ->
              Hashtbl.add cl_table class_key
                { class_key
                ; field_table
                ; method_table
                ; parent_key
                ; dec_class= adding_class } ;
              return cl_table
          | _ ->
              error ("The class with this key: " ^ class_key ^ " already exists")
          ) in
    monadic_list_iter (add_class_in_table class_table) class_list class_table

  let interpret_classes class_list class_table =
    match class_list with
    | [] -> error "No classes found, incorrect syntax or empty file"
    | _ ->
        system_exception_init class_table
        >>= fun class_table_with_ex ->
        add_classes class_list class_table_with_ex
end
