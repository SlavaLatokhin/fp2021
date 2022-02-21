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

  let system_exception_init class_list =
    let body = StatementBlock [Return (Some (IdentVar "message"))] in
    let method_t : method_t =
      {method_type= String; method_key= "ToString"; args= []; body} in
    let field_t : field_t =
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
    let class_t =
      { class_key= "Exception"
      ; field_list= []
      ; method_list= []
      ; parent_key= None
      ; dec_class } in
    return (add_method class_t method_t)
    >>= fun class_t ->
    return (add_field class_t field_t)
    >>= fun class_t -> return (class_t :: class_list)

  let add_classes class_list_ast class_list =
    let add_class_in_list cl_list adding_class =
      match adding_class with
      | Class (_, class_key, parent, fields) -> (
          (* Function of adding a class element to the corresponding list *)
          let add_class_elem field_elem field_list method_list =
            match field_elem with
            | mod_list, VariableField (field_type, arg_list) ->
                let rec helper_add_var list field_list method_list =
                  match list with
                  | [] -> return (field_list, method_list)
                  | (field_key, sub_tree) :: ps ->
                      let is_const = List.mem Const mod_list in
                      ( match find_opt_field field_list field_key with
                      | None ->
                          return
                            ( {field_type; field_key; is_const; sub_tree}
                              :: field_list
                            , method_list )
                      | _ ->
                          error
                            (String.concat ""
                               [ "The field with this key: "; field_key
                               ; " already exists" ] ) )
                      >>= fun (field_l, method_l) ->
                      helper_add_var ps field_l method_l in
                helper_add_var arg_list field_list method_list
            | _, Method (method_type, method_key, args, body) -> (
              match find_opt_method method_list method_key with
              | None ->
                  return
                    ( field_list
                    , {method_type; method_key; args; body} :: method_list )
              | _ ->
                  error
                    (String.concat ""
                       [ "The method with this key: "; method_key
                       ; " already exists" ] ) ) in
          let rec iter_fields fields field_list method_list =
            match fields with
            | [] -> return (field_list, method_list)
            | x :: xs ->
                add_class_elem x field_list method_list
                >>= fun (field_l, method_l) -> iter_fields xs field_l method_l
          in
          iter_fields fields [] []
          >>= fun (field_list, method_list) ->
          let parent_key = parent in
          match find_opt_class cl_list class_key with
          | None ->
              let class_t =
                { class_key
                ; field_list
                ; method_list
                ; parent_key
                ; dec_class= adding_class } in
              return (class_t :: cl_list)
          | _ ->
              error
                (String.concat ""
                   ["The class with this key: "; class_key; " already exists"] )
          ) in
    let rec iter_classes class_list_ast class_list =
      match class_list_ast with
      | [] -> return class_list
      | x :: xs ->
          add_class_in_list class_list x
          >>= fun class_l -> iter_classes xs class_l in
    iter_classes class_list_ast class_list >>= fun class_l -> return class_l

  let interpret_classes class_list_ast class_list =
    match class_list_ast with
    | [] -> error "No classes found, incorrect syntax or empty file"
    | _ ->
        system_exception_init class_list
        >>= fun class_list_with_ex ->
        add_classes class_list_ast class_list_with_ex
end
