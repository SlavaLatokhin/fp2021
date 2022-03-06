open Ast

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type MONADERROR = sig
  include MONAD

  val fail : string -> 'a t
end

module Result = struct
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind
  let return = Result.ok
  let fail = Result.error
end

module Eval (M : MONADERROR) = struct
  open M

  (* Context *)
  type var_ctx =
    { id : identifier
    ; value : value
    }

  type func_ctx =
    { id : identifier
    ; args : identifier list
    ; stmts : statement list
    }

  type class_ctx =
    { id : identifier
    ; vars : var_ctx list
    ; funcs : func_ctx list
    }

  type global_ctx =
    { id : identifier
    ; vars : var_ctx list
    ; sub_ctx : global_ctx list
    ; funcs : func_ctx list
    ; classes : class_ctx list
    ; signal : signal
    ; last_return : value
    ; output : string list (** holds values to be printed on a console *)
    }

  (* Init Context *)
  let tmp_ctx =
    { id = Identifier "tmp"
    ; vars = []
    ; sub_ctx = []
    ; funcs = []
    ; classes = []
    ; signal = Work
    ; last_return = Nil
    ; output = []
    }
  ;;

  let tmp_class_ctx = { id = Identifier "tmp"; vars = []; funcs = [] }

  let main_ctx =
    { id = Identifier "main"
    ; vars = []
    ; sub_ctx = [ tmp_ctx ]
    ; funcs = []
    ; classes = []
    ; signal = Work
    ; last_return = Nil
    ; output = []
    }
  ;;

  (* Context Interface *)
  (* Existence *)
  let if_var_exists_in_ctx i ctx = List.exists (fun (x : var_ctx) -> i = x.id) ctx.vars
  let if_func_exists_in_ctx i ctx = List.exists (fun (x : func_ctx) -> i = x.id) ctx.funcs

  let if_class_exists_in_ctx i ctx =
    List.exists (fun (x : class_ctx) -> i = x.id) ctx.classes
  ;;

  let if_func_exists_in_class_ctx i (ctx : class_ctx) =
    List.exists (fun (x : func_ctx) -> i = x.id) ctx.funcs
  ;;

  (* Changing *)
  let add_var_in_ctx id value ctx = { ctx with vars = { id; value } :: ctx.vars }

  let change_var_in_ctx i value ctx =
    let new_vars =
      List.map (fun (x : var_ctx) -> if i = x.id then { x with value } else x) ctx.vars
    in
    { ctx with vars = new_vars }
  ;;

  let add_func_in_ctx id args stmts ctx =
    { ctx with funcs = { id; args; stmts } :: ctx.funcs }
  ;;

  (* Getting *)
  let get_var_ctx_from_ctx i ctx = List.find (fun (x : var_ctx) -> i = x.id) ctx.vars
  let get_func_ctx_from_ctx i ctx = List.find (fun (x : func_ctx) -> i = x.id) ctx.funcs

  let get_class_ctx_from_ctx i ctx =
    List.find (fun (x : class_ctx) -> i = x.id) ctx.classes
  ;;

  let get_func_ctx_from_class_ctx i (ctx : class_ctx) =
    match if_func_exists_in_class_ctx i ctx with
    | true -> Some (List.find (fun (x : func_ctx) -> i = x.id) ctx.funcs)
    | false -> None
  ;;

  (* Unpackers *)
  let multliple_str_unpacker = function
    | Integer x -> Int.to_string x
    | Float x -> Float.to_string x
    | String x -> x
    | Boolean x -> Bool.to_string x
    | Nil | List _ | Object _ | Lambda _ -> ""
  ;;

  let unpack_identifier_in_value = function
    | Object x -> return x
    | _ -> fail "object was expected"
  ;;

  let unpack_string_in_identifier = function
    | Identifier x -> return x
  ;;

  let unpack_int_in_value = function
    | Integer i -> return i
    | _ -> fail "not a integer"
  ;;

  let unpack_list_in_value = function
    | List l -> return l
    | _ -> fail "not a List"
  ;;

  (* Misc *)
  let if_list i ctx =
    match (get_var_ctx_from_ctx i ctx).value with
    | List _ -> true
    | _ -> false
  ;;

  let combine_args_and_params args params =
    List.map (fun x -> { id = fst x; value = snd x }) (List.combine args params)
  ;;

  let get_identifiers_from_args =
    List.map (function
        | Variable (Local, i) -> return i
        | _ -> fail "Local variables only")
  ;;

  let update_or_add_var ctx (x : var_ctx) =
    match if_var_exists_in_ctx x.id ctx with
    | true -> change_var_in_ctx x.id x.value ctx
    | false -> { ctx with vars = x :: ctx.vars }
  ;;

  let update_or_add_var_list = List.fold_left update_or_add_var
  let form_var_ctx id value = { id; value }

  let rec mmap f = function
    | [] -> return []
    | h :: tl -> f h >>= fun c -> mmap f tl >>= fun lst -> return (c :: lst)
  ;;

  let rec fold_left f init = function
    | [] -> return init
    | hd :: tl -> f init hd >>= fun init -> fold_left f init tl
  ;;

  type dispatch =
    { expr : dispatch -> global_ctx -> expression -> value t
    ; stmt : dispatch -> global_ctx -> statement -> global_ctx t
    }

  let bundle =
    let rec expr (duo : dispatch) (e_env : global_ctx) ex =
      let rec doer1 c s =
        if c.signal == Return
        then return c.last_return
        else (
          match s with
          | [] -> return Nil
          | hd :: tl -> duo.stmt duo c hd >>= fun x -> doer1 x tl)
      in
      let rec doer2 c s =
        if c.signal == Return
        then return c.last_return
        else (
          match s with
          | [] -> return Nil
          | hd :: tl ->
            (match hd with
            | Expression e ->
              expr duo c e
              >>= fun last_return -> expr duo { c with signal = Return; last_return } e
            | _ -> duo.stmt duo c hd >>= fun x -> doer2 x tl))
      in
      match ex with
      | Constant x -> return x
      | Add (l, r) ->
        expr duo e_env l
        >>= fun l ->
        expr duo e_env r
        >>= fun r ->
        (match l, r with
        | Integer l1, Integer r1 -> return @@ Integer (l1 + r1)
        | Float l1, Float r1 -> return @@ Float (l1 +. r1)
        | Float l1, Integer r1 -> return @@ Float (l1 +. Int.to_float r1)
        | Integer l1, Float r1 -> return @@ Float (Int.to_float l1 +. r1)
        | _ -> fail "L or R has unsupported type")
      | Sub (l, r) ->
        expr duo e_env l
        >>= fun l ->
        expr duo e_env r
        >>= fun r ->
        (match l, r with
        | Integer l1, Integer r1 -> return @@ Integer (l1 - r1)
        | Float l1, Float r1 -> return @@ Float (l1 -. r1)
        | Float l1, Integer r1 -> return @@ Float (l1 -. Int.to_float r1)
        | Integer l1, Float r1 -> return @@ Float (Int.to_float l1 -. r1)
        | _ -> fail "L or R has unsupported type")
      | Mul (l, r) ->
        expr duo e_env l
        >>= fun l ->
        expr duo e_env r
        >>= fun r ->
        (match l, r with
        | Integer l1, Integer r1 -> return @@ Integer (l1 * r1)
        | Float l1, Float r1 -> return @@ Float (l1 *. r1)
        | Float l1, Integer r1 -> return @@ Float (l1 *. Int.to_float r1)
        | Integer l1, Float r1 -> return @@ Float (Int.to_float l1 *. r1)
        | _ -> fail "L or R has unsupported type")
      | Div (l, r) ->
        expr duo e_env l
        >>= fun l ->
        expr duo e_env r
        >>= fun r ->
        (match l, r with
        | Integer l1, Integer r1 -> return @@ Integer (l1 / r1)
        | Float l1, Float r1 -> return @@ Float (l1 /. r1)
        | Float l1, Integer r1 -> return @@ Float (l1 /. Int.to_float r1)
        | Integer l1, Float r1 -> return @@ Float (Int.to_float l1 /. r1)
        | _ -> fail "L or R has unsupported type")
      | Mod (l, r) ->
        expr duo e_env l
        >>= fun l ->
        expr duo e_env r
        >>= fun r ->
        (match l, r with
        | Integer l1, Integer r1 -> return @@ Integer (l1 mod r1)
        | _ -> fail "L or R has unsupported type")
      | Equal (l, r) ->
        expr duo e_env l
        >>= fun l -> expr duo e_env r >>= fun r -> return @@ Boolean (l = r)
      | NotEqual (l, r) ->
        expr duo e_env l
        >>= fun l -> expr duo e_env r >>= fun r -> return @@ Boolean (l != r)
      | Less (l, r) ->
        expr duo e_env l
        >>= fun l -> expr duo e_env r >>= fun r -> return @@ Boolean (l < r)
      | LessOrEq (l, r) ->
        expr duo e_env l
        >>= fun l -> expr duo e_env r >>= fun r -> return @@ Boolean (l <= r)
      | Greater (l, r) ->
        expr duo e_env l
        >>= fun l -> expr duo e_env r >>= fun r -> return @@ Boolean (l > r)
      | GreaterOrEq (l, r) ->
        expr duo e_env l
        >>= fun l -> expr duo e_env r >>= fun r -> return @@ Boolean (l >= r)
      | And (l, r) ->
        expr duo e_env l
        >>= fun l -> expr duo e_env r >>= fun r -> return @@ Boolean (l = r)
      | Or (l, r) ->
        expr duo e_env l
        >>= fun l ->
        expr duo e_env r
        >>= fun r -> return @@ Boolean (l = Boolean true || r = Boolean true)
      | Variable (_, i) ->
        (match if_var_exists_in_ctx i e_env with
        | true -> return @@ (get_var_ctx_from_ctx i e_env).value
        | false -> fail "undefined Variable")
      | ListAccess (i, e) ->
        (match if_var_exists_in_ctx i e_env with
        | false -> fail "undefined Variable"
        | true ->
          (match if_list i e_env with
          | false -> fail "not a List"
          | true ->
            let l = unpack_list_in_value (get_var_ctx_from_ctx i e_env).value in
            let index = expr duo e_env e >>= fun x -> return @@ unpack_int_in_value x in
            index
            >>= fun index ->
            index
            >>= fun index ->
            l >>= fun l -> expr duo e_env (List.nth l index) >>= fun x -> return @@ x))
      | MonoCall (i, e) ->
        (match if_func_exists_in_ctx i e_env with
        | false -> fail "undefined Function"
        | true ->
          mmap (fun x -> expr duo e_env x) e
          >>= fun x ->
          doer1
            { (update_or_add_var_list
                 e_env
                 (combine_args_and_params (get_func_ctx_from_ctx i e_env).args x))
              with
              id = Identifier "tmp"
            ; sub_ctx = [ { tmp_ctx with vars = e_env.vars } ]
            ; funcs = [ get_func_ctx_from_ctx i e_env ]
            }
            (get_func_ctx_from_ctx i e_env).stmts)
      | PolyCall (i1, i2, e) ->
        (match if_var_exists_in_ctx i1 e_env with
        | false -> fail "undefined Variable"
        | true ->
          (match i2 with
          | Identifier "call" ->
            (match (get_var_ctx_from_ctx i1 e_env).value with
            | Lambda (il, sl) ->
              mmap (fun x -> expr duo e_env x) e
              >>= fun x ->
              doer2
                (update_or_add_var_list
                   (update_or_add_var_list e_env (combine_args_and_params il x))
                   (List.hd e_env.sub_ctx).vars)
                sl
            | _ -> fail "lambda was expected")
          | Identifier "to_s" ->
            return
            @@ String (multliple_str_unpacker (get_var_ctx_from_ctx i1 e_env).value)
          | _ ->
            let obj_class =
              unpack_identifier_in_value (get_var_ctx_from_ctx i1 e_env).value
            in
            obj_class
            >>= fun x ->
            let obj_class_ctx = get_class_ctx_from_ctx x e_env in
            (match if_class_exists_in_ctx x e_env with
            | false -> fail "undefined Class"
            | true ->
              (match get_func_ctx_from_class_ctx i2 obj_class_ctx with
              | Some func ->
                mmap (fun x -> expr duo e_env x) e
                >>= fun x ->
                doer1
                  { tmp_ctx with
                    vars = obj_class_ctx.vars @ combine_args_and_params func.args x
                  ; funcs = obj_class_ctx.funcs
                  }
                  func.stmts
              | None ->
                (match
                   get_func_ctx_from_class_ctx (Identifier "method_missing") obj_class_ctx
                 with
                | None -> fail "undefined Class Function"
                | Some mm_func ->
                  unpack_string_in_identifier i2
                  >>= fun x ->
                  doer1
                    { tmp_ctx with
                      vars =
                        obj_class_ctx.vars
                        @ combine_args_and_params [ List.hd mm_func.args ] [ String x ]
                        @ combine_args_and_params
                            [ List.hd (List.rev (List.tl mm_func.args)) ]
                            [ List e ]
                    ; funcs = obj_class_ctx.funcs
                    }
                    mm_func.stmts)))))
      | CallLambda (e1, s1, e2) ->
        mmap (fun x -> expr duo e_env x) e2
        >>= fun x ->
        doer2 (update_or_add_var_list e_env (combine_args_and_params e1 x)) s1
    in
    let rec stmt (duo : dispatch) (s_env : global_ctx) = function
      | Break -> return { s_env with signal = Break }
      | Next -> return { s_env with signal = Next }
      | Expression _ -> return s_env
      | Return e ->
        duo.expr duo s_env e
        >>= fun rtrn -> return { s_env with signal = Return; last_return = rtrn }
      | Assign (l, r) ->
        (match l with
        | Variable (Local, id) ->
          duo.expr duo s_env r
          >>= fun value -> return @@ update_or_add_var s_env { id; value }
        | _ -> fail "L has unsupported type")
      | MultipleAssign (ll, rl) ->
        let id_list =
          mmap
            (function
              | Variable (Local, i) -> return i
              | _ -> fail "L has unsupported type")
            ll
        in
        let val_list = mmap (fun x -> duo.expr duo s_env x) rl in
        id_list
        >>= fun id_list ->
        val_list
        >>= fun val_list ->
        let var_list = List.map2 form_var_ctx id_list val_list in
        return @@ update_or_add_var_list s_env var_list
      | IfElse (e, s1, s2) ->
        let fl = fold_left (fun x -> stmt duo x) s_env in
        duo.expr duo s_env e
        >>= (function
        | Boolean true -> fl s1
        | Boolean false -> fl s2
        | _ -> fail "condition was expected")
      | Puts x -> duo.expr duo s_env x >>= fun x -> return @@ add_output s_env x
      | While (e, s) ->
        let rec checker ctx =
          duo.expr duo ctx e
          >>= function
          | Boolean false -> return ctx
          | _ -> doer ctx s
        and doer ctx1 s =
          match ctx1.signal with
          | Break -> return { ctx1 with signal = Work }
          | Next -> checker { ctx1 with signal = Work }
          | Return -> return ctx1
          | _ ->
            (match s with
            | [] -> checker ctx1
            | hd :: tl -> stmt duo ctx1 hd >>= fun x -> doer x tl)
        in
        checker s_env
      | Function (i, a, s) -> return @@ add_func_in_ctx i a s s_env
      | Class (id, s) ->
        let outer_ctx = fold_left (fun x -> stmt duo x) tmp_ctx s in
        let cl_ctx =
          outer_ctx
          >>= fun outer_ctx ->
          return { tmp_class_ctx with vars = outer_ctx.vars; funcs = outer_ctx.funcs }
        in
        (match if_class_exists_in_ctx id s_env with
        | true -> return s_env
        | false ->
          cl_ctx
          >>= fun cl_ctx ->
          return { s_env with classes = { cl_ctx with id } :: s_env.classes })
    and add_output ctx = function
      | Integer x -> { ctx with output = Int.to_string x :: ctx.output }
      | Float x -> { ctx with output = Float.to_string x :: ctx.output }
      | String x -> { ctx with output = x :: ctx.output }
      | Boolean x -> { ctx with output = Bool.to_string x :: ctx.output }
      | Nil -> { ctx with output = "" :: ctx.output }
      | Object (Identifier x) -> { ctx with output = ("obj:" ^ x) :: ctx.output }
      | Lambda _ | List _ -> ctx
    in
    { expr; stmt }
  ;;

  let init_main_ctx = fold_left (bundle.stmt bundle) main_ctx
end

open Eval (Result)

let run_ruby str =
  let x = init_main_ctx @@ Parser.parser_result_to_stmt_list str in
  let print_output = List.iter (Printf.printf "%s\n") in
  match x with
  | Ok res -> print_output (List.rev res.output)
  | Error _ -> Format.printf "interpreter error"
;;
