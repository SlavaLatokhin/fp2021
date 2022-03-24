open Ast

module EnvMap = struct
  include Map.Make (String)

  let pp pp_v ppf m =
    Format.fprintf ppf "@[[@[";
    iter (fun k v -> Format.fprintf ppf "@[\"%s\": %a@],@\n" k pp_v v) m;
    Format.fprintf ppf "@]]@]"
  ;;
end

exception Not_bound

let lookup id env =
  try EnvMap.find id env with
  | Not_found -> raise Not_bound
;;

let empty_env = EnvMap.empty

module type MONAD_FAIL = sig
  include Base.Monad.S2

  val run : ('a, 'e) t -> ok:('a -> ('b, 'e) t) -> err:('e -> ('b, 'e) t) -> ('b, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

type exval =
  | TupleV of exval list
  | ListV of exval list
  | SomeV of exval
  | NoneV
  | FunV of pt * exp * env Lazy.t * scan_info
  | RefV of exval ref
  | IntV of int
  | StringV of string
  | BoolV of bool
  | InternalV
[@@deriving show { with_path = false }]

and error =
  | Match_fail of pt * exval (** Pattern matching error *)
  | Match_exhaust of exp (** Pattern was not found*)
  | Wrong_arg_pat of pt
  | Wrong_arg of exp
  | Wrong_un_op of un_op * exval (** Incorrect unary operation *)
  | Wrong_bin_op of bin_op * exval * exval (** Incorrect binary operation *)
  | Not_bool_condition of exp (** The condition of the if statement must be boolean *)
  | Not_bound of id (** There is no such key on the map *)
  | Not_function of exp (** Applying an expression not to a function *)
  | Let_rec_only_vars of pt (** Allows patterns *)
  | Undef_var of id (** No such variable in current env *)
  | Division_by_zero
  | Ref_error
[@@deriving show { with_path = false }]

and env = exval EnvMap.t [@@deriving show { with_path = false }]

and scan_info = Info of env * env * env * id list

module Interpret (M : MONAD_FAIL) = struct
  open M

  let pp_value (k, v) =
    let open Format in
    let rec helper _ = function
      | NoneV -> printf "<none>"
      | SomeV x -> helper std_formatter x
      | BoolV b -> printf "%b" b
      | FunV (_, _, _, _) -> printf "<fun>"
      | InternalV -> printf "<internal>"
      | RefV x -> helper std_formatter !x
      | IntV n -> printf "%d" n
      | TupleV l ->
        printf "(%a)" (pp_print_list ~pp_sep:(fun _ _ -> printf ", ") helper) l
      | ListV l -> printf "[%a]" (pp_print_list ~pp_sep:(fun _ _ -> printf "; ") helper) l
      | StringV s -> printf "%S" s
    in
    printf "val %s = %a\n%!" k helper v
  ;;

  let exd_env id v env = EnvMap.add id v env

  let extend_state env binds =
    List.fold_left (fun env (id, v) -> exd_env id v env) env binds
  ;;

  let lookup_env id env =
    try return (lookup id env) with
    | Not_bound -> fail (Not_bound id)
  ;;

  let fold list ~f ~init =
    let rec helper acc = function
      | hd :: tl -> helper (acc >>= fun acc -> f acc hd) tl
      | [] -> acc
    in
    helper (return init) list
  ;;

  let rec pt_match pt var =
    match pt, var with
    | PNone, NoneV -> return []
    | PNil, ListV [] -> return []
    | PWild, _ -> return []
    | PSome x, SomeV y -> pt_match x y
    | PVar id, v -> return [ id, v ]
    | PTuple pats, TupleV vars ->
      (match pats, vars with
      | hd_p :: tl_p, hd_v :: tl_v ->
        let* hd_bind = pt_match hd_p hd_v in
        let* tl_bind = pt_match (PTuple tl_p) (TupleV tl_v) in
        return (hd_bind @ tl_bind)
      | [], [] -> return []
      | _ -> fail (Match_fail (PTuple pats, TupleV vars)))
    | PCons (pat1, pat2), ListV (hd :: tl) ->
      let* hd_matched = pt_match pat1 hd in
      let* tl_matched = pt_match pat2 (ListV tl) in
      return (hd_matched @ tl_matched)
    | PConst x, v ->
      (match x, v with
      | CInt a, IntV b when a = b -> return []
      | CString a, StringV b when a = b -> return []
      | CBool a, BoolV b when a = b -> return []
      | _ -> fail (Match_fail (PConst x, v)))
    | a, b -> fail (Match_fail (a, b))
  ;;

  let apply_bin_op op x y =
    match op, x, y with
    | Eq, IntV x, IntV y -> return (BoolV (x == y))
    | Eq, StringV x, StringV y -> return (BoolV (x == y))
    | Eq, BoolV x, BoolV y -> return (BoolV (x == y))
    | Eq, TupleV x, TupleV y -> return (BoolV (x == y))
    | Eq, ListV x, ListV y -> return (BoolV (x == y))
    | Neq, IntV x, IntV y -> return (BoolV (x != y))
    | Neq, StringV x, StringV y -> return (BoolV (x != y))
    | Neq, BoolV x, BoolV y -> return (BoolV (x != y))
    | Neq, TupleV x, TupleV y -> return (BoolV (x != y))
    | Neq, ListV x, ListV y -> return (BoolV (x != y))
    | And, BoolV x, BoolV y -> return (BoolV (x && y))
    | Or, BoolV x, BoolV y -> return (BoolV (x || y))
    | Gre, IntV x, IntV y -> return (BoolV (x > y))
    | Gre, StringV x, StringV y -> return (BoolV (x > y))
    | Gre, TupleV x, TupleV y -> return (BoolV (x > y))
    | Gre, ListV x, ListV y -> return (BoolV (x > y))
    | Geq, IntV x, IntV y -> return (BoolV (x >= y))
    | Geq, StringV x, StringV y -> return (BoolV (x >= y))
    | Geq, TupleV x, TupleV y -> return (BoolV (x >= y))
    | Geq, ListV x, ListV y -> return (BoolV (x >= y))
    | Less, IntV x, IntV y -> return (BoolV (x < y))
    | Less, StringV x, StringV y -> return (BoolV (x < y))
    | Less, TupleV x, TupleV y -> return (BoolV (x < y))
    | Less, ListV x, ListV y -> return (BoolV (x < y))
    | Leq, IntV x, IntV y -> return (BoolV (x <= y))
    | Leq, StringV x, StringV y -> return (BoolV (x <= y))
    | Leq, TupleV x, TupleV y -> return (BoolV (x <= y))
    | Leq, ListV x, ListV y -> return (BoolV (x <= y))
    | Mul, IntV x, IntV y -> return (IntV (x * y))
    | Div, IntV _, IntV y when y = 0 -> fail Division_by_zero
    | Div, IntV x, IntV y -> return (IntV (x / y))
    | Sub, IntV x, IntV y -> return (IntV (x - y))
    | Add, IntV x, IntV y -> return (IntV (x + y))
    | a, b, c -> fail (Wrong_bin_op (a, b, c))
  ;;

  let apply_un_op op x =
    match op, x with
    | Not, BoolV x -> return (BoolV (not x))
    | Minus, IntV x -> return (IntV (-x))
    | a, b -> fail (Wrong_un_op (a, b))
  ;;

  let rec efun env env_lab env_opt env_b keys_b = function
    | EFun (pt, exp) ->
      (match pt with
      | PVar id ->
        let new_state = exd_env id InternalV env_b in
        let new_list = keys_b @ [ id ] in
        efun env env_lab env_opt new_state new_list exp
      | POptional (name, e) ->
        let* evaled = eval_exp env e in
        let new_state = exd_env name evaled env_opt in
        efun env env_lab new_state env_b keys_b exp
      | PLabeled id ->
        let new_state = exd_env id InternalV env_lab in
        efun env new_state env_opt env_b keys_b exp
      | p -> fail (Wrong_arg_pat p))
    | _ -> return (Info (env_lab, env_opt, env_b, keys_b))

  and scan_app env = function
    | EApp (exp_h1, exp_h2) ->
      let* Info (lab, opt, basic, keys), fstate, body = scan_app env exp_h1 in
      (match exp_h2 with
      | EArg (Labeled (name, exp)) ->
        run
          (lookup_env name lab)
          ~ok:(fun x ->
            match x with
            | InternalV ->
              let* evaled = eval_exp env exp in
              let new_state = exd_env name evaled fstate in
              let new_lab = exd_env name evaled lab in
              return (Info (new_lab, opt, basic, keys), new_state, body)
            | _ -> fail (Wrong_arg (EArg (Labeled (name, exp)))))
          ~err:(fun _ ->
            run
              (lookup_env name opt)
              ~ok:(fun _ ->
                let* evaled = eval_exp env exp in
                let new_state = exd_env name evaled fstate in
                let new_opt = exd_env name evaled opt in
                return (Info (lab, new_opt, basic, keys), new_state, body))
              ~err:fail)
      | EArg (Expr e) ->
        (match keys with
        | hd :: tl ->
          let* evaled = eval_exp env e in
          let new_state = exd_env hd evaled fstate in
          let new_basic = exd_env hd evaled basic in
          return (Info (lab, opt, new_basic, tl), new_state, body)
        | [] -> fail (Wrong_arg (EArg (Expr e))))
      | e -> fail (Wrong_arg e))
    | EVar name ->
      let* evaled = eval_exp env (EVar name) in
      (match evaled with
      | FunV (_, body, fstate, Info (lab, opt, basic, keys)) ->
        let fstate = Lazy.force fstate in
        return (Info (lab, opt, basic, keys), fstate, body)
      | _ -> fail (Not_function (EVar name)))
    | e -> fail (Not_function e)

  and eval_exp env = function
    | ENil -> return (ListV [])
    | EVar x -> run (lookup_env x env) ~ok:return ~err:fail
    | ENone -> return NoneV
    | EArg x ->
      (match x with
      | Expr v -> eval_exp env v
      | Labeled (_, v) -> eval_exp env v)
    | EIf (exp1, exp2, exp3) ->
      run
        (eval_exp env exp1)
        ~ok:(function
          | BoolV false -> eval_exp env exp3
          | BoolV true -> eval_exp env exp2
          | _ -> fail (Not_bool_condition exp1))
        ~err:fail
    | EMatch (exp, mathchings) ->
      let* evaled = eval_exp env exp in
      let rec do_match = function
        | (pt, exp) :: tl ->
          run
            (pt_match pt evaled)
            ~ok:(fun binds ->
              let env = extend_state env binds in
              eval_exp env exp)
            ~err:(fun _ -> do_match tl)
        | [] -> fail (Match_exhaust (EMatch (exp, mathchings)))
      in
      do_match mathchings
    | EConst x ->
      (match x with
      | CInt x -> return (IntV x)
      | CBool x -> return (BoolV x)
      | CString x -> return (StringV x))
    | EOp (op, x, y) ->
      let* x_exp = eval_exp env x in
      let* y_exp = eval_exp env y in
      run (apply_bin_op op x_exp y_exp) ~ok:return ~err:fail
    | EUnOp (op, x) ->
      let* x_exp = eval_exp env x in
      run (apply_un_op op x_exp) ~ok:return ~err:fail
    | ESome x ->
      let* evaled = eval_exp env x in
      return (SomeV evaled)
    | ELet (binds, exp1) ->
      let gen_env =
        fold binds ~init:env ~f:(fun env binding ->
            let* _, st = eval_bind env binding in
            return st)
      in
      run gen_env ~ok:(fun s -> eval_exp s exp1) ~err:fail
    | ETuple exps -> M.all (List.map (eval_exp env) exps) >>= fun x -> return (TupleV x)
    | EApp (exp1, exp2) ->
      (match exp1 with
      | EApp (EVar ":=", x) ->
        let* evaled = eval_exp env exp2 in
        let* evaled_ref = eval_exp env x in
        (match evaled_ref with
        | RefV k ->
          k := RefV (ref evaled);
          return evaled
        | _ -> fail Ref_error)
      | EVar "ref" ->
        let* evaled = eval_exp env exp2 in
        return (RefV (ref evaled))
      | _ ->
        let* Info (lab, opt, basic, keys), new_state, body =
          scan_app env (EApp (exp1, exp2))
        in
        let new_state = EnvMap.union (fun _ _ y -> Some y) opt new_state in
        let rec helper = function
          | EFun (pt, exp) ->
            (match pt with
            | POptional (_, _) when keys = [] -> helper exp
            | POptional (_, _) -> return (EFun (pt, exp))
            | PLabeled name ->
              run
                (lookup_env name lab)
                ~ok:(function
                  | InternalV -> return (EFun (pt, exp))
                  | _ -> helper exp)
                ~err:(fun _ -> return (EFun (pt, exp)))
            | PVar name ->
              run
                (lookup_env name basic)
                ~ok:(function
                  | InternalV -> return (EFun (pt, exp))
                  | _ -> helper exp)
                ~err:(fun _ -> return (EFun (pt, exp)))
            | p -> fail (Wrong_arg_pat p))
          | e -> return e
        in
        let* body = helper body in
        (match body with
        | EFun (pt, exp) ->
          let tmp = FunV (pt, exp, lazy new_state, Info (lab, opt, basic, keys)) in
          return tmp
        | _ -> eval_exp new_state body))
    | ECons (exp1, exp2) ->
      let* evaled_exp1 = eval_exp env exp1 in
      let* evaled_exp2 = eval_exp env exp2 in
      (match evaled_exp2 with
      | ListV list -> return (ListV (evaled_exp1 :: list))
      | x -> return (ListV [ evaled_exp1; x ]))
    | EFun (pt, exp) ->
      let* scan_info = efun env empty_env empty_env empty_env [] (EFun (pt, exp)) in
      return (FunV (pt, exp, lazy env, scan_info))

  and eval_bind env (is_rec, pt, exp) =
    if not is_rec
    then
      let* evd = eval_exp env exp in
      let* bs = pt_match pt evd in
      let exd = extend_state env bs in
      return (bs, exd)
    else
      let* id =
        match pt with
        | PVar id -> return id
        | other -> fail (Let_rec_only_vars other)
      in
      let* tmp =
        match exp with
        | EFun (pt, body) ->
          let* scan_info = efun env empty_env empty_env empty_env [] (EFun (pt, body)) in
          let rec new_env = lazy (exd_env id (FunV (pt, body, new_env, scan_info)) env) in
          return (FunV (pt, body, new_env, scan_info))
        | other -> eval_exp env other
      in
      let exded = exd_env id tmp env in
      let* evd = eval_exp exded exp in
      return ([ id, evd ], exd_env id evd exded)
  ;;

  let eval_dec env = function
    | DLet bind -> eval_bind env bind
  ;;

  let eval_prog prog =
    let* binds, _ =
      fold
        ~f:(fun (binds, env) dec ->
          let* new_binds, new_state = eval_dec env dec in
          return (new_binds :: binds, new_state))
        ~init:([], empty_env)
        prog
    in
    return (binds |> List.rev |> List.flatten)
  ;;
end

module InterpreterResult = struct
  include Base.Result

  let run x ~ok ~err =
    match x with
    | Ok v -> ok v
    | Error e -> err e
  ;;

  let ( let* ) x f = x >>= f
end

let eval_pp _ code =
  let open Format in
  let open Interpret (InterpreterResult) in
  match Parser.parse Parser.prog code with
  | Ok prog ->
    InterpreterResult.run
      (eval_prog prog)
      ~err:(fun x -> pp_error std_formatter x)
      ~ok:(fun x -> List.iter pp_value x)
  | _ -> Printf.printf "Parse error"
;;
