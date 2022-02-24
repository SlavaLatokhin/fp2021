type datum =
  (* Переменная или лист перменных *)
  | DConst of const
  | List of const list

and const =
  | Bool of bool (* #t или #f *)
  | PosInt of int (* +1, 1 *)
  | NegInt of int (* -1 *)
  | String of string

and syn_binding = keyword * expr

and formals =
  | FVarList of variable list
  | FVar of variable

and operator = Op of expr
and operand = Obj of expr
and proc_call = operator * operand list

(* and derived_expr =
  | Cond of expr * expr * expr
  | Let of syn_binding list * def list (**в книге как будто бы по другому скобки**)
  | LetRec of syn_binding list * def list *   стоят, не так как в коде Какаду    * *)
and expr =
  | Err
  | Var of variable (* Переменная *)
  | Quote of datum (* '(<datum> list) или (quote <datum> list) *)
  | Const of const (* 1, "word", #t *)
  | Proc_call of proc_call (* Вызов любой функции, созданной пользователем или уже имеющейся, например: (+ 1 1) *)
  | Lam of formals * expr * expr list (* (lambda <formals> <body>) *)
  | Cond of expr * expr * expr option
(* (if <test> <consequent> <alternate>) *)
(* | Der_expr of derived_expr *)

and id = string
and keyword = id
and variable = id

(* and var_def = variable * expr *)
and def = variable * expr

and form =
  | Def of variable * expr
  | Expr of expr

and program = form list [@@deriving eq, show { with_path = false }]
