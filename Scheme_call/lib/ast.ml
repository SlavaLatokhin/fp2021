type datum =
  (* Переменная или лист перменных *)
  | DConst of dconst
  | List of datum list

and dconst =
  | DInt of int (* -1, 1 *)
  | DBool of bool (* #t или #f *)
  | DString of string

and const =
  | Int of int (* -1, 1 *)
  | Bool of bool (* #t или #f *)
  | String of string

and operator = Op of expr

and formals =
  | FVarList of variable list
  | FVar of variable

and id = string

and expr =
  | Var of variable (* Переменная *)
  | Quote of datum (* '(<datum> list) или (quote <datum> list)*)
  | Const of const (* 1, "word", #t *)
  | ProcCall of operator * expr list (* Вызов любой функции, созданной пользователем или уже имеющейся, например: (+ 1 1) *)
  | Lam of formals * expr * expr list (*   (lambda <formals> <body>)  *)
  | Cond of expr * expr * expr option
(* (if <test> <consequent> <alternate>) *)

and variable = id

and form =
  | Def of variable * expr
  | Expr of expr

and program = form list [@@deriving eq, show { with_path = false }]
