type datum =
  | DConst of dconst (*переменная*)
  | DList of datum list (*лист переменных*)

  and dconst =
  | DBool of bool (*#t или #f*)
  | DInt of int
  | DString of string
  | DSymbol of id

and const =
  | CBool of bool (*#t или #f*)
  | CInt of int
  | CString of string
(*   
and syn_binding = keyword * expr *)

and formals = 
| FVarList of variable list
| FVar of variable

and expr =
  | EConst of const (** 1, "string", false, k**)
  | EVar of variable
  | EQuote of datum (* '(<datum> list) или (quote <datum> list) *)
  | ELambda of formals * expr * expr list option
  | ECond of expr * expr * expr option
  | EProc_call of expr * expr list (** (+ 1 1)*)

and id = string (**в сумме ключевые слова, переменные и символы*)
and keyword = id (**ключевые слова*)
and variable = id (**переменные*)

and def = variable * expr

and form =
  | FDef of def
  | FExpr of expr

and program = form list  [@@deriving eq, show { with_path = false }]
