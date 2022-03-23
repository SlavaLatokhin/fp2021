type const =
  | CBool of bool
  | CInt of int
  | CString of string

and expr =
  | EConst of const (** #t, 1, "Hello world" *)
  | EVar of variable (** a *)
  | EQuote of datum (** '(datum) *)
  | ELambda of formals * expr * expr list option (** (lambda (x) (+ 1 x)) *)
  | ECond of expr * expr * expr option (** (if #t 1 2), (cond ((> 1 2) 1) (else 2)) *)
  | EProc_call of expr * expr list
      (** Все процедурные вызовы (включая сложение (+ 1 2) и т.п.) *)

and formals =
  | FVarList of variable list (* ((lambda (x y) (+ x y)) 1 2) = 3 *)
  | FVar of variable (** ((lambda x (apply + x)) 1 2) = 3 *)

and datum =
  | DConst of datum_const (** '#t, '1, '"Hello world" *)
  | DList of datum list (** '(#t 1 "Hello world") *)

and variable = id

and datum_const =
  | DBool of bool
  | DInt of int
  | DString of string
  | DSymbol of id (** '+ *)

and id = string

and command =
  | FDef of variable * expr (** define a 1 *)
  | FExpr of expr (** (+ 1 2) *)

and program = command list [@@deriving eq, show { with_path = false }]
