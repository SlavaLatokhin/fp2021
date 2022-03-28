type datum = 
      (** Переменная, лист переменных или вложенный quote/quasiquote/unquote *)
  | DConst of dconst
  | DList of datum list
  | DAbreviation of char * datum (** '<datum>; `<datum>; ,<datum> *)

and id = string

and dconst =
  | DInt of int
  | DBool of bool
  | DString of string
  | DSymbol of id (** *; lambda *)

and qlist =
  | QLList of quasiquote list (** `(+ 1 2) = (+ 1 2) *)
  | QLQuote of quasiquote (** ` 'a = 'a *)
  | QLQuasiquote of quasiquote (** `(+ 1 2 `(+ 3 4)) = (+ 1 2 `(+ 3 4)) *)

and quasiquote =
  | QConst of dconst
  | QList of qlist
  | QUnquote of expr (** `(+ 1 2 ,(+ 1 2)) = (+ 1 2 3) *)

and variable = id

and const =
  | Int of int (* -1; 1 *)
  | Bool of bool (* #t или #f *)
  | String of string

and operator = Op of expr

and formals =
  | FVarList of variable list (** Сопоставляет n переменным n выражений *)
  | FVar of variable 
      (** Сопоставляет одной переменной n выражений, создает из них лист, и записывает этот лист в переменную *)

and expr =
  | Quasiquote of quasiquote (** `<qq template> или (quasiquote <qq template>)*)
  | Var of variable (** Переменная *)
  | Quote of datum (** '<datum> или (quote <datum>)*)
  | Const of const (** 1; "word"; #t *)
  | ProcCall of operator * expr list 
      (** Вызов любой функции, созданной пользователем или уже имеющейся. Например: (+ 1 1), ((lambda (n) (* n n)) 5) *)) *)
  | Lam of formals * definition list * expr * expr list 
      (**   (lambda <formals> <definition> list <expr> <expr> list )  *)
  | Cond of expr * expr * expr option (** (if <test> <consequent> <alternate> option) *)

and definition = variable * expr

and form =
  | Def of definition
  | Expr of expr

and program = form list [@@deriving eq, show { with_path = false }]
