type types = Int | Bool | CsClass of string | Void | String
[@@deriving show {with_path= false}]

type modifier = Public | Static | Override | Const
[@@deriving show {with_path= false}]

type values =
  | VInt of int
  | VBool of bool
  | VVoid
  | VNull
  | VString of string
  | VClass of object_references
[@@deriving show {with_path= false}]

and object_references =
  | ObjNull
  | ObjRef of string * string option (*class key and parent key*)
[@@deriving show {with_path= false}]

type expr =
  | Plus of expr * expr
  | Min of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Equal of expr * expr
  | NotEqual of expr * expr
  | Less of expr * expr
  | More of expr * expr
  | LessOrEqual of expr * expr
  | MoreOrEqual of expr * expr
  | PostInc of expr
  | PostDec of expr
  | PrefInc of expr
  | PrefDec of expr
  | Null
  | ConstExpr of values
  | IdentVar of string
  | ClassCreate of string * expr list
  | CallMethod of string * expr list
  | Assign of expr * expr
[@@deriving show {with_path= false}]

and statement =
  | Expression of expr
  | StatementBlock of statement list
  | If of expr * statement * statement option
  | For of statement option * expr option * expr list * statement
  | While of expr * statement
  | Break
  | Continue
  | Return of expr option
  | VarDeclare of modifier option * types * (string * expr option) list
  | Throw of expr
  | Try of
      statement (* try-body*)
      * (types option * statement) list
      (* list of catches*)
      * statement option (* finally-body*)
  | Print of expr
[@@deriving show {with_path= false}]

and field =
  | VariableField of types * (string * expr option) list
  | Method of types * string * (types * string) list * statement
[@@deriving show {with_path= false}]

and class_dec =
  | Class of
      modifier list
      * string (* class name*)
      * string option
      (* parent class name*)
      * (modifier list * field) list
[@@deriving show {with_path= false}]
