type signal =
  | Work
  | Break
  | Next
  | Return (** to track the last impact in environment *)

type identifier = Identifier of string [@@deriving show { with_path = false }]

type modifier =
  | Local (** starts with lowercase letter *)
  | Instance (** starts with @, might be unique for different object instances *)
  | Global (** starts with $, global variables *)
  | Class (** starts with @@, is the same for all object instances *)
[@@deriving show { with_path = false }]

type value =
  | String of string
  | Integer of int
  | Float of float
  | Boolean of bool
  | Nil (** "null" *)
  | Object of identifier (** shows who's object a value is *)
  | Lambda of identifier list * statement list (** lambda{ |x| x + 1 } *)
  | List of expression list (** calcs [expr -> value] when needed *)
[@@deriving show { with_path = false }]

and expression =
  | Constant of value
  | Variable of modifier * identifier
  | Add of expression * expression
  | Sub of expression * expression
  | Mul of expression * expression
  | Div of expression * expression
  | Mod of expression * expression
  | Equal of expression * expression
  | NotEqual of expression * expression
  | Greater of expression * expression
  | GreaterOrEq of expression * expression
  | Less of expression * expression
  | LessOrEq of expression * expression
  | And of expression * expression
  | Or of expression * expression
  | ListAccess of identifier * expression
  | MonoCall of identifier * expression list (** f() *)
  | PolyCall of identifier * identifier * expression list (** x.f() *)
  | CallLambda of identifier list * statement list * expression list
      (** stand-alone lambda call: [var list] [stmt list] [parameters] ( lambda{ |x| x }.call(1) ) *)
[@@deriving show { with_path = false }]

and statement =
  | Expression of expression
  | Assign of expression * expression
  | MultipleAssign of expression list * expression list (** x, y = 1, 2 *)
  | Return of expression
  | IfElse of expression * statement list * statement list
      (** (expr) [stmt list] [stmt list OR [] in case of "else" absence] *)
  | While of expression * statement list
  | Class of identifier * statement list
  | Function of identifier * identifier list * statement list
      (** (id) [args list] [stmt list] *)
  | Break
  | Next (** "continue" *)
  | Puts of expression (** console output *)
[@@deriving show { with_path = false }]
