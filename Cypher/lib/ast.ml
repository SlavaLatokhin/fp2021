type const =
  | CString of string
  | CInt of int
[@@deriving show { with_path = false }]

type binop =
  | Plus
  | Minus
  | Star
  | Slash
  | NotEqual
  | Less
  | Greater
  | LessEq
  | GreEq
  | Equal
  | And
  | Or
  | Xor
  | StartWith
  | EndWith
  | Contain
[@@deriving show { with_path = false }]

type unop =
  | Not
  | IsNotNull
  | IsNull
[@@deriving show { with_path = false }]

type expr =
  | EConst of const
  | EGetProp of string * string
  | EGetType of string
  | EGetId of string
  | EGetElm of string
  | EBinop of binop * expr * expr
  | EUnop of unop * expr
[@@deriving show { with_path = false }]

(** {name:"Daniel"} *)
type property = string * expr [@@deriving show { with_path = false }]

type typeedge =
  | Direct of string option * string option * property list option
  | UnDirect of string option * string option * property list option
[@@deriving show { with_path = false }]

(** [edge : PARENT { role: "Father" }] *)
type edgedata = EdgeData of typeedge [@@deriving show { with_path = false }]

(** (node : PERSON { name: "Daniel" }) *)
type nodedata = NodeData of string option * string list option * property list option
[@@deriving show { with_path = false }]

type elm =
  | Node of nodedata (** (nodedata) *)
  | Edge of nodedata * edgedata * nodedata (** (nodedata)-[edgedata]->(nodedata) *)
[@@deriving show { with_path = false }]

type ordercond =
  | Asc
  | Desc
[@@deriving show { with_path = false }]

(** 
Order (expr; None) ~ Order (expr; Some(ASC))
Order (expr; Some(DESC))
*)
type orderby = Order of expr * ordercond option [@@deriving show { with_path = false }]

type cmdmatch =
  | CMatchRet of expr list * orderby option (** RETURN vars *)
  | CMatchDelete of string list (** DETACH DELETE vars *)
  | CMatchCrt of elm list (** CREATE elms *)
[@@deriving show { with_path = false }]

type cmdwithmatch =
  | CMatchWhere of expr (** WHERE node.name = "Daniel" AND node.age < 20 *)
[@@deriving show { with_path = false }]

type command =
  | CmdCreate of elm list (** CREATE elms *)
  | CmdMatch of elm list * cmdwithmatch option * cmdmatch list (** MATCH elms cmdmatch *)
[@@deriving show { with_path = false }]

type program = command list [@@deriving show { with_path = false }]
