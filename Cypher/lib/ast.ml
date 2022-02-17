type const =
  | CString of string
  | CInt of int
[@@deriving show { with_path = false }]

type binop =
  | Plus
  | Minus
  | Star
  | Slash
[@@deriving show { with_path = false }]

type expr =
  | EConst of const
  | EBinop of binop * expr * expr
[@@deriving show { with_path = false }]

(* {name:"Daniel"} *)
type property = string * expr [@@deriving show { with_path = false }]

(* [edge1 : PARENT { role: "Father" }] *)
type edgedata = EdgeData of string option * string option * property list option
[@@deriving show { with_path = false }]

(* (node1 : PERSON { name: "Daniel" }) *)
type nodedata = NodeData of string option * string list option * property list option
[@@deriving show { with_path = false }]

type elm =
  | Node of nodedata (* (nodedata) *)
  | Edge of nodedata * edgedata * nodedata (* (nodedata)-[edgedata]->(nodedata) *)
[@@deriving show { with_path = false }]

type cmdmatch =
  | CMatchRet of string list (* RETURN vars *)
  | CMatchDelete of string list (* DETACH DELETE vars *)
  | CMatchCrt of elm list (* CREATE elms *)
[@@deriving show { with_path = false }]

type command =
  | CmdCreate of elm list (* CREATE elms *)
  | CmdMatch of elm list * cmdmatch list (* MATCH elms cmdmatch *)
[@@deriving show { with_path = false }]

type program = command list [@@deriving show { with_path = false }]
