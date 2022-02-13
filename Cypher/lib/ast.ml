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

type property = string * expr [@@deriving show { with_path = false }]

type edgedata = Edgedata of string option * string * property list option
[@@deriving show { with_path = false }]

type nodedata = Nodedata of string option * string list option * property list option
[@@deriving show { with_path = false }]

type elm =
  | Node of nodedata
  | Edge of nodedata * edgedata * nodedata (* (s1)-[:s2]->(s2) *)
[@@deriving show { with_path = false }]

type cmdmatch =
  | CMatchRet of string list
  | CMatchCrt of elm list
  | CMatchWhere of string list
[@@deriving show { with_path = false }]

type command =
  | CmdCreate of elm list
  | CmdMatch of elm list * cmdmatch list
(* | CmdReturn of string list *)
[@@deriving show { with_path = false }]

type program = command list [@@deriving show { with_path = false }]
