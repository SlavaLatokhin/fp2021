(**  [a-z_][0-9a-zA-Z_] *)
type id = string [@@deriving show { with_path = false }]

type binder = int [@@deriving show { with_path = false }]

type bin_op =
  | And (**  &&  *)
  | Or (**  ||  *)
  | Less (**  <   *)
  | Leq (**  <=  *)
  | Gre (**  >   *)
  | Geq (**  >=  *)
  | Eq (** =   *)
  | Neq (**  <>  *)
  | Add (**  +   *)
  | Sub (**  -   *)
  | Mul (**  *   *)
  | Div (**  /   *)
[@@deriving show { with_path = false }]

and binding = bool * pt * exp [@@deriving show { with_path = false }]

and case = pt * exp [@@deriving show { with_path = false }]

and decl = DLet of binding (**  let y = 256   *) [@@deriving show { with_path = false }]

and prog = decl list [@@deriving show { with_path = false }]

and un_op =
  | Not (**  not  *)
  | Minus (** - *)
[@@deriving show { with_path = false }]

and arg =
  | Expr of exp
  | Labeled of id * exp

and const =
  | CString of string (**  "xyz"  *)
  | CInt of int (**   256    *)
  | CBool of bool (**  false   *)
[@@deriving show { with_path = false }]

and exp =
  | EConst of const (**    false    *)
  | ENone (**    none    *)
  | ENil (** [] *)
  | EUnOp of un_op * exp (**    not x    *)
  | EVar of id (**    x    *)
  | ECons of exp * exp (**    x :: xs    *)
  | ETuple of exp list (**    x, y, z    *)
  | ELet of binding list * exp (**    let x = 256 in 512    *)
  | EFun of pt * exp (**    fun x,y,z -> x / y - z    *)
  | EMatch of exp * case list (**    match lst    *)
  | ESome of exp (**    Some a    *)
  | EArg of arg (**    arg    *)
  | EIf of exp * exp * exp (**    if predicate then x else y    *)
  | EOp of bin_op * exp * exp (**    25 / (7 - 2)    *)
  | EApp of exp * exp (**    fold a list init    *)
[@@deriving show { with_path = false }]

and pt =
  | PWild (**  _  *)
  | PVar of id (**  xyz   *)
  | PConst of const (**  256   *)
  | PCons of pt * pt (**  hd :: tl  *)
  | PSome of pt (**   Some a  *)
  | PLabeled of id (**  ~x  *)
  | POptional of id * exp (**  ?(step = 1)  *)
  | PNone (**  none  *)
  | PNil (**  []  *)
  | PTuple of pt list (**  x, y, z   *)
[@@deriving show { with_path = false }]
