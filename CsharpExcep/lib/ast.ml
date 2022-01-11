type types = Int | Bool | CsClass of string | Void | String
[@@deriving show { with_path = false }]

type modifier = Public | Static | Override | Const
[@@deriving show { with_path = false }]

type values =
  | VInt of int
  | VBool of bool
  | VVoid
  | VNull
  | VString of string
  | VClass of object_references
[@@deriving show { with_path = false }]

and field_references = {
  key : string;
  field_type : types;
  field_value : values;
  is_const : bool;
  assignment_count : int;
}

and object_references =
  | ObjNull
  | ObjRef of {
      class_key : string;
      parent_key : string option;
      class_table : (string, field_references) Hashtbl_der.t;
      number : int;
    }

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
  | Null
  | PostInc of expr
  | PostDec of expr
  | PrefInc of expr
  | PrefDec of expr
  | Access of expr * expr
  | ConstExpr of values
  | IdentVar of string
  | ClassCreate of string * expr list
  | CallMethod of string * expr list
  | Assign of expr * expr
[@@deriving show { with_path = false }]

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
      statement (*try-body*)
      * ((types * expr option) option * expr option * statement) list
      (*list of catches*)
      * statement option (*finally-body*)
  | Print of expr
[@@deriving show { with_path = false }]

and field =
  | VariableField of types * (string * expr option) list
  | Method of types * string * (types * string) list * statement
  | Constructor of string * (types * string) list * statement
[@@deriving show { with_path = false }]

and class_dec =
  | Class of
      modifier list
      * string (*class name*)
      * string option
      (*parent class name*)
      * (modifier list * field) list
[@@deriving show { with_path = false }]

let get_obj_value = function VClass o -> o | _ -> ObjNull

let get_default_value = function
  | Int -> VInt 0
  | String -> VString ""
  | CsClass _ -> VClass ObjNull
  | Bool -> VBool false
  | Void -> VVoid

let get_obj_num = function
  | ObjNull -> raise (Invalid_argument "NullReferenceException")
  | ObjRef { number = n; _ } -> n

let get_obj_info = function
  | ObjNull -> raise (Invalid_argument "NullReferenceException")
  | ObjRef { class_key = key; class_table = table; number = n; _ } ->
      (key, table, n)

let get_obj_fields = function
  | ObjNull -> raise (Invalid_argument "NullReferenceException")
  | ObjRef { class_table = frt; _ } -> frt

let get_field_list = function Class (_, _, _, f_list) -> List.map snd f_list

let convert_pair = function
  | t, p_list -> List.map (fun p -> match p with s, f -> (t, s, f)) p_list

let get_field_pairs in_class =
  List.concat
    (List.map convert_pair
       (List.filter_map
          (fun field ->
            match field with
            | VariableField (t_f, pair_list) -> Some (t_f, pair_list)
            | _ -> None)
          (get_field_list in_class)))
