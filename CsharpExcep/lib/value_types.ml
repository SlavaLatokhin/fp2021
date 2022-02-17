open Ast

type key_t = string [@@deriving show]

type field_t =
  {field_type: types; field_key: key_t; is_const: bool; sub_tree: expr option}
[@@deriving show {with_path= false}]

type method_t =
  { method_type: types
  ; method_key: key_t
  ; args: (types * string) list
  ; body: statement }
[@@deriving show {with_path= false}]

type class_t =
  { class_key: key_t
  ; field_table: (key_t, field_t) Hashtbl_der.t
  ; method_table: (key_t, method_t) Hashtbl_der.t
  ; parent_key: key_t option
  ; dec_class: class_dec }
[@@deriving show {with_path= false}]
