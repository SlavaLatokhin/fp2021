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
  ; field_list: field_t list
  ; method_list: method_t list
  ; parent_key: key_t option
  ; dec_class: class_dec }
[@@deriving show {with_path= false}]

let add_field class_t field_t =
  {class_t with field_list= field_t :: class_t.field_list}

let add_method class_t method_t =
  {class_t with method_list= method_t :: class_t.method_list}

let find_opt_field field_list field_key =
  List.find_opt (fun (x : field_t) -> field_key = x.field_key) field_list

let find_opt_method method_list method_key =
  List.find_opt (fun (x : method_t) -> method_key = x.method_key) method_list

let find_opt_class class_list class_key =
  List.find_opt (fun (x : class_t) -> class_key = x.class_key) class_list

let find_field field_list field_key =
  List.find (fun (x : field_t) -> field_key = x.field_key) field_list

let find_method method_list method_key =
  List.find (fun (x : method_t) -> method_key = x.method_key) method_list
