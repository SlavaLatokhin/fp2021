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

module KeyMap = struct
  include Map.Make (String)

  let pp pp_v ppf map =
    Format.fprintf ppf "@[[@[" ;
    iter (fun k v -> Format.fprintf ppf "@[\"%s\": %a@],@\n" k pp_v v) map ;
    Format.fprintf ppf "@]]@]"
end

type class_t =
  { class_key: key_t
  ; field_map: field_t KeyMap.t
  ; method_map: method_t KeyMap.t
  ; parent_key: key_t option
  ; dec_class: class_dec }
[@@deriving show {with_path= false}]
