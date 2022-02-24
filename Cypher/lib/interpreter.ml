open Ast
open Graph

type value =
  | VString of string
  | VInt of int
  | VBool of bool
  | VNull of string
[@@deriving show { with_path = false }]

type propertyList = (string * value) list [@@deriving show { with_path = false }]

let pp_value_ret fmt = function
  | VString s -> Format.fprintf fmt "%S\n" s
  | VInt n -> Format.fprintf fmt "%d\n" n
  | VBool b -> Format.fprintf fmt "%b\n" b
  | VNull s -> Format.fprintf fmt "%s\n" s
;;

let pp_vproperties_ret fmt =
  List.iter (fun prop ->
      match prop with
      | str, value -> Format.fprintf fmt "   \"%s\": %a " str pp_value_ret value)
;;

let pp_type_ret fmt labels =
  List.iter (fun label -> Format.fprintf fmt "\"%s\",\n" label) labels
;;

let pp_labels_ret fmt labels =
  Format.fprintf fmt "[\n";
  List.iter (fun label -> Format.fprintf fmt "    \"%s\",\n" label) labels;
  Format.fprintf fmt "  ],\n"
;;

type vproperty = string * value [@@deriving show { with_path = false }]
type vproperties = vproperty list [@@deriving show { with_path = false }]

type elm_data_src_dst = int * string list * vproperties
[@@deriving show { with_path = false }]

(** 
Uses the same type for vertices and edges to make 
it easier to store data in a single environment.

Node: (id, labels, vproperties), (None, None) 
Edge: (id. labels, vproperties), (src, dst)
*)
type elm_data =
  (int * string list * vproperties) * (elm_data_src_dst option * elm_data_src_dst option)
[@@deriving show { with_path = false }]

type abstvalue =
  | AValue of value
  | AElm of elm_data
[@@deriving show { with_path = false }]

let gen_sym =
  let id = ref 0 in
  fun () ->
    id := !id + 1;
    !id
;;

module ElmData = struct
  type t = elm_data

  let compare = Stdlib.compare
  let hash = Hashtbl.hash
  let equal = ( = )
  let default : t = (0, [], []), (None, None)
end

module Graph = Persistent.Digraph.ConcreteLabeled (ElmData) (ElmData)

type graph = Graph.t
type env = graph * (string * elm_data) list

let pp_graph fmt graph =
  Format.fprintf fmt "Vertecies:\n";
  Graph.iter_vertex (fun v -> Format.fprintf fmt "%a\n" pp_elm_data v) graph;
  Format.fprintf fmt "--------------------------\n";
  Format.fprintf fmt "Edges:\n";
  Graph.iter_edges_e
    (fun e ->
      match e with
      | n1, e, n2 ->
        Format.fprintf fmt "%a-[%a]->%a\n%!" pp_elm_data n1 pp_elm_data e pp_elm_data n2)
    graph
;;

let ( let* ) m f = Result.bind m f

type err =
  | IncorrectType
  | DivByZero
  | NotBound of string
  | NoLabelEdge of string
  | IncorrectProps
  | UnstblElm
  | ElmNotValid
  | IncorrectWhere of string
  | TypeNotValid of string
  | UnderectNotValidInCrt
[@@deriving show { with_path = false }]

type elms =
  | ENode
  | EEdge

let find_typeofrelat_env varelm env =
  match List.mem_assoc varelm env with
  | true ->
    List.fold_left
      (fun acc oneenv ->
        let* listabstvalue = acc in
        match oneenv with
        | evar, eelm ->
          (match eelm with
          | (_, labels, _), (Some _, Some _) when varelm = evar ->
            (match labels with
            | label :: _ -> Result.ok (AValue (VString label) :: listabstvalue)
            | _ -> Result.ok listabstvalue)
          | (_, _, _), (None, None) when varelm = evar ->
            Result.error
              (TypeNotValid
                 "The type of relationship was expected to be checked, not the node.")
          | (_, _, _), (_, _) -> Result.ok listabstvalue))
      (Result.ok [])
      env
  | false -> Result.error (NotBound "Undefined variable or nothing was found.")
;;

let find_valueofelm_env varelm varfield env =
  match List.mem_assoc varelm env with
  | true ->
    List.fold_left
      (fun acc oneenv ->
        let* listabstvalue = acc in
        match oneenv with
        | evar, eelm ->
          (match eelm with
          | (_, _, props), (_, _) when varelm = evar ->
            (match List.assoc_opt varfield props with
            | Some field -> Result.ok (AValue field :: listabstvalue)
            | None -> Result.ok (AValue (VNull "null") :: listabstvalue))
          | _ -> Result.ok listabstvalue))
      (Result.ok [])
      env
  | false -> Result.error (NotBound "Undefined variable or nothing was found.")
;;

let find_idofelm_env varelm env =
  match List.mem_assoc varelm env with
  | true ->
    List.fold_left
      (fun acc oneenv ->
        let* listabstvalue = acc in
        match oneenv with
        | evar, eelm ->
          (match eelm with
          | (id, _, _), (_, _) when varelm = evar ->
            Result.ok (AValue (VInt id) :: listabstvalue)
          | _ -> Result.ok listabstvalue))
      (Result.ok [])
      env
  | false -> Result.error (NotBound "Undefined variable or nothing was found.")
;;

let find_elm_env varelm env =
  match List.mem_assoc varelm env with
  | true ->
    List.fold_left
      (fun acc oneenv ->
        let* listabstvalue = acc in
        match oneenv with
        | evar, eelm when varelm = evar -> Result.ok (AElm eelm :: listabstvalue)
        | _ -> Result.ok listabstvalue)
      (Result.ok [])
      env
  | false -> Result.error (NotBound "Undefined variable or nothing was found.")
;;

let check_var_props var props elm field =
  match var = elm with
  | true ->
    (match List.assoc_opt field props with
    | Some field -> Result.ok field
    | None -> Result.ok (VNull "null"))
  | false -> Result.error UnstblElm
;;

let check_var_id id var elm =
  match var = elm with
  | true -> Result.ok (VInt id)
  | false -> Result.error UnstblElm
;;

let rec interpret_rec_expr_where id expr var props =
  match expr with
  | EConst (CString s) -> Result.ok (VString s)
  | EConst (CInt n) -> Result.ok (VInt n)
  | EGetProp (elm, field) -> check_var_props var props elm field
  | EGetId elm -> check_var_id id var elm
  | EGetType _ ->
    Result.error (IncorrectWhere "The request needs to be modified. ()-[r: TYPE]-()")
  | EGetElm _ -> Result.error ElmNotValid
  | EUnop (op, expr) ->
    let* expr = interpret_rec_expr_where id expr var props in
    (match op, expr with
    | Not, VBool expr -> Result.ok (VBool (not expr))
    | IsNotNull, expr ->
      (match expr with
      | VNull _ -> Result.ok (VBool false)
      | _ -> Result.ok (VBool true))
    | IsNull, expr ->
      (match expr with
      | VNull _ -> Result.ok (VBool true)
      | _ -> Result.ok (VBool false))
    | _ -> Result.error IncorrectType)
  | EBinop (op, n, m) ->
    let* n = interpret_rec_expr_where id n var props in
    let* m = interpret_rec_expr_where id m var props in
    (match op, n, m with
    | Plus, VInt n, VInt m -> Result.ok (VInt (n + m))
    | Minus, VInt n, VInt m -> Result.ok (VInt (n - m))
    | Star, VInt n, VInt m -> Result.ok (VInt (n * m))
    | Slash, VInt n, VInt m -> Result.ok (VInt (n / m))
    | NotEqual, _, _ -> Result.ok (VBool (n <> m))
    | Less, _, _ -> Result.ok (VBool (n < m))
    | Greater, _, _ -> Result.ok (VBool (n > m))
    | LessEq, _, _ -> Result.ok (VBool (n <= m))
    | GreEq, _, _ -> Result.ok (VBool (n >= m))
    | Equal, _, _ -> Result.ok (VBool (n = m))
    | And, VBool n, VBool m -> Result.ok (VBool (n && m))
    | Or, VBool n, VBool m -> Result.ok (VBool (n || m))
    | Xor, VBool n, VBool m -> Result.ok (VBool (n <> m))
    | StartWith, VString n, VString m ->
      Result.ok (VBool (Str.string_match (Str.regexp_string m) n 0))
    | EndWith, VString n, VString m ->
      Result.ok
        (VBool
           (Str.string_match (Str.regexp_string m) n (String.length n - String.length m)))
    | Contain, VString n, VString m ->
      Result.ok
        (VBool
           (try
              ignore (Str.search_forward (Str.regexp_string m) n 0);
              true
            with
           | Not_found -> false))
    | _ -> Result.error IncorrectType)
;;

(** 
Initially, the interpreter checks for WHERE 
that WHERE will return boolean. 
*)
let interpret_expr_where id expr var props =
  match expr with
  | EUnop (op, expr) ->
    let* expr = interpret_rec_expr_where id expr var props in
    (match op, expr with
    | Not, VBool expr -> Result.ok (not expr)
    | IsNotNull, expr ->
      (match expr with
      | VNull _ -> Result.ok false
      | _ -> Result.ok true)
    | IsNull, expr ->
      (match expr with
      | VNull _ -> Result.ok true
      | _ -> Result.ok false)
    | _ -> Result.error IncorrectType)
  | EBinop (op, n, m) ->
    let* n = interpret_rec_expr_where id n var props in
    let* m = interpret_rec_expr_where id m var props in
    (match op with
    | NotEqual -> Result.ok (n <> m)
    | Less -> Result.ok (n < m)
    | Greater -> Result.ok (n > m)
    | LessEq -> Result.ok (n <= m)
    | GreEq -> Result.ok (n >= m)
    | Equal -> Result.ok (n = m)
    | And ->
      (match n, m with
      | VBool n, VBool m -> Result.ok (n && m)
      | _, _ -> Result.error IncorrectType)
    | Or ->
      (match n, m with
      | VBool n, VBool m -> Result.ok (n || m)
      | _, _ -> Result.error IncorrectType)
    | Xor ->
      (match n, m with
      | VBool n, VBool m -> Result.ok (n <> m)
      | _, _ -> Result.error IncorrectType)
    | StartWith ->
      (match n, m with
      | VString n, VString m -> Result.ok (Str.string_match (Str.regexp_string m) n 0)
      | _, _ -> Result.error IncorrectType)
    | EndWith ->
      (match n, m with
      | VString n, VString m ->
        Result.ok
          (Str.string_match (Str.regexp_string m) n (String.length n - String.length m))
      | _, _ -> Result.error IncorrectType)
    | Contain ->
      (match n, m with
      | VString n, VString m ->
        Result.ok
          (try
             ignore (Str.search_forward (Str.regexp_string m) n 0);
             true
           with
          | Not_found -> false)
      | _, _ -> Result.error IncorrectType)
    | _ -> Result.error (IncorrectWhere "The request needs to be modified."))
  | _ -> Result.error (IncorrectWhere "The request needs to be modified.")
;;

let rec interpret_expr env expr =
  match expr with
  | EConst (CString s) -> Result.ok [ AValue (VString s) ]
  | EConst (CInt n) -> Result.ok [ AValue (VInt n) ]
  | EGetProp (elm, field) -> find_valueofelm_env elm field env
  | EGetType elm -> find_typeofrelat_env elm env
  | EGetId elm -> find_idofelm_env elm env
  | EGetElm elm -> find_elm_env elm env
  | EUnop (op, expr) ->
    let* abstvalues = interpret_expr env expr in
    List.fold_left
      (fun acc expr ->
        let* listabstvalue = acc in
        match op, expr with
        | Not, AValue (VBool expr) ->
          Result.ok (listabstvalue @ [ AValue (VBool (not expr)) ])
        | IsNotNull, AValue value ->
          (match value with
          | VNull _ -> Result.ok listabstvalue
          | _ -> Result.ok (listabstvalue @ [ AValue value ]))
        | IsNull, AValue value ->
          (match value with
          | VNull _ -> Result.ok (listabstvalue @ [ AValue value ])
          | _ -> Result.ok listabstvalue)
        | _ -> Result.error IncorrectType)
      (Result.ok [])
      abstvalues
  | EBinop (op, n, m) ->
    let* n = interpret_expr env n in
    let* m = interpret_expr env m in
    List.fold_left
      (fun acc n ->
        let* listabstvalue = acc in
        List.fold_left
          (fun acc m ->
            let* listabstvalue = acc in
            match n, m with
            | AValue n, AValue m ->
              (match op, n, m with
              | Plus, VInt n, VInt m ->
                Result.ok (listabstvalue @ [ AValue (VInt (n + m)) ])
              | Minus, VInt n, VInt m ->
                Result.ok (listabstvalue @ [ AValue (VInt (n - m)) ])
              | Star, VInt n, VInt m ->
                Result.ok (listabstvalue @ [ AValue (VInt (n * m)) ])
              | Slash, VInt n, VInt m ->
                Result.ok (listabstvalue @ [ AValue (VInt (n / m)) ])
              | NotEqual, VInt n, VInt m ->
                Result.ok (listabstvalue @ [ AValue (VBool (n <> m)) ])
              | Less, VInt n, VInt m ->
                Result.ok (listabstvalue @ [ AValue (VBool (n < m)) ])
              | Greater, VInt n, VInt m ->
                Result.ok (listabstvalue @ [ AValue (VBool (n > m)) ])
              | LessEq, VInt n, VInt m ->
                Result.ok (listabstvalue @ [ AValue (VBool (n <= m)) ])
              | GreEq, VInt n, VInt m ->
                Result.ok (listabstvalue @ [ AValue (VBool (n >= m)) ])
              | Equal, VInt n, VInt m ->
                Result.ok (listabstvalue @ [ AValue (VBool (n = m)) ])
              | And, VBool n, VBool m ->
                Result.ok (listabstvalue @ [ AValue (VBool (n && m)) ])
              | Or, VBool n, VBool m ->
                Result.ok (listabstvalue @ [ AValue (VBool (n || m)) ])
              | Xor, VBool n, VBool m ->
                Result.ok (listabstvalue @ [ AValue (VBool (n <> m)) ])
              | StartWith, VString n, VString m ->
                Result.ok
                  (listabstvalue
                  @ [ AValue (VBool (Str.string_match (Str.regexp_string m) n 0)) ])
              | EndWith, VString n, VString m ->
                Result.ok
                  (listabstvalue
                  @ [ AValue
                        (VBool
                           (Str.string_match
                              (Str.regexp_string m)
                              n
                              (String.length n - String.length m)))
                    ])
              | Contain, VString n, VString m ->
                Result.ok
                  (listabstvalue
                  @ [ AValue
                        (VBool
                           (try
                              ignore (Str.search_forward (Str.regexp_string m) n 0);
                              true
                            with
                           | Not_found -> false))
                    ])
              | _ -> Result.error IncorrectType)
            | _ -> Result.error IncorrectType)
          (Result.ok listabstvalue)
          m)
      (Result.ok [])
      n
;;

let rec interpret_expr_for_ret = function
  | EConst (CString s) -> Result.ok s
  | EConst (CInt n) -> Result.ok (Int.to_string n)
  | EGetProp (elm, field) -> Result.ok (String.concat "" [ elm; "."; field ])
  | EGetType elm -> Result.ok (String.concat "" [ "type"; "("; elm; ")" ])
  | EGetId elm -> Result.ok (String.concat "" [ "id"; "("; elm; ")" ])
  | EGetElm elm -> Result.ok elm
  | EUnop (op, expr) ->
    let* elm = interpret_expr_for_ret expr in
    (match op with
    | Not -> Result.ok (String.concat "" [ "NOT"; elm ])
    | IsNotNull -> Result.ok (String.concat "" [ elm; "IS NOT NULL" ])
    | IsNull -> Result.ok (String.concat "" [ elm; "IS NULL" ]))
  | EBinop (op, n, m) ->
    let* n = interpret_expr_for_ret n in
    let* m = interpret_expr_for_ret m in
    let listabstvalue = "" in
    (match op with
    | Plus -> Result.ok (String.concat "" [ listabstvalue; n; "+"; m ])
    | Minus -> Result.ok (String.concat "" [ listabstvalue; n; "-"; m ])
    | Star -> Result.ok (String.concat "" [ listabstvalue; n; "*"; m ])
    | Slash -> Result.ok (String.concat "" [ listabstvalue; n; "/"; m ])
    | NotEqual -> Result.ok (String.concat "" [ listabstvalue; n; "<>"; m ])
    | Less -> Result.ok (String.concat "" [ listabstvalue; n; "<"; m ])
    | Greater -> Result.ok (String.concat "" [ listabstvalue; n; ">"; m ])
    | LessEq -> Result.ok (String.concat "" [ listabstvalue; n; "<="; m ])
    | GreEq -> Result.ok (String.concat "" [ listabstvalue; n; ">="; m ])
    | Equal -> Result.ok (String.concat "" [ listabstvalue; n; "="; m ])
    | And -> Result.ok (String.concat "" [ listabstvalue; n; " AND "; m ])
    | Or -> Result.ok (String.concat "" [ listabstvalue; n; " OR "; m ])
    | Xor -> Result.ok (String.concat "" [ listabstvalue; n; " XOR "; m ])
    | StartWith -> Result.ok (String.concat "" [ listabstvalue; n; " STARTS WITH "; m ])
    | EndWith -> Result.ok (String.concat "" [ listabstvalue; n; " ENDS WITH "; m ])
    | Contain -> Result.ok (String.concat "" [ listabstvalue; n; " CONTAINS "; m ]))
;;

let get_props env props =
  List.fold_left
    (fun acc (name, expr) ->
      let* abstvaluelist = interpret_expr env expr in
      match abstvaluelist with
      | abstvalue :: _ ->
        (match abstvalue with
        | AValue value ->
          let* acc = acc in
          Result.ok ((name, value) :: acc)
        | AElm _ -> Result.error IncorrectProps)
      | _ -> Result.error IncorrectProps)
    (Result.ok [])
    props
;;

let save_node var label vproperties graph env =
  let node = (gen_sym (), label, vproperties), (None, None) in
  Result.ok
    ( (* Format.fprintf Format.std_formatter "Vertex created\n%!"; *)
      Graph.add_vertex graph node
    , (match var with
      | None -> env
      | Some var -> (var, node) :: env)
    , node )
;;

let add_node var label graph env = function
  | Some props ->
    let* vproperties = get_props env props in
    (match label with
    | Some label -> save_node var label vproperties graph env
    | None -> save_node var [] vproperties graph env)
  | None ->
    (match label with
    | Some label -> save_node var label [] graph env
    | None -> save_node var [] [] graph env)
;;

let peek_or_add_node env graph nnodes = function
  | NodeData (var, label, properties) ->
    let varopt = var in
    (match var with
    | Some var ->
      (match List.assoc_opt var env with
      | None ->
        (match add_node varopt label graph env properties with
        | Ok (graph, env, node1) -> Result.ok (graph, env, node1, nnodes + 1)
        | Error err -> Result.error err)
      | Some n1 ->
        (match n1 with
        | n1 -> Result.ok (graph, env, n1, nnodes)))
    | None ->
      (match add_node None label graph env properties with
      | Ok (graph, env, node1) -> Result.ok (graph, env, node1, nnodes + 1)
      | Error err -> Result.error err))
;;

let save_edge var label vproperties graph env n1 n2 =
  match n1, n2 with
  | (elm1, (_, _)), (elm2, (_, _)) ->
    let edge = (gen_sym (), [ label ], vproperties), (Some elm1, Some elm2) in
    Result.ok
      ( (* Format.fprintf Format.std_formatter "Edge created\n%!"; *)
        Graph.add_edge_e graph (n1, edge, n2)
      , (match var with
        | None -> env
        | Some var -> (var, edge) :: env)
      , edge )
;;

let add_edge var graph env n1 n2 label = function
  | Some props ->
    let* vproperties = get_props env props in
    save_edge var label vproperties graph env n1 n2
  | None -> save_edge var label [] graph env n1 n2
;;

let check_data ddatas datas var elm env fdatas =
  match List.length ddatas <= List.length datas with
  | true ->
    (match List.for_all (fun ddata -> List.mem ddata datas) ddatas with
    | true -> Result.ok ((var, elm) :: env, (var, elm) :: fdatas)
    | false -> Result.ok (env, fdatas))
  | false -> Result.ok (env, fdatas)
;;

let check_data_with_where id dprops props expr var =
  match List.length dprops <= List.length props with
  | true ->
    (match List.for_all (fun dprop -> List.mem dprop props) dprops with
    | true -> interpret_expr_where id expr var props
    | false -> Result.ok false)
  | false -> Result.ok false
;;

let send_check_datas_without_where dprops env labels props var elm fdatas = function
  | Some dlabels ->
    (match dprops with
    | Some dprops ->
      let* dprops = get_props env dprops in
      (match List.length dlabels <= List.length labels with
      | true ->
        (match List.for_all (fun dlabel -> List.mem dlabel labels) dlabels with
        | true -> check_data dprops props var elm env fdatas
        | false -> Result.ok (env, fdatas))
      | false -> Result.ok (env, fdatas))
    | None -> check_data dlabels labels var elm env fdatas)
  | None ->
    (match dprops with
    | Some dprops ->
      let* dprops = get_props env dprops in
      check_data dprops props var elm env fdatas
    | None -> Result.ok ((var, elm) :: env, (var, elm) :: fdatas))
;;

let send_check_datas_with_where id dprops env labels props var expr = function
  | Some dlabels ->
    (match dprops with
    | Some dprops ->
      let* dprops = get_props env dprops in
      (match List.length dlabels <= List.length labels with
      | true ->
        (match List.for_all (fun dlabel -> List.mem dlabel labels) dlabels with
        | true -> check_data_with_where id dprops props expr var
        | false -> Result.ok false)
      | false -> Result.ok false)
    | None ->
      (match List.length dlabels <= List.length labels with
      | true ->
        (match List.for_all (fun dlabel -> List.mem dlabel labels) dlabels with
        | true -> interpret_expr_where id expr var props
        | false -> Result.ok false)
      | false -> Result.ok false))
  | None ->
    (match dprops with
    | Some dprops ->
      let* dprops = get_props env dprops in
      check_data_with_where id dprops props expr var
    | None -> interpret_expr_where id expr var props)
;;

let send_check_datas id dprops props labels var elm env fdatas dlabels typeofelm
  = function
  | None -> send_check_datas_without_where dprops env labels props var elm fdatas dlabels
  | Some (CMatchWhere expr) ->
    let booln = send_check_datas_with_where id dprops env labels props var expr dlabels in
    (match booln with
    | Ok true -> Result.ok ((var, elm) :: env, (var, elm) :: fdatas)
    | Ok false -> Result.ok (env, fdatas)
    | Error UnstblElm ->
      (match typeofelm with
      | EEdge -> Result.ok ((var, elm) :: env, (var, elm) :: fdatas)
      | ENode -> Result.ok (env, fdatas))
    | Error err -> Result.error err)
;;

let iter_edges fedges var dlabels dprops cmdwithmatch env fnode1 fnode2 stbledges =
  List.fold_left
    (fun acc fedge ->
      match fedge with
      | _, fedata, _ ->
        (match fedata with
        | (id, labels, props), (_, _) ->
          let* env, stbledges = acc in
          let stbledges_len = List.length stbledges in
          let* _, stbledges =
            send_check_datas
              id
              dprops
              props
              labels
              (Option.value var ~default:"")
              fedata
              env
              stbledges
              (match dlabels with
              | None -> None
              | Some dlabels -> Some [ dlabels ])
              EEdge
              cmdwithmatch
          in
          (match List.length stbledges > stbledges_len with
          | true ->
            Result.ok
              ( fnode1 :: (Option.value var ~default:"", fedata) :: fnode2 :: env
              , stbledges )
          | false -> Result.ok (env, stbledges))))
    (Result.Ok (env, stbledges))
    fedges
;;

(** 
The function finds all edges between two nodes, 
iterates one by one and sends it for comparison 
with user-specified data. 
*)
let check_type_edges cmdwithmatch graph env fnode1 fnode2 stbledges elm1 elm2 e =
  match e with
  | EdgeData (Direct (var, dlabels, dprops)) ->
    iter_edges
      (Graph.find_all_edges graph elm1 elm2)
      var
      dlabels
      dprops
      cmdwithmatch
      env
      fnode1
      fnode2
      stbledges
  | EdgeData (UnDirect (var, dlabels, dprops)) ->
    iter_edges
      (Graph.find_all_edges graph elm1 elm2 @ Graph.find_all_edges graph elm2 elm1)
      var
      dlabels
      dprops
      cmdwithmatch
      env
      fnode1
      fnode2
      stbledges
;;

let find_nodes cmdwithmatch var dlabels dprops graph env typeofelm =
  Graph.fold_vertex
    (fun v acc ->
      match v with
      | (id, labels, props), (_, _) ->
        let* env, fnodes = acc in
        send_check_datas
          id
          dprops
          props
          labels
          var
          v
          env
          fnodes
          dlabels
          typeofelm
          cmdwithmatch)
    graph
    (Result.ok (env, []))
;;

let find_edges cmdwithmatch n1 e n2 graph env =
  let* _, fnodes1 =
    match n1 with
    | NodeData (var, label, properties) ->
      find_nodes
        cmdwithmatch
        (Option.value var ~default:"")
        label
        properties
        graph
        []
        EEdge
  in
  let* _, fnodes2 =
    match n2 with
    | NodeData (var, label, properties) ->
      find_nodes
        cmdwithmatch
        (Option.value var ~default:"")
        label
        properties
        graph
        []
        EEdge
  in
  List.fold_left
    (fun acc fnode1 ->
      let* env, stbledges = acc in
      List.fold_left
        (fun acc fnode2 ->
          let* env, stbledges = acc in
          match fnode1, fnode2 with
          | (_, elm1), (_, elm2) ->
            check_type_edges cmdwithmatch graph env fnode1 fnode2 stbledges elm1 elm2 e)
        (Result.Ok (env, stbledges))
        fnodes2)
    (Result.Ok (env, []))
    fnodes1
;;

let interp_crt elms graph env =
  let* graph, env, nnode, nedges =
    List.fold_left
      (fun acc elm ->
        let* graph, env, nnodes, nedges = acc in
        match elm with
        | Node nodedata ->
          (match peek_or_add_node env graph nnodes nodedata with
          | Ok (graph, env, _, nnodes) -> Result.ok (graph, env, nnodes, nedges)
          | Error err -> Result.error err)
        | Edge (n1, e, n2) ->
          (match e with
          | EdgeData (Direct (var, label, properties)) ->
            (match label with
            | Some label ->
              let* graph, env, n1, nnodes = peek_or_add_node env graph nnodes n1 in
              let* graph, env, n2, nnodes = peek_or_add_node env graph nnodes n2 in
              (match add_edge var graph env n1 n2 label properties with
              | Ok (graph, env, _) -> Result.ok (graph, env, nnodes, nedges + 1)
              | Error err -> Result.error err)
            | None ->
              Result.error (NoLabelEdge "To create an edge, you must specify a label."))
          | EdgeData _ -> Result.error UnderectNotValidInCrt))
      (Result.ok (graph, env, 0, 0))
      elms
  in
  if nnode <> 0 then Format.fprintf Format.std_formatter "Was created %d nodes\n%!" nnode;
  if nedges <> 0
  then Format.fprintf Format.std_formatter "Was created %d edges\n%!" nedges;
  Result.ok (graph, env)
;;

let exe_cmd_vars eelm src dst graph env =
  match src, dst with
  | Some src, Some dst ->
    Result.ok (Graph.remove_edge graph (src, (None, None)) (dst, (None, None)), env)
  | _, _ -> Result.ok (Graph.remove_vertex graph eelm, env)
;;

(** The function can be used for all commands followed by a list of elements.*)
let interp_cmd_vars vars graph env =
  match List.length vars <= List.length env with
  | true ->
    (match List.for_all (fun var -> List.mem_assoc var env) vars with
    | true ->
      List.fold_left
        (fun acc var ->
          let* graph, env = acc in
          List.fold_left
            (fun acc oneenv ->
              let* graph, env = acc in
              match oneenv with
              | evar, eelm ->
                (match eelm with
                | (_, _, _), (src, dst) when var = evar ->
                  exe_cmd_vars eelm src dst graph env
                | _ -> Result.ok (graph, env)))
            (Result.ok (graph, env))
            env)
        (Result.ok (graph, env))
        vars
    | false -> Result.error (NotBound "Undefined variable or nothing was found."))
  | false -> Result.error (NotBound "Undefined variable or nothing was found.")
;;

let sep_env varelm env =
  match List.mem_assoc varelm env with
  | true ->
    List.fold_left
      (fun acc oneenv ->
        let* listorder, listdef = acc in
        match oneenv with
        | evar, _ when varelm = evar -> Result.ok (oneenv :: listorder, listdef)
        | _ -> Result.ok (listorder, oneenv :: listdef))
      (Result.ok ([], []))
      env
  | false -> Result.error (NotBound "Undefined variable or nothing was found.")
;;

(**
"Failwith" in this function is written to get rid of the warning. 
The code won't get here. In the case of "IncorrectType", 
the program will stop using "Result.error". 
But this will not happen either, since only 3 types described 
below are supported at the parsing stage.
*)
let sort_env expr env cond =
  let* listorder, listdef =
    match expr with
    | EGetProp (varelm, _) -> sep_env varelm env
    | EGetId varelm -> sep_env varelm env
    | EGetElm varelm -> sep_env varelm env
    | _ -> Result.error IncorrectType
  in
  let newlistorder =
    List.stable_sort
      (fun elm1 elm2 ->
        match elm1, elm2 with
        | (_, ((id1, _, props1), (_, _))), (_, ((id2, _, props2), (_, _))) ->
          (match expr with
          | EGetProp (_, field) ->
            (match List.assoc_opt field props1, List.assoc_opt field props2 with
            | Some field1, Some field2 -> cond * compare field1 field2
            | Some _, None -> cond
            | None, Some _ -> cond
            | None, None -> 0)
          | EGetElm _ -> cond * compare id1 id2
          | EGetId _ -> cond * compare id1 id2
          | _ -> failwith "Incorrect type"))
      listorder
  in
  Result.ok (newlistorder @ listdef)
;;

let interp_ret exprs orderby graph env =
  let* env =
    match orderby with
    | Some (Order (expr, cond)) ->
      let condascdesc = 1 in
      (match cond with
      | Some Asc -> sort_env expr env (-condascdesc)
      | Some Desc -> sort_env expr env condascdesc
      | None -> sort_env expr env (-condascdesc))
    | None -> Result.ok env
  in
  let* listret =
    List.fold_left
      (fun acc expr ->
        let* listret = acc in
        let* abstvaluelist = interpret_expr env expr in
        let exprabstvaluelist = expr, abstvaluelist in
        Result.ok (listret @ [ exprabstvaluelist ]))
      (Result.ok [])
      exprs
  in
  List.fold_left
    (fun acc exprelmret ->
      let expr, elmsret = exprelmret in
      let* expr = interpret_expr_for_ret expr in
      Format.fprintf Format.std_formatter "-------------------------------\n%s\n%!" expr;
      let* graph, env = acc in
      List.fold_left
        (fun acc elmret ->
          let* graph, env = acc in
          match elmret with
          | AValue value ->
            pp_value_ret Format.std_formatter value;
            Result.ok (graph, env)
          | AElm elm ->
            (match elm with
            | (id, labels, props), (src, dst) ->
              (match src, dst with
              | Some (idsrc, _, _), Some (iddst, _, _) ->
                Format.fprintf
                  Format.std_formatter
                  "{\n\
                  \  \"identity\": %d,\n\
                  \  \"start\": %d,\n\
                  \  \"end\": %d,\n\
                  \  \"type\": %a  \"properties\": {\n\
                  \ %a  }\n\
                   }\n\n"
                  id
                  idsrc
                  iddst
                  pp_type_ret
                  labels
                  pp_vproperties_ret
                  props;
                Result.ok (graph, env)
              | _, _ ->
                Format.fprintf
                  Format.std_formatter
                  "{\n\
                  \  \"identity\": %d,\n\
                  \  \"labels\": %a  \"properties\": {\n\
                  \ %a  }\n\
                   }\n\n"
                  id
                  pp_labels_ret
                  labels
                  pp_vproperties_ret
                  props;
                Result.ok (graph, env))))
        (Result.ok (graph, env))
        elmsret)
    (Result.ok (graph, env))
    listret
;;

let interp_match elms env commands cmdwithmatch =
  let* env =
    List.fold_left
      (fun acc elm ->
        let* graph, env = acc in
        match elm with
        | Node nodedata ->
          (match nodedata with
          | NodeData (var, label, properties) ->
            (match var with
            | Some var ->
              let* env, _ =
                find_nodes cmdwithmatch var label properties graph env ENode
              in
              Result.ok (graph, env)
            | None -> Result.ok (graph, env)))
        | Edge (n1, e, n2) ->
          let* env, _ = find_edges cmdwithmatch n1 e n2 graph env in
          Result.ok (graph, env))
      (Result.ok env)
      elms
  in
  List.fold_left
    (fun acc cmd ->
      let* graph, env = acc in
      match cmd with
      | CMatchCrt elms ->
        (match interp_crt elms graph env with
        | Ok (graph, env) -> Result.ok (graph, env)
        | Error err -> Result.error err)
      | CMatchRet (exprs, orderby) -> interp_ret exprs orderby graph env
      | CMatchDelete vars -> interp_cmd_vars vars graph env)
    (Result.ok env)
    commands
;;

let interpret_command env = function
  | CmdCreate elms ->
    let graph, env = env in
    (match interp_crt elms graph env with
    | Ok (graph, env) -> Result.ok (graph, env)
    | Error err -> Result.error err)
  | CmdMatch (elms, cmdwithmatch, commands) -> interp_match elms env commands cmdwithmatch
;;

let interpret_program commands =
  let graph = Graph.empty in
  List.fold_left
    (fun acc command ->
      let* graph, _ = acc in
      match interpret_command (graph, []) command with
      | Error err -> Result.error err
      | Ok (graph, _) -> Result.ok (graph, []))
    (Result.ok (graph, []))
    commands
;;
