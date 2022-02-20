open Ast
open Graph

type value =
  | VString of string
  | VInt of int
  | VBool of bool
[@@deriving show { with_path = false }]

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

let gen_sum =
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
  | NotExistField of string
  | UnstblElm
  | ElmNotValid
  | IncorrectWhere
[@@deriving show { with_path = false }]

type elms =
  | ENode
  | EEdge

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
            | None -> Result.ok (AValue (VString "null") :: listabstvalue))
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
    | None -> Result.error (NotExistField "This element does not have this property"))
  | false -> Result.error UnstblElm
;;

let rec interpret_rec_expr_where expr var props =
  match expr with
  | EConst (CString s) -> Result.ok (VString s)
  | EConst (CInt n) -> Result.ok (VInt n)
  | EGetProp (elm, field) -> check_var_props var props elm field
  | EGetElm _ -> Result.error ElmNotValid
  | EBinop (op, n, m) ->
    let* n = interpret_rec_expr_where n var props in
    let* m = interpret_rec_expr_where m var props in
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
    | _ -> Result.error IncorrectType)
;;

(** 
Initially, the interpreter checks for WHERE 
that WHERE will return boolean. 
*)
let interpret_expr_where expr var props =
  match expr with
  | EBinop (op, n, m) ->
    let* n = interpret_rec_expr_where n var props in
    let* m = interpret_rec_expr_where m var props in
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
    | _ -> Result.error IncorrectWhere)
  | _ -> Result.error IncorrectWhere
;;

let rec interpret_expr env = function
  | EConst (CString s) -> Result.ok [ AValue (VString s) ]
  | EConst (CInt n) -> Result.ok [ AValue (VInt n) ]
  | EGetProp (elm, field) -> find_valueofelm_env elm field env
  | EGetElm elm -> find_elm_env elm env
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
              | _ -> Result.error IncorrectType)
            | _ -> Result.error IncorrectType)
          (Result.ok listabstvalue)
          m)
      (Result.ok [])
      n
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
  let node = (gen_sum (), label, vproperties), (None, None) in
  Result.ok
    (Format.fprintf Format.std_formatter "Vertex created\n%!";
     ( Graph.add_vertex graph node
     , (match var with
       | None -> env
       | Some var -> (var, node) :: env)
     , node ))
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

let peek_or_add_node env graph = function
  | NodeData (var, label, properties) ->
    let varopt = var in
    (match var with
    | Some var ->
      (match List.assoc_opt var env with
      | None ->
        (match add_node varopt label graph env properties with
        | Ok (graph, env, node1) -> Result.ok (graph, env, node1)
        | Error err -> Result.error err)
      | Some n1 ->
        (match n1 with
        | n1 -> Result.ok (graph, env, n1)))
    | None ->
      (match add_node None label graph env properties with
      | Ok (graph, env, node1) -> Result.ok (graph, env, node1)
      | Error err -> Result.error err))
;;

let save_edge var label vproperties graph env n1 n2 =
  match n1, n2 with
  | (elm1, (_, _)), (elm2, (_, _)) ->
    let edge = (gen_sum (), [ label ], vproperties), (Some elm1, Some elm2) in
    Result.ok
      (Format.fprintf Format.std_formatter "Edge created\n%!";
       ( Graph.add_edge_e graph (n1, edge, n2)
       , (match var with
         | None -> env
         | Some var -> (var, edge) :: env)
       , edge ))
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

let send_check_datas_with_where dprops env labels props var expr = function
  | Some dlabels ->
    (match dprops with
    | Some dprops ->
      let* dprops = get_props env dprops in
      (match List.length dlabels <= List.length labels with
      | true ->
        (match List.for_all (fun dlabel -> List.mem dlabel labels) dlabels with
        | true ->
          (match List.length dprops <= List.length props with
          | true ->
            (match List.for_all (fun dprop -> List.mem dprop props) dprops with
            | true -> interpret_expr_where expr var props
            | false -> Result.ok false)
          | false -> Result.ok false)
        | false -> Result.ok false)
      | false -> Result.ok false)
    | None ->
      (match List.length dlabels <= List.length labels with
      | true ->
        (match List.for_all (fun dlabel -> List.mem dlabel labels) dlabels with
        | true -> interpret_expr_where expr var props
        | false -> Result.ok false)
      | false -> Result.ok false))
  | None ->
    (match dprops with
    | Some dprops ->
      let* dprops = get_props env dprops in
      (match List.length dprops <= List.length props with
      | true ->
        (match List.for_all (fun dprop -> List.mem dprop props) dprops with
        | true -> interpret_expr_where expr var props
        | false -> Result.ok false)
      | false -> Result.ok false)
    | None -> interpret_expr_where expr var props)
;;

let send_check_datas dprops props labels var elm env fdatas dlabels typeofelm = function
  | None -> send_check_datas_without_where dprops env labels props var elm fdatas dlabels
  | Some (CMatchWhere expr) ->
    let booln = send_check_datas_with_where dprops env labels props var expr dlabels in
    (match booln with
    | Ok true -> Result.ok ((var, elm) :: env, (var, elm) :: fdatas)
    | Ok false -> Result.ok (env, fdatas)
    | Error UnstblElm ->
      (match typeofelm with
      | EEdge -> Result.ok ((var, elm) :: env, (var, elm) :: fdatas)
      | ENode -> Result.ok (env, fdatas))
    | Error err -> Result.error err)
;;

(** 
The function finds all edges between two nodes, 
iterates one by one and sends it for comparison 
with user-specified data. 
*)
let iter_fedges cmdwithmatch graph env fnode1 fnode2 stbledges elm1 elm2 e =
  let fedges = Graph.find_all_edges graph elm1 elm2 in
  List.fold_left
    (fun acc fedge ->
      match fedge, e with
      | (_, fedata, _), EdgeData (var, dlabels, dprops) ->
        (match fedata with
        | (_, labels, props), (_, _) ->
          let* env, stbledges = acc in
          let stbledges_len = List.length stbledges in
          let* _, stbledges =
            send_check_datas
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

let find_nodes cmdwithmatch var dlabels dprops graph env typeofelm =
  Graph.fold_vertex
    (fun v acc ->
      match v with
      | (_, labels, props), (_, _) ->
        let* env, fnodes = acc in
        send_check_datas
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
            iter_fedges cmdwithmatch graph env fnode1 fnode2 stbledges elm1 elm2 e)
        (Result.Ok (env, stbledges))
        fnodes2)
    (Result.Ok (env, []))
    fnodes1
;;

let interp_crt elms env =
  List.fold_left
    (fun acc elm ->
      let* graph, env = acc in
      match elm with
      | Node nodedata ->
        (match peek_or_add_node env graph nodedata with
        | Ok (graph, env, _) -> Result.ok (graph, env)
        | Error err -> Result.error err)
      | Edge (n1, e, n2) ->
        (match e with
        | EdgeData (var, label, properties) ->
          (match label with
          | Some label ->
            let* graph, env, n1 = peek_or_add_node env graph n1 in
            let* graph, env, n2 = peek_or_add_node env graph n2 in
            (match add_edge var graph env n1 n2 label properties with
            | Ok (graph, env, _) -> Result.ok (graph, env)
            | Error err -> Result.error err)
          | None ->
            Result.error (NoLabelEdge "To create an edge, you must specify a label."))))
    (Result.ok env)
    elms
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

let interp_ret exprs graph env =
  let* listret =
    List.fold_left
      (fun acc expr ->
        let* listret = acc in
        let* abstvaluelist = interpret_expr env expr in
        Result.ok (listret @ abstvaluelist))
      (Result.ok [])
      exprs
  in
  List.fold_left
    (fun acc elmret ->
      let* graph, env = acc in
      match elmret with
      | AValue value ->
        Format.fprintf Format.std_formatter "%a\n%!" pp_value value;
        Result.ok (graph, env)
      | AElm elm ->
        (match elm with
        | (id, labels, props), (src, dst) ->
          (match src, dst with
          | Some _, Some _ ->
            Format.fprintf
              Format.std_formatter
              "Edge: %a\n----------------------------------\n%!"
              pp_elm_data
              elm;
            Result.ok (graph, env)
          | _, _ ->
            Format.fprintf
              Format.std_formatter
              "Node: %a\n----------------------------------\n%!"
              pp_elm_data_src_dst
              (id, labels, props);
            Result.ok (graph, env))))
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
      | CMatchCrt elms -> interp_crt elms (graph, env)
      | CMatchRet exprs -> interp_ret exprs graph env
      | CMatchDelete vars -> interp_cmd_vars vars graph env)
    (Result.ok env)
    commands
;;

let interpret_command env = function
  | CmdCreate elms -> interp_crt elms env
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
