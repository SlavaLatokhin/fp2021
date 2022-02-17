open Ast
open Graph

type value =
  | VString of string
  | VInt of int
[@@deriving show { with_path = false }]

let pp_value fmt = function
  | VString s -> Format.fprintf fmt "(VString %S)" s
  | VInt n -> Format.fprintf fmt "(VInt %d)" n
;;

type vproperty = string * value [@@deriving show { with_path = false }]
type vproperties = vproperty list [@@deriving show { with_path = false }]

type elm_data_src_dst = int * string list * vproperties
[@@deriving show { with_path = false }]

(* 
Uses the same type for vertices and edges to make 
it easier to store data in a single environment.

Node: (id, labels, vproperties), (None, None) 
Edge: (id. labels, vproperties), (src, dst)
*)
type elm_data =
  (int * string list * vproperties) * (elm_data_src_dst option * elm_data_src_dst option)
[@@deriving show { with_path = false }]

let id = ref 0

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
[@@deriving show { with_path = false }]

(* 
This type is needed to define commands that 
look for the necessary elements in the environment. 
Helps for code reuse.
*)
type cmdvars =
  | Delete
  | Return
[@@deriving show { with_path = false }]

let rec interpret_expr = function
  | EConst (CString s) -> Result.ok (VString s)
  | EConst (CInt n) -> Result.ok (VInt n)
  | EBinop (op, n, m) ->
    let* n = interpret_expr n in
    let* m = interpret_expr m in
    (match op, n, m with
    | Plus, VInt n, VInt m -> Result.ok (VInt (n + m))
    | Minus, VInt n, VInt m -> Result.ok (VInt (n - m))
    | Star, VInt n, VInt m -> Result.ok (VInt (n * m))
    | Slash, VInt n, VInt m -> Result.ok (VInt (n / m))
    | _ -> Result.error IncorrectType)
;;

let get_props props =
  List.fold_left
    (fun acc (name, expr) ->
      let* value = interpret_expr expr in
      let* acc = acc in
      Result.ok ((name, value) :: acc))
    (Result.ok [])
    props
;;

let save_node var label vproperties graph env =
  id := !id + 1;
  let node = (!id, label, vproperties), (None, None) in
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
    let* vproperties = get_props props in
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
  id := !id + 1;
  match n1, n2 with
  | (elm1, (_, _)), (elm2, (_, _)) ->
    let edge = (!id, [ label ], vproperties), (Some elm1, Some elm2) in
    Result.ok
      (Format.fprintf Format.std_formatter "Edge created\n%!";
       ( Graph.add_edge_e graph (n1, edge, n2)
       , (match var with
         | None -> env
         | Some var -> (var, edge) :: env)
       , edge ))
;;

let add_edge var graph env n1 n2 properties label =
  match properties with
  | Some props ->
    let* vproperties = get_props props in
    save_edge var label vproperties graph env n1 n2
  | None -> save_edge var label [] graph env n1 n2
;;

let check_data ddatas datas var elm env fdatas =
  if List.length ddatas <= List.length datas
  then
    if List.for_all (fun ddata -> List.mem ddata datas) ddatas
    then Result.ok ((var, elm) :: env, (var, elm) :: fdatas)
    else Result.ok (env, fdatas)
  else Result.ok (env, fdatas)
;;

let send_check_datas dprops props labels var elm env fdatas = function
  | Some dlabels ->
    (match dprops with
    | Some dprops ->
      let* dprops = get_props dprops in
      if List.length dlabels <= List.length labels
      then
        if List.for_all (fun dlabel -> List.mem dlabel labels) dlabels
        then check_data dprops props var elm env fdatas
        else Result.ok (env, fdatas)
      else Result.ok (env, fdatas)
    | None -> check_data dlabels labels var elm env fdatas)
  | None ->
    (match dprops with
    | Some dprops ->
      let* dprops = get_props dprops in
      check_data dprops props var elm env fdatas
    | None -> Result.ok ((var, elm) :: env, (var, elm) :: fdatas))
;;

let iter_fedges graph env fnode1 fnode2 stbledges elm1 elm2 e =
  let fedges = Graph.find_all_edges graph elm1 elm2 in
  List.fold_left
    (fun acc fedge ->
      match fedge, e with
      | (_, fedata, _), EdgeData (var, dlabels, dprops) ->
        (match fedata with
        | (_, labels, props), (_, _) ->
          let* env, stbledges = acc in
          let stbledges_len = ref (List.length stbledges) in
          let* _, stbledges =
            send_check_datas
              dprops
              props
              labels
              (Option.value var ~default:"")
              fedata
              env
              stbledges
              (Some (Option.to_list dlabels))
          in
          if List.length stbledges > !stbledges_len
          then
            Result.ok
              ( fnode1 :: (Option.value var ~default:"", fedata) :: fnode2 :: env
              , stbledges )
          else Result.ok (env, stbledges)))
    (Result.Ok (env, stbledges))
    fedges
;;

let find_nodes var dlabels dprops graph env =
  Graph.fold_vertex
    (fun v acc ->
      match v with
      | (_, labels, props), (_, _) ->
        let* env, fnodes = acc in
        send_check_datas dprops props labels var v env fnodes dlabels)
    graph
    (Result.ok (env, []))
;;

let find_edges n1 e n2 graph env =
  let* _, fnodes1 =
    match n1 with
    | NodeData (var, label, properties) ->
      find_nodes (Option.value var ~default:"") label properties graph []
  in
  let* _, fnodes2 =
    match n2 with
    | NodeData (var, label, properties) ->
      find_nodes (Option.value var ~default:"") label properties graph []
  in
  List.fold_left
    (fun acc fnode1 ->
      let* env, stbledges = acc in
      List.fold_left
        (fun acc fnode2 ->
          let* env, stbledges = acc in
          match fnode1, fnode2 with
          | (_, elm1), (_, elm2) ->
            iter_fedges graph env fnode1 fnode2 stbledges elm1 elm2 e)
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
            (match add_edge var graph env n1 n2 properties label with
            | Ok (graph, env, _) -> Result.ok (graph, env)
            | Error err -> Result.error err)
          | None ->
            Result.error (NoLabelEdge "To create an edge, you must specify a label."))))
    (Result.ok env)
    elms
;;

let exe_cmd_vars eelm id labels props src dst graph env cmd =
  match src, dst with
  | Some src, Some dst ->
    (match cmd with
    | Delete ->
      Result.ok (Graph.remove_edge graph (src, (None, None)) (dst, (None, None)), env)
    | Return ->
      Format.fprintf
        Format.std_formatter
        "Edge: %a\n----------------------------------\n%!"
        pp_elm_data
        eelm;
      Result.ok (graph, env))
  | _, _ ->
    (match cmd with
    | Delete -> Result.ok (Graph.remove_vertex graph eelm, env)
    | Return ->
      Format.fprintf
        Format.std_formatter
        "Node: %a\n----------------------------------\n%!"
        pp_elm_data_src_dst
        (id, labels, props);
      Result.ok (graph, env))
;;

let interp_cmd_vars vars graph env cmd =
  if List.length vars <= List.length env
  then
    if List.for_all (fun var -> List.mem_assoc var env) vars
    then (
      let processed = Array.make (List.length env) 0 in
      let i = ref 0 in
      List.fold_left
        (fun acc var ->
          let* graph, env = acc in
          List.fold_left
            (fun acc oneenv ->
              let* graph, env = acc in
              match oneenv with
              | evar, eelm ->
                (match eelm with
                | (id, labels, props), (src, dst) ->
                  if var = evar && not (Array.exists (fun aid -> id = aid) processed)
                  then (
                    processed.(!i) <- id;
                    i := !i + 1;
                    exe_cmd_vars eelm id labels props src dst graph env cmd)
                  else Result.ok (graph, env)))
            (Result.ok (graph, env))
            env)
        (Result.ok (graph, env))
        vars)
    else Result.error (NotBound "Undefined variable or nothing was found.")
  else Result.error (NotBound "Undefined variable or nothing was found.")
;;

let interp_match elms env commands =
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
              let* env, _ = find_nodes var label properties graph env in
              Result.ok (graph, env)
            | None -> Result.ok (graph, env)))
        | Edge (n1, e, n2) ->
          let* env, _ = find_edges n1 e n2 graph env in
          Result.ok (graph, env))
      (Result.ok env)
      elms
  in
  List.fold_left
    (fun acc cmd ->
      let* graph, env = acc in
      match cmd with
      | CMatchCrt elms -> interp_crt elms (graph, env)
      | CMatchRet vars -> interp_cmd_vars vars graph env Return
      | CMatchDelete vars -> interp_cmd_vars vars graph env Delete)
    (Result.ok env)
    commands
;;

let interpret_command env = function
  | CmdCreate elms -> interp_crt elms env
  | CmdMatch (elms, commands) -> interp_match elms env commands
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
