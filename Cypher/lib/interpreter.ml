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
type elm_data = int * string list * vproperties [@@deriving show { with_path = false }]
type elm_data_list = elm_data list [@@deriving show { with_path = false }]

type string_elm_data_list = (string * elm_data) list
[@@deriving show { with_path = false }]

let id = ref 0

module ElmData = struct
  type t = elm_data

  let compare = Stdlib.compare
  let hash = Hashtbl.hash
  let equal = ( = )
  let default : t = 0, [], []
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
  | NotBound
  | NotFound
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

let save_node var label vproperties graph env =
  id := !id + 1;
  let node = !id, label, vproperties in
  Result.ok
    (Format.fprintf
       Format.std_formatter
       "Saving a vertex %a to a graph\n%!"
       pp_elm_data
       node;
     ( Graph.add_vertex graph node
     , (match var with
       | None -> env
       | Some var -> (var, node) :: env)
     , node ))
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
  | Nodedata (var, label, properties) ->
    let varopt = var in
    (match var with
    | Some var ->
      (match List.assoc_opt var env with
      | None ->
        (match add_node varopt label graph env properties with
        | Ok (graph, env, node1) -> Result.ok (graph, env, node1)
        | Error err -> Result.error err)
      | Some n1 -> Result.ok (graph, env, n1))
    | None ->
      (match add_node None label graph env properties with
      | Ok (graph, env, node1) -> Result.ok (graph, env, node1)
      | Error err -> Result.error err))
;;

let save_edge var label vproperties graph env n1 n2 =
  id := !id + 1;
  let edge = !id, [ label ], vproperties in
  Result.ok
    (Format.fprintf
       Format.std_formatter
       "Saving a edge %a to a graph\n%!"
       pp_elm_data
       edge;
     ( Graph.add_edge_e graph (n1, edge, n2)
     , (match var with
       | None -> env
       | Some var -> (var, edge) :: env)
     , edge ))
;;

let add_edge var label graph env n1 n2 = function
  | Some props ->
    let* vproperties = get_props props in
    save_edge var label vproperties graph env n1 n2
  | None -> save_edge var label [] graph env n1 n2
;;

let interp_crt elms env =
  List.fold_left
    (fun acc elm ->
      let* graph, env = acc in
      match elm with
      | Node nodedata ->
        (match nodedata with
        | Nodedata (var, label, properties) ->
          (match add_node var label graph env properties with
          | Ok (graph, env, _) -> Result.ok (graph, env)
          | Error err -> Result.error err))
      | Edge (n1, e, n2) ->
        let* graph, env, n1 = peek_or_add_node env graph n1 in
        let* graph, env, n2 = peek_or_add_node env graph n2 in
        (match e with
        | Edgedata (var, label, properties) ->
          (match add_edge var label graph env n1 n2 properties with
          | Ok (graph, env, _) -> Result.ok (graph, env)
          | Error err -> Result.error err)))
    (Result.ok env)
    elms
;;

let check_data ddata data var v env =
  if List.length ddata <= List.length data
  then
    if List.for_all (fun dlabel -> List.mem dlabel data) ddata
    then Result.ok ((var, v) :: env)
    else Result.ok env
  else Result.ok env
;;

let find_nodes var dlabels dprops graph env =
  Graph.fold_vertex
    (fun v acc ->
      match v with
      | id, labels, props ->
        let* env = acc in
        (match dlabels with
        | Some dlabels ->
          (match dprops with
          | Some dprops ->
            let* dprops = get_props dprops in
            if List.length dlabels <= List.length labels
            then
              if List.for_all (fun dlabel -> List.mem dlabel labels) dlabels
              then check_data dprops props var v env
              else Result.ok env
            else Result.ok env
          | None -> check_data dlabels labels var v env)
        | None ->
          (match dprops with
          | Some dprops ->
            let* dprops = get_props dprops in
            check_data dprops props var v env
          | None -> Result.ok ((var, v) :: env))))
    graph
    (Result.ok env)
;;

let interp_match elms env commands =
  List.fold_left
    (fun acc elm ->
      let* graph, env = acc in
      match elm with
      | Node nodedata ->
        (match nodedata with
        | Nodedata (var, label, properties) ->
          (match var with
          | Some var ->
            let* env = find_nodes var label properties graph env in
            Format.fprintf Format.std_formatter "Find %a\n%!" pp_string_elm_data_list env;
            Result.ok (graph, env)
          | None -> Result.ok (graph, env)))
      | Edge (n1, e, n2) -> Result.ok (graph, env))
    (Result.ok env)
    elms
;;

let interpret_command env fmt = function
  | CmdCreate elms -> interp_crt elms env
  | CmdMatch (elms, commands) -> interp_match elms env commands
;;

let interpret_program commands =
  let graph = Graph.empty in
  List.fold_left
    (fun acc command ->
      let* graph, _ = acc in
      match interpret_command (graph, []) Format.std_formatter command with
      | Error err -> Result.error err
      | Ok (graph, _) -> Result.ok (graph, []))
    (Result.ok (graph, []))
    commands
;;

let%expect_test _ =
  let _ =
    let parsed =
      Parser.parse_with
        Parser.pcmdssep
        {|
        CREATE (pam :Person :Student {name: "Pam"}),
                (tom :Person {name: "Tom"}),
                (kate :Person {name: "Kate"}),
                (pam)-[:PARENT {role: "Father"}]->(tom),
                (kate)-[:PARENT]->(jessica:Person{name:"Jessica"});
        CREATE (bob:Person {name: "Bob"})-[:PARENT {role: "Father"}]->(ann), (a)-[:hello]->(b);
        MATCH (n: Person);
        |}
    in
    let open Caml.Format in
    match parsed with
    | Error err -> printf "%s%!" err
    | Ok commands ->
      (match interpret_program commands with
      | Error err -> printf "%a%!" pp_err err
      | Ok (graph, _) -> printf "%a%!" pp_graph graph)
  in
  [%expect
    {|
    Saving a vertex (1, ["Person"; "Student"], [("name", (VString "Pam"))]) to a graph
    Saving a vertex (2, ["Person"], [("name", (VString "Tom"))]) to a graph
    Saving a vertex (3, ["Person"], [("name", (VString "Kate"))]) to a graph
    Saving a edge (4, ["PARENT"], [("role", (VString "Father"))]) to a graph
    Saving a vertex (5, ["Person"], [("name", (VString "Jessica"))]) to a graph
    Saving a edge (6, ["PARENT"], []) to a graph
    Saving a vertex (7, ["Person"], [("name", (VString "Bob"))]) to a graph
    Saving a vertex (8, [], []) to a graph
    Saving a edge (9, ["PARENT"], [("role", (VString "Father"))]) to a graph
    Saving a vertex (10, [], []) to a graph
    Saving a vertex (11, [], []) to a graph
    Saving a edge (12, ["hello"], []) to a graph
    Find [("n", (7, ["Person"], [("name", (VString "Bob"))]));
           ("n", (5, ["Person"], [("name", (VString "Jessica"))]));
           ("n", (3, ["Person"], [("name", (VString "Kate"))]));
           ("n", (2, ["Person"], [("name", (VString "Tom"))]));
           ("n", (1, ["Person"; "Student"], [("name", (VString "Pam"))]))]
    Vertecies:
    (1, ["Person"; "Student"], [("name", (VString "Pam"))])
    (2,
                                                                        ["Person"
                                                                        ],
                                                                        [("name",
                                                                        (VString "Tom"))
                                                                        ])
    (
    3, ["Person"], [("name", (VString "Kate"))])
    (5, ["Person"],
                                                  [("name", (VString "Jessica"))])
    (
    7, ["Person"], [("name", (VString "Bob"))])
    (8, [], [])
    (10, [], [])
    (
    11, [], [])
    --------------------------
    Edges:
    (1, ["Person"; "Student"],
                                                   [("name", (VString "Pam"))])-[(
    4, ["PARENT"], [("role", (VString "Father"))])]->(2, ["Person"],
                                                      [("name", (VString "Tom"))])
    (3, ["Person"], [("name", (VString "Kate"))])-[(6, ["PARENT"], [])]->(
    5, ["Person"], [("name", (VString "Jessica"))])
    (7, ["Person"], [("name", (VString "Bob"))])-[(9, ["PARENT"],
                                                   [("role", (VString "Father"))])]->(
    8, [], [])
    (10, [], [])-[(12, ["hello"], [])]->(11, [], [])
 |}]
;;
