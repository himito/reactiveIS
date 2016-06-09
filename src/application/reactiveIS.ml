open Types
open Lexer
(* open Interpreter *)

(** function that parses the program **)
let process_of_file =
  let filename = Sys.argv.(1) in
  Parser.main Lexer.lex (Lexing.from_channel (open_in filename))

(* let _ = interpreterIS process_of_file *)

(* Creation of the root node of the program tree *)
let program_root_node = { label_p      = "_";
                          start_cond = True;
                          stop_cond  = EndScenario;
                          start_msg  = None;
                          stop_msg   = None}


(* Return the node structure representing a temporal object *)
let program_box_node b l =
  match b with
  | Process p -> {label_p    = name2str p.parameters.name;
                  start_cond = parse_condition p.parameters.start_cond;
                  stop_cond  = parse_condition p.parameters.stop_cond;
                  start_msg  = Some p.start_msg;
                  stop_msg   = Some p.stop_msg}

  | Hierarchical h -> { label_p    = name2str h.parameters.name;
                        start_cond = parse_condition h.parameters.start_cond;
                        stop_cond  = parse_condition h.parameters.stop_cond;
                        start_msg  = None;
                        stop_msg   = None}


(* Return vertices and edges of the program tree *)
let rec vertices_edges p l =
  List.concat (
    List.map (fun b ->
      let vertex = P.V.create (program_box_node b p.label_p) in
      let edge = P.E.create p vertex.label_p vertex in
      let path = p.label_p ^ "." ^ vertex.label_p in
      match b with
      | Process _ -> [((vertex, path), edge)]
      | Hierarchical h -> ((vertex, path), edge)::(vertices_edges vertex h.children)
    ) l
  )

(* Create the program tree of a scenario *)
let create_program_tree s =
  let g = P.create () in
  let root_node = P.V.create program_root_node in
  let lv, le = List.split (vertices_edges root_node s) in
  let nodes = Hashtbl.create (List.length lv) in

  List.iter
    (fun (v, p) -> P.add_vertex g v; Hashtbl.add nodes p v)
    ((root_node, root_node.label_p)::lv);
  List.iter (P.add_edge_e g) le;

  (nodes, g)


(* Creation of the root node of the program tree *)
let state_root_node = { label_s    = "_";
                        start_time = 0;
                        stop_time  = None}

(* Create the initial state tree *)
let create_state_tree size_p =
  let g = S.create () in
  let root_node = S.V.create state_root_node in
  let nodes = Hashtbl.create size_p in
  S.add_vertex g root_node;
  Hashtbl.add nodes root_node.label_s root_node;
  (nodes, g)

(* Translate a list-like path into a string-like path *)
let get_hash_path p =
  String.concat "." p

(* Stopping a TO *)
let stop s ns p t =
  let path = get_hash_path p in
  let vertex = Hashtbl.find ns path in
  if vertex.stop_time == None then
    S.iter_succ
      (fun v -> if v.stop_time == None then v.stop_time <- Some t)
      s vertex;
    vertex.stop_time <- Some t


(* Starting a TO *)
let start s ns p t =
  let rev_p = List.rev p in
  let new_name = List.hd rev_p in        (* name of the new box - funciton up *)
  let path = get_hash_path (List.rev (List.tl rev_p)) in (* path of the parent - function last*)
  let new_node = S.V.create {label_s = new_name;
                              start_time = t;
                              stop_time = None} in
  let new_edge = S.E.create (Hashtbl.find ns path) new_name new_node in
  S.add_vertex s new_node;
  S.add_edge_e s new_edge;
  Hashtbl.add ns (get_hash_path p) new_node





let str_state_node n =
  String.concat "\n" ["label: "^(n.label_s);
                      "start time: "^(string_of_int n.start_time);
                      "stop time: "^(time2str n.stop_time)]


let _ =
  let scenario = process_of_file in
  let _ = print_scenario scenario in
  let (nodes_g, g) = create_program_tree scenario in
  let (nodes_s, s) = create_state_tree (Hashtbl.length nodes_g) in


  start s nodes_s ["_"; "C"] 2;
  stop s nodes_s ["_"; "C"] 20;

  print_endline ("Find_Vertex:\n"^(str_state_node (Hashtbl.find nodes_s "_.C")));
  dot_output DotProgram.output_graph g "program_tree.dot";
  dot_output DotState.output_graph s "state_tree.dot"
