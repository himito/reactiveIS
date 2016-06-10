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
  String.concat "." (Array.to_list p)

(* Exception indicating that the path does not exist *)
exception PathNoExists

(* Stopping a TO *)
let stop s ns p t =
  let rec apply_children v =
    if v.stop_time == None then
      S.iter_succ (fun c -> apply_children c) s v;
      v.stop_time <- Some t
  in
  let path = get_hash_path p in
  if Hashtbl.mem ns path then
    let vertex = Hashtbl.find ns path in
      apply_children vertex
  else raise PathNoExists


let up p = Array.sub p 0 ((Array.length p) - 1)

(* Starting a TO. TODO: Change p \in L(S) by up(p) \in L(S) *)
let start s ns p t =
  let pos_array = (Array.length p) - 1 in
  let new_name = Array.get p pos_array  in        (* name of the new box - funciton up *)
  let path = get_hash_path (Array.sub p 0 pos_array) in (* path of the parent - function last*)
  if Hashtbl.mem ns path then
    let new_node = S.V.create {label_s = new_name;
                                start_time = t;
                                stop_time = None} in
    let new_edge = S.E.create (Hashtbl.find ns path) new_name new_node in
    S.add_vertex s new_node;
    S.add_edge_e s new_edge;
    Hashtbl.add ns (get_hash_path p) new_node
  else raise PathNoExists


(* Return the paths of the nodes that are currently running *)
let alive ns =  Hashtbl.fold
                (fun p v acc ->
                  if v.stop_time == None then
                    (get_array_path p)::acc
                  else acc)
                ns []

(* Return the children of the node reached by a path *)
let children nodes_t path =
  if Hashtbl.mem nodes_t (get_hash_path path) then
    Hashtbl.fold
      (fun p n acc -> let path_child = (get_array_path p) in
                      if (up path_child) = path then path_child :: acc else acc)
      nodes_t []
  else raise PathNoExists


(* Computes the path of the boxes that can start *)
let can_start np ns =
  List.filter
    (fun p -> not (Hashtbl.mem ns (get_hash_path p)))
    (List.concat (List.map (children np) (alive ns)))

(* Computes the path of the boxes that can stop *)
let can_stop = alive


(* Show the information of a state node *)
let str_state_node n =
  String.concat "\n" ["label: "^(n.label_s);
                      "start time: "^(string_of_int n.start_time);
                      "stop time: "^(time2str n.stop_time)]


let _ =
  let scenario = process_of_file in
  let (nodes_g, g) = create_program_tree scenario in
  let (nodes_s, s) = create_state_tree (Hashtbl.length nodes_g) in

  start s nodes_s [| "_"; "C" |] 2;
  start s nodes_s [| "_"; "C"; "E" |] 10;
  stop s nodes_s [| "_";"C";"E" |] 20;

  (* List.iter (fun v -> print_endline (String.concat "." (Array.to_list v))) (alive nodes_s); *)

  print_endline "Can Start:";
  List.iter (fun v -> print_endline (String.concat "." (Array.to_list v))) (can_start nodes_g nodes_s);

  print_endline "Can Stop:";
  List.iter (fun v -> print_endline (String.concat "." (Array.to_list v))) (can_stop nodes_s);


(*   print_endline ("Find_Vertex:\n"^(str_state_node (Hashtbl.find nodes_s "_.C.E"))); *)
  dot_output DotProgram.output_graph g "program_tree.dot";
  dot_output DotState.output_graph s "state_tree.dot"
