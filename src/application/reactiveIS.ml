open Types
open Lexer
(* open Interpreter *)

(** function that parses the program **)
let process_of_file =
  let filename = Sys.argv.(1) in
  Parser.main Lexer.lex (Lexing.from_channel (open_in filename))

(* let _ = interpreterIS process_of_file *)


(* Creation of the root node of the program tree *)
let program_root_node = { label      = "_";
                          start_cond = True;
                          stop_cond  = EndScenario;
                          start_msg  = None;
                          stop_msg   = None}


(* Return the node structure representing a temporal object *)
let program_box_node b =
  match b with
  | Process p -> {label      = name2str p.parameters.name;
                  start_cond = True;
                  stop_cond  = EndScenario;
                  start_msg  = Some p.start_msg;
                  stop_msg   = Some p.stop_msg}

  | Hierarchical h -> { label      = name2str h.parameters.name;
                        start_cond = True;
                        stop_cond  = EndScenario;
                        start_msg  = None;
                        stop_msg   = None}


(* Return vertices and edges of the program tree *)
let rec vertices_edges p l =
  List.concat (
    List.map (fun b ->
      let vertex = P.V.create (program_box_node b) in
      let edge = P.E.create p vertex.label vertex in
      match b with
      | Process p -> [(vertex, edge)]
      | Hierarchical h -> (vertex, edge)::(vertices_edges vertex h.children)
    ) l
  )

(* Create the program tree of a scenario *)
let create_program_tree s =
  let g = P.create () in
  let root_node = P.V.create program_root_node in
  let lv, le = List.split (vertices_edges root_node s) in

  P.add_vertex g root_node;
  List.iter (P.add_vertex g) lv;
  List.iter (P.add_edge_e g) le;

  dot_output g "program_tree.dot";
  g



let _ =
  let scenario = process_of_file in
  let _ = print_scenario scenario in
  let g = create_program_tree scenario in
  g
