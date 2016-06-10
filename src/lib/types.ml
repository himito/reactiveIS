open Graph

(* Names of temporal objects *)
type label = string

(* Temporal objects send messages to external applications *)
type msg = string

(* Time is discrete *)
type time = int

(* The duration of a box can be finite or infinite *)
type duration = Finite of time | Infinite

(* A path is an array of string *)
type path = label array

(* Translate a string-like path into a list-like path *)
let get_array_path p = Array.of_list (Str.split (Str.regexp "\\.") p)

(* Definition of a Condition System *)
type condition_system =
  | True
  | WaitFromStart of path * time * duration
  | WaitFromEnd of path * time * duration
  | EndScenario
  | WaitEvent of msg
  | And of condition_system * condition_system
  | Or of condition_system * condition_system

(* information of a node program *)
type program_node_info = {label_p: label ;
                          start_cond: condition_system ;
                          stop_cond: condition_system;
                          start_msg: msg option;
                          stop_msg: msg option;
                          }

(* representation of a node in the program tree -- must be hashable *)
module ProgramVertex = struct
  type t = program_node_info
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
end

(* representation of an edge in the program tree -- must be comparable *)
module ProgramEdge = struct
  type t = label
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = ""
end

(* A program tree is a mutable directed graphs *)
module P = Imperative.Digraph.ConcreteLabeled(ProgramVertex)(ProgramEdge)

(* module for creating dot-files *)
module DotProgram = Graphviz.Dot (struct
    include P (* use the graph module from above *)

    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let default_edge_attributes _ = []
    let get_subgraph _ = None

    let vertex_name v = v.label_p

    let vertex_attributes _ = [`Shape `Box]

    let edge_attributes (a, e, b) = [`Label e; `Color 4711]

end)

(* print the graph *)
let dot_output output g f=
  let file = open_out f in
  output file g;
  close_out file


(* information of a state node *)
type state_node_info = {label_s: label ;
                        mutable start_time: time ;
                        mutable stop_time: time option;
                        }


let time2str v =
  match v with
  | Some t -> string_of_int t
  | None -> "undefined"

(* representation of a node in the state tree -- must be hashable *)
module StateVertex = struct
  type t = state_node_info
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
end

(* representation of an edge in the state tree -- must be comparable *)
module StateEdge = struct
  type t = label
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = ""
end

(* A state tree is a mutable directed graphs *)
module S = Imperative.Digraph.ConcreteLabeled(StateVertex)(StateEdge)

(* module for creating dot-files *)
module DotState = Graphviz.Dot (struct
    include S (* use the graph module from above *)

    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let default_edge_attributes _ = []
    let get_subgraph _ = None

    let vertex_name v = v.label_s

    let vertex_attributes _ = [`Shape `Box]

    let edge_attributes (a, e, b) = [`Label e; `Color 4711]

end)


(* The name of a box is a string *)
type name = Box of string | Scenario

(* Return the name of the box as string *)
let name2str n =
  match n with
  | Box s -> s
  | Scenario -> ""

(* Boxes have two events, the start and the end *)
type boxEvent = Start of name | End of name

type condition =
  | Wait of boxEvent * time * duration (* An event happened at a specific time interval *)
  | Event of msg                       (* An event was triggered *)
  | And of condition * condition       (* Conjunction of conditions *)
  | Or of condition * condition        (* Disjunction of conditions *)


let rec parse_condition c =
  match c with
  | Wait (b, t, d) ->
    begin
      match b with
      | Start n -> WaitFromStart (get_array_path (name2str n), t, d)
      | End n -> WaitFromEnd (get_array_path (name2str n), t, d)
    end
  | Event m -> WaitEvent m
  | And (c1, c2) -> And (parse_condition c1, parse_condition c2)
  | Or (c1, c2) -> Or (parse_condition c1, parse_condition c2)


(* All boxes have a name, a start condition and a stop condition *)
type params = {name: name; start_cond: condition; stop_cond: condition}

(* A box can have a process or contains more boxes (hierarchy) *)
type box =
  | Process of proc
  | Hierarchical of hierarchical
and proc = {parameters: params; start_msg: msg; stop_msg: msg}
and hierarchical = {parameters: params; children: box list }

type scenario = box list (* A scenario is a list of hierarchical or process boxes*)

(* Function that prints the scenario *)
let print_scenario s =
  let rec print_box b l =
    match b with
    | Process p -> name2str p.parameters.name
    | Hierarchical h -> name2str h.parameters.name ^ (
        List.fold_left
          (fun acc b -> ("\n"^(String.make l ' ')^"- "^print_box b (l+1))^acc)
          "" h.children
        )
  in
  print_endline (String.concat "\n" (
    List.fold_left (fun acc b -> ("- "^print_box b 1)::acc) [] s
  ))

(* Return the duration of a box as a string *)
let duration2str d =
  match d with
  | Finite i -> string_of_int i
  | Infinite -> "∞"

(* Return a condition as a string *)
let rec condition2str c =
  match c with
  | True -> "True"
  | EndScenario -> "EndScenario"
  | WaitEvent m -> "WaitEvent ("^m^")"
  | And (c1,c2) -> "("^(condition2str c1)^" & "^(condition2str c2)^")"
  | Or (c1, c2) -> "("^(condition2str c1)^" | "^(condition2str c2)^")"
  | WaitFromStart (p,s,e) -> "WaitFromStart ("^ (String.concat "." (Array.to_list p)) ^", "^(string_of_int s)^", "^(duration2str e)
  | WaitFromEnd (p,s,e) -> "WaitFromEnd ("^ (String.concat "." (Array.to_list p)) ^", "^(string_of_int s)^", "^(duration2str e)
