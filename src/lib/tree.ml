open Types

(* Definition of a node in the program tree *)
type programNode = {mutable start: conditionSystem; mutable stop : conditionSystem; start_msg: msg option; stop_msg: msg option}

(* Definition of a labelled tree*)
type ('a, 'b) tree = Node of 'a * ('b * ('a,'b) tree) list


(* Function to get all paths *)
let getPaths tree =
  let rec getPaths treeList r past =
    match treeList with
    | [] -> r
    | tree::t -> begin match tree with
                  | (l, Node (p,c)) ->
                      let new_past = l::past in
                      let new_r = [new_past]@r in
                      (getPaths c new_r new_past)@(getPaths t [] past)
                  end
  in
  match tree with
  | Node (p,c) -> [[""]]@(getPaths c [] [])

(* Function to translate a name into string *)
let name2string n root=
  match n with
  | Box b -> b
  | Scenario -> root

(* Function to translate a condition into a string *)
let rec condition2string f =
  let path2string p =
    List.fold_left (fun a b -> if a <> "" then a^"."^(name2string b "ε") else (name2string b "ε")) "" p
  in
  let duration2string d =
    match d with
    | Finite i -> string_of_int i
    | Infinite -> "∞"
  in
  match f with
  | True -> "True"
  | WaitFromStart (p,t,d) -> "WaitFromStart ("^(path2string p)^", "^(string_of_int t)^", "^(duration2string d)^")"
  | WaitFromEnd (p,t,d) -> "WaitFromEnd ("^(path2string p)^", "^(string_of_int t)^", "^(duration2string d)^")"
  | EndScenario -> "EndScenario"
  | WaitEvent m -> "WaitEvent ("^m^")"
  | AndCS (c1,c2) -> (condition2string c1)^" &and; "^(condition2string c2)
  | OrCS (c1,c2) -> (condition2string c1)^" &or; "^(condition2string c2)

(* Function to build a program tree from a scenario*)
let buildTree scenario =
  let updateTree paths tree =
    let getPath name paths =
      let p = List.hd name in
      match p with
      | Scenario -> [Scenario]
      | Box b ->  let pathFound = List.find (fun x -> (List.hd x) = b) paths in
                  (List.map (fun a -> Box a) (List.rev pathFound))
    in
    let rec updatePath paths f =
      match f with
      | WaitFromStart (p,t,d) ->  WaitFromStart (getPath p paths,t,d)
      | WaitFromEnd (p,t,d) -> WaitFromEnd (getPath p paths,t,d)
      | AndCS (c1,c2) -> AndCS ((updatePath paths c1), (updatePath paths c2))
      | OrCS (c1,c2) -> OrCS ((updatePath paths c1), (updatePath paths c2))
      | _ -> f
    in
    let rec updateChildren paths t =
      match t with
      | (l, Node (p,c)) -> p.start <- (updatePath paths p.start); p.stop <- (updatePath paths p.stop); List.iter (updateChildren paths) c
    in
    match tree with
    | Node (p,c) -> List.iter (updateChildren paths) c
  in
  let rec buildCondition condition =
    match condition with
    | Wait (e, t, d) -> begin match e with
                          | Start (n) -> WaitFromStart ([n], t, d)
                          | End (n) -> WaitFromEnd ([n], t, d)
                        end
    | Event m -> WaitEvent (m)
    | And (c1,c2) -> AndCS (buildCondition c1, buildCondition c2)
    | Or (c1,c2) -> OrCS (buildCondition c1, buildCondition c2)
  in
  let rec buildChildren l =
    let buildNode box =
      match box with
      | Process p -> ((name2string p.parameters.name ""), Node ({start = (buildCondition p.parameters.start_cond); stop = (buildCondition p.parameters.stop_cond); start_msg = Some (p.start_msg); stop_msg = Some (p.stop_msg)}, []))
      | Hierarchical h -> ((name2string h.parameters.name ""), Node ({start = (buildCondition h.parameters.start_cond); stop = (buildCondition h.parameters.stop_cond); start_msg = None ; stop_msg = None }, (buildChildren h.children)))
    in
    List.fold_left (fun a b -> (buildNode b)::a) [] l
  in
  let tree = Node ({start = True; stop = EndScenario; start_msg = None; stop_msg = None}, (buildChildren scenario)) in
  let pathsTree = getPaths tree in
  let _ = updateTree pathsTree tree in
  (tree, pathsTree)


let getChildrenPath node =
  match node with
  | Node (p,children) -> List.map (fun c -> match c with | (l,Node (param, chil)) -> l) children

let downBranchProgram tree label =
  if label <> "" then
    match tree with
    | Node (p,c) -> List.assoc label c
  else
    tree

let getNodeProgram tree path =
  List.fold_left (downBranchProgram) tree path

let getStartCondition tree path =
  let node = getNodeProgram tree path in
  match node with
  | Node (p,c) -> p.start

let getStopCondition tree path =
  let node = getNodeProgram tree path in
  match node with
  | Node (p,c) -> p.stop


let getMessage msg =
  match msg with
  | Some m -> m
  | None -> ""

let getStopMessage tree path =
  let node = getNodeProgram tree path in
  match node with
  | Node (p,c) -> getMessage (p.stop_msg)

let getStartMessage tree path =
  let node = getNodeProgram tree path in
  match node with
  | Node (p,c) -> getMessage (p.start_msg)


(* Function to print a generic tree*)
let printTree printNode2file t f=
  let rec printNode parent n =
    match n with
    | (label, Node (param,children)) -> (* normal node *)
        printNode2file f label param;
        Printf.fprintf f "\t%s -> %s [fontname = \"Courier\", arrowhead = \"vee\", label = \" %s\"];\n" parent label label;
        List.iter (printNode label) children
  in
  match t with
    | Node (param,children) -> (* root node*)
        printNode2file f "r" param;
        List.iter (printNode "r") children

(* Function to write a dot file of a tree *)
let print2dot printFunction name =
  let file = open_out (name^".dot") in
  Printf.fprintf file "digraph %s {\n" name;
  Printf.fprintf file "\tgraph [labelloc = \"t\"];\nratio = auto;\n";
  printFunction file;
  Printf.fprintf file "}\n";
  close_out file;
  Sys.command ("dot -q -Tpdf "^name^".dot -o "^name^".pdf")

(* Function to print a program tree*)
let printProgram tree name =
  let printNodeProgram2file f label param =
    let printMessage msg =
      match msg.start_msg with
      | Some m -> "<tr><td colspan = \"2\" bgcolor=\"black\"><font color=\"white\">Messages</font></td></tr><tr><td bgcolor=\"grey\">start: </td><td align=\"left\">"^(getMessage msg.start_msg)^"</td></tr><tr><td bgcolor=\"grey\">stop: </td><td align=\"left\">"^(getMessage msg.stop_msg)^"</td></tr>"
      | None -> ""
    in
    let table_params = "border=\"0\" cellborder=\"0\" cellpadding=\"3\"" in
    Printf.fprintf f "\t%s [style = \"filled\" penwidth = 1 fillcolor = \"white\" fontname = \"Courier\", shape = \"Mrecord\", label = <<table %s><tr><td colspan = \"2\" bgcolor=\"black\"><font color=\"white\">Conditions</font></td></tr><tr><td bgcolor=\"grey\">start: </td><td align=\"left\">%s</td></tr><tr><td bgcolor=\"grey\">stop: </td><td align=\"left\">%s</td></tr>%s</table>>];\n" label table_params (condition2string param.start) (condition2string param.stop) (printMessage param)
  in
  print2dot (printTree printNodeProgram2file tree) name


(*****************************************************************************
 *
 *                                State Tree
 *
******************************************************************************)

let stringPath path =
  List.fold_left (fun a b -> a^(if b = "" then "ε" else b)) "" path

(* Extension type of time *)
type timeState = Defined of time | Undefined

(* Definition of a node in the state tree *)
type stateNode = {startDate: time; mutable stopDate : timeState}

(* Definition of a mutable tree *)
type treeMutable = {params: stateNode; mutable branches : childMutable list}
and childMutable = {label : string; node : treeMutable}

(* Function to traslate a date into string *)
let time2string t =
  match t with
  | Defined d -> string_of_int d
  | Undefined -> "┴"



let isUndefined node =
  match node.params.stopDate with
  | Defined _ -> false
  | Undefined -> true

let getChildrenState node =
  List.map (fun n -> n.label) node.branches

(* Function to print a state tree *)
let printState tree name =
  let printStateFunction tree f =
    let printNodeState2file f label param =
      let table_params = "border=\"0\" cellborder=\"0\" cellspacing=\"3\"" in
      Printf.fprintf f "\t%s [style = \"filled\" penwidth = 1 fillcolor = \"white\" fontname = \"Courier\", shape = \"Mrecord\", label = <<table %s><tr><td colspan = \"2\" bgcolor=\"black\"><font color=\"white\">Dates</font></td></tr><tr><td bgcolor=\"grey\">start: </td><td>%i</td></tr><tr><td bgcolor=\"grey\">stop: </td><td>%s</td></tr></table>>];\n" label table_params param.startDate (time2string param.stopDate)
    in
    let rec printNode parent t =
      let label = t.label in
      printNodeState2file f label t.node.params;
      Printf.fprintf f "\t%s -> %s [fontname = \"Courier\", arrowhead = \"vee\", label = \" %s\"];\n" parent label label;
      List.iter (printNode label) t.node.branches
    in
    printNodeState2file f "r" tree.params;
    List.iter (printNode "r") tree.branches
  in
  print2dot (printStateFunction tree) name

(* Function to down a branch of a tree *)
let downBranch tree label =
  if label <> "" then
    let children = tree.branches in (* List of children *)
    (* print_string "casca aqui en downBranch"; *)
    let branch = List.find (fun c -> c.label = label ) children in
    branch.node
  else
    tree

let getNodeState tree path =
  List.fold_left downBranch tree path

(* Funtion to start a node. It adds a new node to the target(path) node with the label *)
let startNode tree path label time =
  let node = getNodeState tree path in
  let new_node = {params = {startDate = time; stopDate = Undefined}; branches = []} in
  node.branches <- {label = label ; node = new_node}::node.branches

(* Function to stop a node. It updates the end date of the target(path) node *)
let stopNode tree path time =
  let rec changeStopDate t node =
    if node.params.stopDate = Undefined then node.params.stopDate <- Defined (t);
    List.iter (fun n -> changeStopDate t n.node) node.branches
  in
  let node = getNodeState tree path in
  changeStopDate time node

(* Function to initialize a state tree with a root node with start and stop dates *)
let newStateTree start stop=
  let stateTree = {params = {startDate = start; stopDate = stop}; branches = []} in
  stateTree
