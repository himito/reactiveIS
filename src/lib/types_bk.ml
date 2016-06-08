(* The name of a box is a string *)
type name = Box of string | Scenario

(* Time is discrete *)
type time = int

(* The duration of a box can be finite or infinite *)
type duration = Finite of time | Infinite

(* Processes send messages to external applications *)
type msg = string

(* Boxes have two events, the start and the end *)
type boxEvent = Start of name | End of name

type condition =
  | Wait of boxEvent * time * duration (* An event happened at a specific time interval *)
  | Event of msg                       (* An event was triggered *)
  | And of condition * condition       (* Conjunction of conditions *)
  | Or of condition * condition        (* Disjunction of conditions *)

(* All boxes have a name, a start condition and a stop condition *)
type params = {name: name; start_cond: condition; stop_cond: condition}

(* A box can have a process or contains more boxes (hierarchy) *)
type box =
  | Process of proc
  | Hierarchical of hierarchical
and proc = {parameters: params; start_msg: msg; stop_msg: msg}
and hierarchical = {parameters: params; children: box list }

type scenario = box list (* A scenario is a list of hierarchical or process boxes*)

(* A path is a string*)
type path = name list

(* Definition of a Condition System *)
type conditionSystem =
  | True
  | WaitFromStart of path * time * duration
  | WaitFromEnd of path * time * duration
  | EndScenario
  | WaitEvent of msg
  | AndCS of conditionSystem * conditionSystem
  | OrCS of conditionSystem * conditionSystem
