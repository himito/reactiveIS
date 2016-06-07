open Types
open Tree

let getValueDuration d delta=
  match d with
  | Finite v -> delta <= v
  | Infinite -> true


let waitFromStart stateTree pathsState path minTime maxTime currentTime =
  let new_path = List.map (fun a -> name2string a "") path in
  if List.mem (List.rev new_path) pathsState then
    let node = getNodeState stateTree new_path in
    let start_date = node.params.startDate in
    (* let _ = print_string ("\n"^(string_of_int start_date)) in *)
    let delta = currentTime - start_date in
    (minTime <= delta) && (getValueDuration maxTime delta)
  else
    (* let _ = Printf.printf "no existe path : %s \n" (stringPath (new_path)) in *)
    false

let waitFromEnd stateTree pathsState path minTime maxTime currentTime =
  let new_path = List.map (fun a -> name2string a "") path in
  if List.mem (List.rev new_path) pathsState then
    let node = getNodeState stateTree new_path in
    let stop_date = node.params.stopDate in
    match stop_date with
    | Defined t -> let delta = currentTime - t in
                   (minTime <= delta) && (getValueDuration maxTime delta)
    | Undefined -> false
  else
    (* let _ = Printf.printf "no existe path : %s \n" (stringPath (new_path)) in *)
    false

let waitEvent inputList msg =
  if (List.length inputList) > 0 then
    List.mem msg inputList
  else
    false

let endScenario program_tree state_tree =
  let children_state = getChildrenState state_tree in
  if List.length (getChildrenPath program_tree) == List.length children_state then
    if  (List.fold_left (fun a b -> let node = getNodeState state_tree [b] in a && not(isUndefined(node))) true  children_state) then
      true
    else
      false
  else
    false


let rec conditionSatisfied programTree stateTree pathsState inputList currentTime conditionFormula =
  (* let _ = print_string (""^(condition2string conditionFormula)^"\n") in *)
  match conditionFormula with
  | True -> true
  | WaitFromStart (p,t,d)  -> waitFromStart stateTree pathsState p t d currentTime
  | WaitFromEnd (p,t,d) -> waitFromEnd stateTree pathsState p t d currentTime
  | WaitEvent e -> waitEvent inputList e
  | AndCS (c1,c2) -> (conditionSatisfied programTree stateTree pathsState inputList currentTime c1) && (conditionSatisfied programTree stateTree pathsState inputList currentTime c2)
  | OrCS (c1,c2) -> (conditionSatisfied programTree stateTree pathsState inputList currentTime c1) || (conditionSatisfied programTree stateTree pathsState inputList currentTime c2)
  | EndScenario -> endScenario programTree stateTree
