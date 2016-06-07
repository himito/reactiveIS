open Tree
open Types
open Conditions

(* State *)
(* type configuration = {program : (programNode, string) tree; mutable state : treeMutable; mutable current_time : time } *)

(* Note, we need to add the path [""] because the root is important*)

let possibleStop state_tree paths =
  (* print_string "casco aqui\n"; *)
  let path_nodes = List.fold_left
              (fun a b ->
                let node = (getNodeState state_tree (List.rev b)) in
                if (isUndefined node) then
                    b::a
                else
                  a
              )
              [] paths in
  path_nodes

let possibleStart paths_state paths_program =
  (* let getLastList l = *)
  (*   List.hd (List.rev l) *)
  (* in *)
  let checkPath p l =
    (List.hd p) = (List.hd l)
  in
  let checkPathList p ll =
    List.fold_left (fun a b -> (checkPath p b) || a ) false ll
  in
  List.fold_left (fun a b -> if (checkPathList b paths_state) then a else a@[b] ) []  paths_program

let printListPaths l =
  List.iter (fun p -> print_string ((stringPath p)^"\n")) l

let ruleStartIsPossible program_tree paths_program state_tree paths_state input current_time   =
  let rec findTrue set =
    match set with
    | [] -> (false,[])
    | h::t ->
        let path_chose = List.rev h in
        if (conditionSatisfied program_tree state_tree paths_state input current_time (getStartCondition program_tree path_chose)) then
                (true, path_chose)
              else
                findTrue t
  in
  let possible_start = possibleStart paths_state paths_program in
  findTrue possible_start


let ruleStopIsPossible program_tree state_tree paths_state input current_time =
  let rec findTrue set =
     match set with
    | [] -> (false,[])
    | h::t ->
        let path_chose = List.rev h in
        if  (conditionSatisfied program_tree state_tree paths_state input current_time (getStopCondition program_tree path_chose)) then
          (true, path_chose)
        else
          findTrue t
  in
  let possible_stop = possibleStop state_tree paths_state in
  findTrue possible_stop


let internalTransition program_tree paths_program state_tree paths_state input current_time output =
    let (is_possible_start,path_start) = ruleStartIsPossible program_tree paths_program state_tree !paths_state input current_time in
    (* let _ = Printf.printf "Es posible Arrancar: %b -> %s\n" is_possible_start (stringPath path_start) in *)
    if is_possible_start then
      begin
        (* hacer rule start *)
        let label = (List.nth path_start (List.length (path_start)-1)) in
        let path_root =  List.rev (List.tl (List.rev path_start)) in
        let sent_msg = getStartMessage program_tree path_start in
        (* Printf.printf "Arranca : %s\n" (stringPath path_start); *)
        (* Printf.printf "Label : %s\n" label; *)
        startNode state_tree path_root label current_time;
        (* Printf.printf "Murio Antes" ; *)
        paths_state := !paths_state@[List.rev path_start];
        if sent_msg <> "" then output := !output@[sent_msg]  ;
        true
      end
    else
      let (is_possible_stop, path_stop) =  ruleStopIsPossible program_tree state_tree !paths_state input current_time in
      (* let _ = Printf.printf "Es posible Parar : %b\n" is_possible_stop in *)
      if is_possible_stop then
        (* hacer rule stop *)
        begin
          let sent_msg = getStopMessage program_tree path_stop in
          stopNode state_tree path_stop current_time;
          if sent_msg <> "" then output := !output@[sent_msg]  ;
          true
        end
      else
        false


let minisleep (sec: float) =
    ignore (Unix.select [] [] [] sec)

let interpreterIS program =
  let (program_tree, paths_program) = buildTree program in
  let _ = printProgram program_tree "program_tree" in
  let state_tree = newStateTree 0 Undefined in
  let paths_state = ref [[""]] in
  let current_time = ref 0 in
  let _ = printState state_tree "state_0" in
  while isUndefined (state_tree) do
    Printf.printf "Time %i\n" !current_time;
    let input = if !current_time <> 13 then [] else ["/mouse 1"] in
    if List.length input > 0 then List.iter (fun a -> Printf.printf "OSC received from environment: \"%s\"\n" a) input;
    let output =  ref [] in
    let counter = ref 1 in
    (* Start Internal Transitions *)
    while (internalTransition program_tree paths_program state_tree paths_state input !current_time output) do
      let _ = printState state_tree ("time_"^(string_of_int !current_time)^"_state_"^(string_of_int !counter)) in
      counter := !counter + 1
    done;
    (* End Internal Transitions *)
    List.iter (fun a -> print_string ("OSC message sent to environment: \""^a^"\"\n")) !output;
    (* minisleep 1.; *)
    current_time := !current_time + 1;
  done
