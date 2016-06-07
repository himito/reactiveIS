%token <string> NAME
%token <string> STRING
%token <int> INT
%token OP CL BOP BCL SEMICOLON COMMA EQ  EOF
%token PROCESS BOX WAIT INF START END EVENT START_COND STOP_COND START_MSG STOP_MSG AND OR SCENARIO

(* %left AND OR *)

%{
  open Lexer
  open Types
%}

%start <Types.scenario> main
%%

main:
  | b = box_expr EOF      { [b] }
  | b = box_expr m = main { b :: m }
;

children_expr:
  | {[]}
  | b = box_expr l= children_expr {b :: l}
;


name_expr:
  | n = NAME { Box (String.uppercase_ascii  n) }
  | SCENARIO { Scenario }
;

box_expr:
  | PROCESS n = NAME EQ BOP par = parameters_expr proc = process_expr BCL SEMICOLON { let (start_c, stop_c) = par in
                                                                                      let (start_m, stop_m) = proc in
                                                                                      Process ({parameters = {name = Box (String.uppercase_ascii  n); start_cond = start_c; stop_cond = stop_c}; start_msg = start_m ; stop_msg = stop_m })
                                                                                    }
  | BOX n = NAME EQ BOP par = parameters_expr c = children_expr BCL SEMICOLON { let (start_c, stop_c) = par in
                                                                                Hierarchical ({parameters = {name = Box (String.uppercase_ascii  n); start_cond = start_c; stop_cond = stop_c}; children = c})
                                                                              }
;

event_expr:
  | START OP n = name_expr CL { Start (n) }
  | END OP n = name_expr CL { End (n) }
;

duration_expr:
  | d = INT { Finite (d) }
  | INF     { Infinite }

condition_expr:
  | EVENT OP s = STRING  CL { Event (s) }
  | WAIT OP e = event_expr COMMA t = INT COMMA d = duration_expr CL { Wait (e, t, d)}
  | OP c1 = condition_expr AND c2 = condition_expr CL  { And (c1,c2) }
  | OP c1 = condition_expr OR c2 = condition_expr CL  { Or (c1,c2) }

;

parameters_expr:
  START_COND EQ c1 = condition_expr SEMICOLON STOP_COND EQ c2 = condition_expr SEMICOLON { (c1,c2) }
;

process_expr:
  START_MSG EQ m1 = STRING SEMICOLON STOP_MSG EQ m2 = STRING SEMICOLON { (m1,m2) }
;
