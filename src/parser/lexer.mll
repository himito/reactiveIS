{
  type token =
    | INT of int
    | NAME of string
    | OP
    | CL
    | BOP
    | BCL
    | SEMICOLON
    | COMMA
    | PROCESS
    | BOX
    | WAIT
    | EQ
    | INF
    | START
    | END
    | EVENT
    | AND
    | OR
    | START_COND
    | STOP_COND
    | START_MSG
    | STOP_MSG
    | EOF
    | STRING of string
    | SCENARIO
}



rule lex = parse
  | [' ' '\t' '\n']+  { lex lexbuf }
  | "/**"        { comment lexbuf }
  | "//"         { comment_line lexbuf}
  | ['0'-'9']['0'-'9']* as i     { INT (int_of_string (i)) }
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | "("           { OP }
  | ")"           { CL }
  | "{"           { BOP }
  | "}"           { BCL }
  | ";"           { SEMICOLON }
  | "="           { EQ }
  | ","           { COMMA }
  | "INF"         { INF }
  | "&"           { AND }
  | "|"           { OR }
  | "Scenario"    { SCENARIO }
  | "Texture"     { PROCESS }
  | "Structure"   { BOX }
  | "Wait"        { WAIT }
  | "Start"       { START }
  | "End"         { END }
  | "Event"       { EVENT }
  | "_start.msg_"  { START_MSG }
  | "_stop.msg_"   { STOP_MSG }
  | "_start.cond_" { START_COND }
  | "_stop.cond_"  { STOP_COND }
  | ['a'-'z' 'A'-'Z']['0'-'9' 'a'-'z' 'A'-'Z']* as s { NAME (s) }
  | eof           { EOF }

and comment = parse
    | "**/"        { lex lexbuf   }
    | _             { comment lexbuf }

and comment_line = parse
    | "\n"          { lex lexbuf   }
    | _             { comment_line lexbuf }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }

