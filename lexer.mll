{
  open Lexing
  open Parser

  let on_newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      {
        pos with
        pos_lnum = pos.pos_lnum + 1;
        pos_bol = pos.pos_cnum;
      }
}

let space = ' ' | '\t' | '\r'
let newline = '\n'

rule token = parse
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | "done" { DONE }
  | '(' { LPAR }
  | '{' { LPAR }
  | ')' { RPAR }
  | '}' { RPAR }
  | ',' { COMMA }
  | ';' { SEQ }
  | '=' { EQ }
  | '+' { ADD }
  | '-' { SUB }
  | '*' { MUL }
  | '/' { DIV }
  | "rand" { RAND }
  | (['0'-'9']+'.'?['0'-'9']* as x) { FLOAT (float_of_string x) }
  | (['a'-'z''0'-'9']+ as str) { STRING str }
  | space+ { token lexbuf }
  | newline { on_newline lexbuf; token lexbuf }
  | eof { EOF }
