type token =
  | LPAR
  | RPAR
  | EQ
  | COMMA
  | SEQ
  | EOF
  | ADD
  | SUB
  | MUL
  | DIV
  | RAND
  | IF
  | THEN
  | ELSE
  | FLOAT of (float)
  | STRING of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Language.prog
