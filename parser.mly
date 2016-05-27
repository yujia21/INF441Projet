%{
  open Language
%}

%token LPAR RPAR EQ COMMA SEQ EOF
%token ADD SUB MUL DIV RAND
%token IF THEN ELSE
%token WHILE DO DONE
%token <float> FLOAT
%token <string> STRING

%right SEQ
%nonassoc THEN ELSE
%nonassoc DO DONE
%right ADD SUB
%right MUL DIV
%nonassoc UMINUS

%start program
%type <Language.prog> program
%%

program:
    | prog EOF { $1 }

prog:
    | STRING EQ expr { Assign ($1, $3) }
    | prog SEQ prog { Seq ($1, $3) }
    | IF expr THEN prog ELSE prog { If ($2, $4, $6) }
    | WHILE STRING DO prog DONE {While ($2, $4)}
    | LPAR prog RPAR { $2 }

expr:
    | STRING { Var $1 }
    | FLOAT { Float $1 }
    | expr ADD expr { Add ($1, $3) }
    | expr SUB expr { Sub ($1, $3) }
    | expr MUL expr { Mul ($1, $3) }
    | expr DIV expr { Div ($1, $3) }
    | RAND LPAR expr COMMA expr RPAR { Rand ($3, $5) }
    | LPAR expr RPAR { $2 }
    | SUB expr %prec UMINUS { Sub (Float 0., $2) }
