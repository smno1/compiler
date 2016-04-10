type token =
  | BOOL_CONST of (bool)
  | INT_CONST of (int)
  | IDENT of (string)
  | STRING_CONST of (string)
  | BOOL
  | INT
  | WRITE
  | READ
  | ASSIGN
  | LPAREN
  | RPAREN
  | LBRAC
  | RBRAC
  | EQ
  | LT
  | GT
  | GTE
  | LTE
  | NEQ
  | PLUS
  | MINUS
  | MUL
  | DIV
  | SEMICOLON
  | PERIOD
  | COLON
  | COMMA
  | EOF
  | AND
  | OR
  | NOT
  | WHILE
  | DO
  | OD
  | IF
  | ELSE
  | THEN
  | FI
  | END
  | PROC
  | REF
  | VAL
  | TYPEDEF
  | UNKNOWN

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Bean_ast.program
