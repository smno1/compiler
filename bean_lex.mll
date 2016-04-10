{
open Bean_parse
open Lexing

(* line number counter for error handling *)
let num_lines = ref 1 
}

let digit = ['0' - '9']
(* alphabet with underscore *)
let alpha = ['a' - 'z' 'A' - 'Z' '_'] 
(* alphabet with apostrophe *)
let alapos = alpha | ['''] 
let digits = digit+
(* the first char must be a letter or _ *)
let ident = alpha alapos*  
let stringregex = '"'[^ '"' '\n' '\t']*'"'

(* As soon as one of the patterns matches the input string, the OCaml code 
 * within the corresponding { and } is added to the list of tokens, and the 
 * token function is recursively invoked on what remains of the input string. *)
rule token = parse
  (* skip blanks: lexbuf is the implicit name of the input string, so token 
   * lexbuf is a recursive call to the lexer that does not add any tokens to 
   * the list whenever any whitespace is matched in the input string *)
  | [' ' '\t']        { token lexbuf }     
  | '\n'              { incr num_lines; Lexing.new_line lexbuf; token lexbuf }
  | '-'?digits as lxm { INT_CONST(int_of_string lxm) }
  | stringregex as lxm { STRING_CONST lxm }
  (* When the comment symbol is met, we ignore the tokens by using the 
   * comments rule, until a new line is met. *)
  | '#'               { comments lexbuf }
  (* keywords *)
  | "and"             { AND }
  | "bool"            { BOOL }
  | "do"              { DO }
  | "else"            { ELSE }
  | "end"             { END }
  | "fi"              { FI }
  | "if"              { IF }
  | "int"             { INT }
  | "not"             { NOT }
  | "od"              { OD }
  | "or"              { OR }
  | "proc"            { PROC }
  | "ref"             { REF }
  | "then"            { THEN }
  | "true"            { BOOL_CONST true }
  | "typedef"         { TYPEDEF }
  | "val"             { VAL }
  | "while"           { WHILE }
  | "false"           { BOOL_CONST false }
  | "read"            { READ }
  | "write"           { WRITE }
  | ":="              { ASSIGN }
  | '.'               { PERIOD }
  | ','               { COMMA }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | '{'               { LBRAC }
  | '}'               { RBRAC }
  | '='               { EQ }
  | "!="              { NEQ }
  | "<="              { LTE }
  | ">="              { GTE }
  | '<'               { LT }
  | '>'               { GT }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '*'               { MUL }
  | '/'               { DIV }
  | ';'               { SEMICOLON }
  | ':'               { COLON }
  | ident as lxm      { IDENT lxm }
  | _                 { UNKNOWN }
  | eof               { EOF }

(* The comments rule deals with commentation in the bean program. Once a '#' 
 * symbol is met, this rule takes over, adding 1 to the line number count. 
 * Anything till the end of the line will not be parsed *)
and comments = parse
  | '\n'              { incr num_lines; Lexing.new_line lexbuf; token lexbuf }
  | _                 { comments lexbuf }
  | eof               { EOF }
