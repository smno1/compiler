%{
open Bean_ast

(* The customized error report function, which gives the location and
 * suitable error messages. This implementation will work on OCaml 3.11.2.
 * To compile on OCaml 4, just remove the 'Lexing' before 'pos_lnum' *)
let parse_error s =
  begin
    try
      let start_pos = Parsing.symbol_start_pos ()
      and end_pos = Parsing.symbol_end_pos () in
      Printf.printf "at line %d, characters %d-%d: \n"
        start_pos.Lexing.pos_lnum
        (start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol)
        (end_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol)
    with Invalid_argument(_) -> ()
  end;
  Printf.printf "Syntax error: %s\n" s;
  raise Parsing.Parse_error
%}

%token <bool> BOOL_CONST
%token <int> INT_CONST
%token <string> IDENT
%token <string> STRING_CONST
%token BOOL INT
%token WRITE READ
%token ASSIGN
%token LPAREN RPAREN LBRAC RBRAC
%token EQ LT GT GTE LTE NEQ
%token PLUS MINUS MUL DIV
%token SEMICOLON PERIOD COLON COMMA
%token EOF
%token AND OR NOT
%token WHILE DO OD
%token IF ELSE THEN FI END
%token PROC
%token REF VAL
%token TYPEDEF
%token UNKNOWN

%nonassoc EQ LT GT GTE LTE NEQ
%left PLUS MINUS
%left MUL DIV
%nonassoc UMINUS

%type <Bean_ast.program> program

%start program
%%

/* -- Top Level: program -- */
program :
  typedefs procs                  { { typedefs = List.rev $1; procs = List.rev $2 } }

/* -- Type definition -- */
typedefs :
  | typedefs typedef              { $2 :: $1 }
  |                               { [] }

typedef :
  | TYPEDEF typespec IDENT        { ($2, $3) }

/* -- Type specification -- */
typespec :
  | BOOL                          { Bool }
  | INT                           { Int }
  | LBRAC fielddefs RBRAC         { Flist (List.rev $2) } 
  | IDENT                         { Id $1 }
  /* error patterns */
  /*| LBRAC fielddefs               { parse_error "expected '}'."}
  | UNKNOWN                       { parse_error "not a valid identifier." }*/

/* -- Field definition: { f : int, g : bool } -- */
fielddef :
  | IDENT COLON typespec          { ($1, $3) }
  /* error patterns */
  | UNKNOWN COLON typespec        { parse_error "not a valid identifier." }
  | IDENT COLON error             { parse_error "not a valid typespec."}

fielddefs :
  | fielddefs COMMA fielddef      { $3 :: $1 }
  | fielddef                      { [$1] }
  /* error patterns */
  | fielddefs SEMICOLON fielddef  { parse_error "should be ',' separated." }
  |                               { parse_error "must not be empty."}

/* -- Procedure -- */
procs :
  | procs proc                    { $2 :: $1 }
  | proc                          { [$1] }
  /* error patterns */
  /*|                               { parse_error "must not have empty procedure." }*/

proc :
  | PROC procheader procbody END  { ($2, $3) }
  /* error patterns */
/*  | PROC procheader procbody error
                                  { parse_error "expected 'end'." }*/

/* -- Name and Parameter List of a procedure -- */
procheader :
  | IDENT LPAREN params RPAREN    { { procname = $1; parameters = (List.rev $3) } }
  /* error patterns */
  | IDENT LPAREN params           { parse_error "expected ')'" }
  | IDENT params                  { parse_error "expected '('" }

/* -- Parameter list of the procedure -- */
params :
  | params COMMA param            { $3 :: $1 }
  | param                         { [$1] }
  |                               { [] }
  /* error patterns */
  /*| params SEMICOLON param        { parse_error "parameters should be ',' separated." }*/

param :
  | passspec typespec IDENT       { ($1, $2, $3) }  

/* -- Indicator of variable type: Val / Ref -- */
passspec :
  | VAL                           { Val }
  | REF                           { Ref }

/* -- Procedure body: variable declaration and statements -- */
procbody :
  vardecls stmts                  { { vardecls = List.rev $1; stmts = List.rev $2 } }

vardecls :
  | vardecls vardecl              { $2 :: $1 }
  |                               { [] }

vardecl :
  | typespec IDENT SEMICOLON      { ($1, $2) }

stmts :
  | stmts stmt                    { $2 :: $1 }
  | stmt                          { [$1] }
  /* error patterns */
  |                               { parse_error "expected a statement." }

stmt :
  | lvalue ASSIGN rvalue SEMICOLON 
                                  { Assign ($1, $3) }
  | READ lvalue SEMICOLON         { Read $2 }
  | WRITE expr SEMICOLON          { Write $2 }
  | WRITE STRING_CONST SEMICOLON  { WriteS $2 }
  | IDENT LPAREN exprs RPAREN SEMICOLON 
                                  { Call ($1, List.rev $3) }
  | IF expr THEN stmts FI         { IfThen ($2, List.rev $4) }
  | IF expr THEN stmts ELSE stmts FI 
                                  { IfThenElse ($2, List.rev $4, List.rev $6) }
  | WHILE expr DO stmts OD        { While ($2, List.rev $4) }
  /* error patterns */
/*  | SEMICOLON                     { parse_error "do not accept empty statement." }
  | WRITE expr                    { parse_error "expected a ';'."}
  | READ lvalue                   { parse_error "expected a ';'."}
  | lvalue ASSIGN rvalue          { parse_error "expected a ';'."}*/

/* -- Right Value of statements -- */
rvalue :
  | expr                          { Rexpr $1 }
  | LBRAC fieldinits RBRAC        { Rstruct (List.rev $2) }
  /* error patterns */
  | UNKNOWN                       { parse_error "unknown token when expecting an expression." }
  |                               { parse_error "do not accept empty right hand side." }

/* -- Field Initializer: g = 0, f = true -- */
fieldinits :
  | fieldinits COMMA fieldinit    { $3 :: $1 }
  | fieldinit                     { [$1] }
  |                               { [] }
  /* error patterns */
  | fieldinits error fieldinit    { parse_error "should be ',' separated." }

fieldinit :
  IDENT EQ rvalue                 { ($1, $3) }

/* -- Left Value of statements -- */
lvalue :
  | IDENT                         { LId $1 }
  | lvalue PERIOD IDENT           { LField ($1, $3) }

/* -- Expressions -- */
exprs :
  | exprs COMMA expr              { $3 :: $1 }
  | expr                          { [$1] }
  |                               { [] }
  /* error patterns */
  | exprs error expr              { parse_error "expressions should be ',' separated." }

expr :
  | BOOL_CONST                    { Ebool $1 }
  | INT_CONST                     { Eint $1 }
  | lvalue                        { Elval $1 }
  /* Binary operators */
  | expr binop expr               { Ebinop ($1,$2,$3) }
  | LPAREN expr binop expr RPAREN { Pebinop ($2, $3, $4) }
  | NOT expr                      { Eunop (Op_not, $2) }
  | MINUS expr %prec UMINUS       { Eunop (Op_minus, $2) }

binop:
  | MINUS                         { Op_sub }
  | PLUS                          { Op_add }
  | MUL                           { Op_mul }
  | DIV                           { Op_div }
  | EQ                            { Op_eq }
  | NEQ                           { Op_neq }
  | LT                            { Op_lt }
  | GT                            { Op_gt }
  | LTE                           { Op_lte }
  | GTE                           { Op_gte }
  | OR                            { Op_or }
  | AND                           { Op_and }
  | LTE                           { Op_lte }
  | GTE                           { Op_gte } 
  | OR                            { Op_or }   
  | AND                           { Op_and }
