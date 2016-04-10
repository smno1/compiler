%{
open Bean_ast

let main_num = ref 0
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

program :
  typedefs procs { { typedefs = List.rev $1 ; procs = List.rev $2 } }

typedefs :
  | typedefs typedef { $2 :: $1 }
  | { [] }

typedef :
  | TYPEDEF typespec IDENT { ($2, $3) }

typespec :
  | BOOL { Bool }
  | INT { Int }
  | LBRAC fielddefs RBRAC { Flist (List.rev $2) } 
  /* comma separated list of field definitions surrounded by {} */
  | IDENT { Id $1 }
  | LBRAC fielddefs           { parse_error "expected '}'."}
  | UNKNOWN                   { parse_error "not a valid identifier." }



fielddef :
  | IDENT COLON typespec { ($1, $3) }
  | UNKNOWN COLON typespec    { parse_error "not a valid identifier." }

fielddefs :
  | fielddefs COMMA fielddef { $3 :: $1 }
  | fielddef { [$1] }

/* Builds procedures in reverse order */
procs :
  | procs proc { $2 :: $1 }
  | proc { [$1] }
  |                           { parse_error "must not have empty typespec."}


proc :
  | PROC procheader procbody END { ($2, $3) }
  | PROC procheader procbody  { parse_error "expected 'end'." }


procheader :
  | IDENT LPAREN params RPAREN { { procname = $1; parameters = (List.rev $3) } }
  | IDENT LPAREN params       { parse_error "expected ')'" }
  | IDENT params              { parse_error "expected '('" }

params :
  | params COMMA param { $3 :: $1 }
  | param { [$1] }
  | { [] }
  | params SEMICOLON param    { parse_error "parameters should be ',' separated." }


param :
  passspec typespec IDENT { ($1, $2, $3) }

passspec :
  | VAL { Val }
  | REF { Ref }

procbody :
  vardecls stmts { { vardecls = List.rev $1 ; stmts = List.rev $2 } }

vardecls :
  | vardecls vardecl { $2 :: $1 }
  | { [] }

vardecl :
  | typespec IDENT SEMICOLON { ($1, $2) }

/* Builds stmts in reverse order */
stmts :
  | stmts stmt { $2 :: $1 }
  | stmt { [$1] }
  |                           { parse_error "expected a statement." }

stmt :
  | lvalue ASSIGN rvalue SEMICOLON { Assign ($1, $3) }
  | READ lvalue SEMICOLON { Read $2 }
  | WRITE expr SEMICOLON { Write $2 }
  | WRITE STRING_CONST SEMICOLON { WriteS $2 }
  | IDENT LPAREN exprs RPAREN SEMICOLON { Call ($1, List.rev $3) }
  | IF expr THEN stmts FI { IfThen ($2, List.rev $4) }
  | IF expr THEN stmts ELSE stmts FI { IfThenElse ($2, List.rev $4, List.rev $6) }
  | WHILE expr DO stmts OD { While ($2, List.rev $4) }
  | SEMICOLON                 { parse_error "do not accept empty statement."}
  | WRITE expr                { parse_error "expected a ';'."}
  | READ lvalue               { parse_error "expected a ';'."}
  | lvalue ASSIGN rvalue      { parse_error "expected a ';'."}

rvalue :
  | expr { Rexpr $1 }
  | LBRAC fieldinits RBRAC    { Rstruct (List.rev $2) }
  | UNKNOWN                   { parse_error "unknown token when expecting an expression." }
  |                           { parse_error "do not accept empty right hand side." }

fieldinits :
  | fieldinits COMMA fieldinit { $3 :: $1 }
  | fieldinit { [$1] }
  | { [] }
  | fieldinits fieldinit
                              { parse_error "should be ',' separated." }
  | fieldinits SEMICOLON fieldinit
                              { parse_error "should be ',' separated." }

fieldinit :
  IDENT EQ rvalue { ($1, $3) }

lvalue :
  | IDENT { LId $1 }
  | lvalue PERIOD IDENT { LField ($1, $3) }

exprs :
  | exprs COMMA expr { $3 :: $1 }
  | exprs error expr          { parse_error "expressions should be ',' separated." }
  | expr { [$1] }
  | { [] }

expr :
  | BOOL_CONST { Ebool $1 }
  | INT_CONST { Eint $1 }
  | lvalue { Elval $1 }
  /* Binary operators */
  | expr binop expr {Ebinop ($1,$2,$3)}
  | LPAREN expr binop expr RPAREN {Pebinop ($2, $3, $4)}
  | NOT expr { Eunop (Op_not, $2) }
  | MINUS expr %prec UMINUS { Eunop (Op_minus, $2) }


binop:
  | MINUS {Op_sub}
  | PLUS {Op_add}
  | MUL {Op_mul}
  | DIV {Op_div}
  | EQ  {Op_eq}
  | NEQ {Op_neq}
  | LT {Op_lt}
  | GT {Op_gt}
  | LTE {Op_lte}
  | GTE {Op_gte}
  | OR {Op_or}
  | AND {Op_and}
  | LTE {Op_lte}
  | GTE {Op_gte} 
  | OR {Op_or}  
  | AND {Op_and}
