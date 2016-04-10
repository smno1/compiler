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

open Parsing;;
# 2 "bean_parse.mly"
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
# 66 "bean_parse.ml"
let yytransl_const = [|
  261 (* BOOL *);
  262 (* INT *);
  263 (* WRITE *);
  264 (* READ *);
  265 (* ASSIGN *);
  266 (* LPAREN *);
  267 (* RPAREN *);
  268 (* LBRAC *);
  269 (* RBRAC *);
  270 (* EQ *);
  271 (* LT *);
  272 (* GT *);
  273 (* GTE *);
  274 (* LTE *);
  275 (* NEQ *);
  276 (* PLUS *);
  277 (* MINUS *);
  278 (* MUL *);
  279 (* DIV *);
  280 (* SEMICOLON *);
  281 (* PERIOD *);
  282 (* COLON *);
  283 (* COMMA *);
    0 (* EOF *);
  284 (* AND *);
  285 (* OR *);
  286 (* NOT *);
  287 (* WHILE *);
  288 (* DO *);
  289 (* OD *);
  290 (* IF *);
  291 (* ELSE *);
  292 (* THEN *);
  293 (* FI *);
  294 (* END *);
  295 (* PROC *);
  296 (* REF *);
  297 (* VAL *);
  298 (* TYPEDEF *);
  299 (* UNKNOWN *);
    0|]

let yytransl_block = [|
  257 (* BOOL_CONST *);
  258 (* INT_CONST *);
  259 (* IDENT *);
  260 (* STRING_CONST *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\005\000\005\000\005\000\005\000\
\005\000\005\000\007\000\007\000\006\000\006\000\003\000\003\000\
\003\000\008\000\008\000\009\000\009\000\009\000\011\000\011\000\
\011\000\011\000\012\000\013\000\013\000\010\000\014\000\014\000\
\016\000\015\000\015\000\015\000\017\000\017\000\017\000\017\000\
\017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
\019\000\019\000\019\000\019\000\022\000\022\000\022\000\022\000\
\022\000\023\000\018\000\018\000\021\000\021\000\021\000\021\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\024\000\
\024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
\024\000\024\000\024\000\024\000\024\000\024\000\024\000\000\000"

let yylen = "\002\000\
\002\000\002\000\000\000\003\000\001\000\001\000\003\000\001\000\
\002\000\001\000\003\000\003\000\003\000\001\000\002\000\001\000\
\000\000\004\000\003\000\004\000\003\000\002\000\003\000\001\000\
\000\000\003\000\003\000\001\000\001\000\002\000\002\000\000\000\
\003\000\002\000\001\000\000\000\004\000\003\000\003\000\003\000\
\005\000\005\000\007\000\005\000\001\000\002\000\002\000\003\000\
\001\000\003\000\001\000\000\000\003\000\001\000\000\000\002\000\
\003\000\003\000\001\000\003\000\003\000\003\000\001\000\000\000\
\001\000\001\000\001\000\003\000\005\000\002\000\002\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\003\000\000\000\088\000\000\000\000\000\000\000\000\000\002\000\
\016\000\000\000\032\000\008\000\005\000\006\000\000\000\010\000\
\000\000\015\000\000\000\029\000\028\000\000\000\024\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\004\000\000\000\
\000\000\000\000\000\000\018\000\000\000\000\000\000\000\045\000\
\000\000\000\000\000\000\000\000\031\000\035\000\000\000\000\000\
\000\000\007\000\000\000\020\000\026\000\023\000\027\000\000\000\
\065\000\066\000\059\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\034\000\000\000\
\000\000\011\000\012\000\013\000\000\000\000\000\040\000\000\000\
\000\000\000\000\076\000\078\000\079\000\081\000\080\000\077\000\
\073\000\072\000\074\000\075\000\039\000\083\000\082\000\000\000\
\038\000\000\000\000\000\033\000\000\000\051\000\000\000\000\000\
\060\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\054\000\037\000\000\000\041\000\000\000\000\000\
\044\000\000\000\042\000\000\000\050\000\000\000\000\000\056\000\
\069\000\000\000\058\000\057\000\053\000\043\000"

let yydgoto = "\002\000\
\003\000\004\000\007\000\008\000\017\000\029\000\030\000\009\000\
\011\000\025\000\022\000\023\000\024\000\026\000\044\000\045\000\
\046\000\064\000\103\000\104\000\078\000\114\000\115\000\096\000"

let yysindex = "\019\000\
\000\000\000\000\000\000\072\255\037\255\016\255\032\255\000\000\
\000\000\253\254\000\000\000\000\000\000\000\000\012\255\000\000\
\081\255\000\000\227\254\000\000\000\000\107\255\000\000\016\255\
\063\255\044\255\077\255\083\255\070\255\000\000\000\000\139\255\
\227\254\227\254\113\255\000\000\100\255\116\255\121\255\000\000\
\141\255\141\255\132\255\105\255\000\000\000\000\023\255\016\255\
\016\255\000\000\012\255\000\000\000\000\000\000\000\000\141\255\
\000\000\000\000\000\000\117\255\141\255\141\255\141\255\123\255\
\041\001\017\255\190\255\167\255\128\255\100\255\000\000\024\255\
\154\255\000\000\000\000\000\000\057\001\033\255\000\000\057\001\
\051\255\057\001\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\141\255\
\000\000\105\255\105\255\000\000\157\255\000\000\140\255\057\001\
\000\000\141\255\143\255\141\255\141\255\057\001\099\255\058\255\
\155\255\146\255\000\000\000\000\057\001\000\000\057\001\212\255\
\000\000\105\255\000\000\024\255\000\000\157\255\157\255\000\000\
\000\000\091\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\168\000\000\000\000\000\175\000\000\000\
\000\000\014\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\053\000\000\000\000\000\
\008\000\004\000\000\000\000\000\188\255\000\000\000\000\063\000\
\000\000\000\000\000\000\000\000\014\255\000\000\000\000\000\000\
\000\000\000\000\000\000\016\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\035\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\100\000\
\043\000\238\000\000\000\000\000\000\000\096\255\000\000\161\000\
\000\000\000\000\000\000\000\000\127\255\000\000\000\000\000\000\
\140\000\180\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\159\255\224\254\000\000\152\255\000\000\015\001\001\001\
\000\000\000\000\000\000\000\000\000\000\220\000\000\000\000\000\
\000\000\000\000\000\000\000\000\145\255\000\000\147\255\000\000\
\000\000\156\255\000\000\223\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\129\000\000\000\143\000\190\000\
\000\000\000\000\179\000\057\000\000\000\000\000\215\255\000\000\
\214\255\230\255\075\000\224\255\000\000\000\000\088\000\120\000"

let yytablesize = 598
let yytable = "\047\000\
\025\000\071\000\036\000\036\000\036\000\065\000\019\000\019\000\
\067\000\068\000\020\000\021\000\066\000\025\000\027\000\030\000\
\008\000\047\000\012\000\001\000\013\000\014\000\059\000\077\000\
\057\000\058\000\059\000\015\000\080\000\081\000\082\000\072\000\
\106\000\061\000\064\000\101\000\020\000\021\000\059\000\010\000\
\097\000\073\000\046\000\107\000\062\000\064\000\037\000\073\000\
\013\000\014\000\038\000\039\000\022\000\063\000\028\000\015\000\
\111\000\112\000\016\000\108\000\070\000\064\000\021\000\110\000\
\038\000\039\000\102\000\040\000\071\000\071\000\005\000\047\000\
\047\000\117\000\041\000\119\000\120\000\042\000\094\000\095\000\
\130\000\040\000\050\000\031\000\047\000\047\000\016\000\071\000\
\041\000\053\000\054\000\042\000\122\000\070\000\123\000\047\000\
\051\000\038\000\039\000\067\000\036\000\070\000\048\000\047\000\
\059\000\038\000\039\000\070\000\049\000\056\000\005\000\038\000\
\039\000\006\000\040\000\055\000\057\000\058\000\059\000\060\000\
\059\000\041\000\040\000\059\000\042\000\061\000\063\000\134\000\
\040\000\041\000\033\000\121\000\042\000\034\000\069\000\041\000\
\062\000\063\000\042\000\071\000\079\000\057\000\058\000\059\000\
\062\000\063\000\061\000\073\000\113\000\052\000\061\000\100\000\
\035\000\063\000\043\000\062\000\105\000\061\000\125\000\113\000\
\052\000\062\000\033\000\116\000\055\000\034\000\118\000\017\000\
\124\000\126\000\063\000\062\000\127\000\061\000\001\000\055\000\
\074\000\075\000\055\000\070\000\083\000\084\000\085\000\086\000\
\087\000\088\000\089\000\090\000\091\000\092\000\009\000\036\000\
\036\000\076\000\094\000\095\000\018\000\032\000\131\000\109\000\
\000\000\128\000\099\000\083\000\084\000\085\000\086\000\087\000\
\088\000\089\000\090\000\091\000\092\000\132\000\133\000\000\000\
\000\000\094\000\095\000\068\000\000\000\098\000\129\000\000\000\
\000\000\083\000\084\000\085\000\086\000\087\000\088\000\089\000\
\090\000\091\000\092\000\052\000\000\000\047\000\000\000\094\000\
\095\000\000\000\000\000\000\000\000\000\000\000\052\000\000\000\
\000\000\052\000\000\000\000\000\000\000\000\000\000\000\000\000\
\049\000\000\000\000\000\025\000\000\000\025\000\025\000\025\000\
\025\000\000\000\000\000\025\000\025\000\000\000\048\000\000\000\
\025\000\000\000\025\000\025\000\025\000\025\000\000\000\000\000\
\025\000\025\000\000\000\025\000\000\000\000\000\000\000\025\000\
\000\000\000\000\025\000\000\000\000\000\025\000\025\000\025\000\
\025\000\036\000\036\000\025\000\025\000\046\000\019\000\025\000\
\000\000\046\000\046\000\025\000\025\000\030\000\030\000\022\000\
\025\000\022\000\022\000\022\000\022\000\000\000\000\000\000\000\
\022\000\021\000\000\000\021\000\021\000\021\000\021\000\000\000\
\000\000\046\000\021\000\046\000\046\000\046\000\000\000\046\000\
\046\000\046\000\000\000\022\000\000\000\000\000\022\000\000\000\
\000\000\000\000\022\000\022\000\000\000\021\000\000\000\022\000\
\021\000\000\000\000\000\067\000\021\000\021\000\067\000\000\000\
\000\000\021\000\067\000\067\000\000\000\000\000\067\000\000\000\
\067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
\067\000\067\000\067\000\067\000\000\000\000\000\067\000\067\000\
\067\000\000\000\067\000\067\000\067\000\067\000\067\000\067\000\
\067\000\067\000\067\000\071\000\000\000\000\000\071\000\000\000\
\000\000\000\000\071\000\071\000\000\000\000\000\071\000\000\000\
\071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
\071\000\071\000\071\000\071\000\000\000\000\000\071\000\052\000\
\052\000\000\000\071\000\071\000\071\000\071\000\071\000\071\000\
\071\000\071\000\071\000\070\000\000\000\000\000\070\000\000\000\
\052\000\000\000\070\000\070\000\000\000\000\000\070\000\052\000\
\070\000\052\000\052\000\052\000\000\000\052\000\052\000\052\000\
\000\000\000\000\000\000\070\000\000\000\000\000\070\000\000\000\
\000\000\000\000\070\000\070\000\070\000\070\000\070\000\070\000\
\070\000\070\000\070\000\068\000\000\000\000\000\068\000\000\000\
\000\000\000\000\068\000\068\000\000\000\000\000\068\000\000\000\
\068\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\047\000\000\000\000\000\068\000\047\000\047\000\068\000\000\000\
\000\000\000\000\068\000\068\000\068\000\068\000\068\000\068\000\
\068\000\068\000\068\000\049\000\000\000\000\000\000\000\049\000\
\049\000\000\000\000\000\000\000\047\000\049\000\047\000\047\000\
\047\000\048\000\047\000\047\000\047\000\048\000\048\000\000\000\
\049\000\000\000\000\000\049\000\000\000\000\000\000\000\049\000\
\000\000\049\000\049\000\049\000\000\000\049\000\049\000\049\000\
\000\000\000\000\000\000\000\000\000\000\048\000\000\000\048\000\
\048\000\048\000\000\000\048\000\048\000\048\000\083\000\084\000\
\085\000\086\000\087\000\088\000\089\000\090\000\091\000\092\000\
\093\000\000\000\000\000\000\000\094\000\095\000\083\000\084\000\
\085\000\086\000\087\000\088\000\089\000\090\000\091\000\092\000\
\000\000\000\000\000\000\000\000\094\000\095\000"

let yycheck = "\026\000\
\000\000\044\000\035\001\000\000\037\001\038\000\010\001\000\000\
\041\000\042\000\040\001\041\001\039\000\000\000\003\001\000\000\
\003\001\044\000\003\001\001\000\005\001\006\001\009\001\056\000\
\001\001\002\001\003\001\012\001\061\000\062\000\063\000\009\001\
\000\001\010\001\000\001\012\001\040\001\041\001\025\001\003\001\
\024\001\025\001\000\000\011\001\021\001\011\001\003\001\025\001\
\005\001\006\001\007\001\008\001\000\000\030\001\043\001\012\001\
\098\000\099\000\043\001\027\001\003\001\027\001\000\000\096\000\
\007\001\008\001\043\001\024\001\111\000\112\000\039\001\098\000\
\099\000\106\000\031\001\108\000\109\000\034\001\028\001\029\001\
\122\000\024\001\013\001\003\001\111\000\112\000\043\001\130\000\
\031\001\033\000\034\000\034\001\035\001\003\001\037\001\122\000\
\027\001\007\001\008\001\000\000\038\001\003\001\026\001\130\000\
\009\001\007\001\008\001\003\001\026\001\010\001\039\001\007\001\
\008\001\042\001\024\001\003\001\001\001\002\001\003\001\004\001\
\025\001\031\001\024\001\003\001\034\001\010\001\000\001\037\001\
\024\001\031\001\024\001\033\001\034\001\027\001\003\001\031\001\
\021\001\011\001\034\001\000\000\024\001\001\001\002\001\003\001\
\000\001\030\001\000\001\025\001\003\001\011\001\010\001\024\001\
\024\000\027\001\026\000\011\001\003\001\011\001\013\001\003\001\
\000\000\021\001\024\001\024\001\013\001\027\001\024\001\000\000\
\014\001\024\001\030\001\027\001\027\001\027\001\000\000\024\001\
\048\000\049\000\027\001\000\000\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\003\001\033\001\
\037\001\051\000\028\001\029\001\007\000\019\000\124\000\080\000\
\255\255\114\000\036\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\126\000\127\000\255\255\
\255\255\028\001\029\001\000\000\255\255\032\001\011\001\255\255\
\255\255\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\013\001\255\255\000\000\255\255\028\001\
\029\001\255\255\255\255\255\255\255\255\255\255\024\001\255\255\
\255\255\027\001\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\255\255\255\255\003\001\255\255\005\001\006\001\007\001\
\008\001\255\255\255\255\011\001\012\001\255\255\000\000\255\255\
\003\001\255\255\005\001\006\001\007\001\008\001\255\255\255\255\
\024\001\012\001\255\255\027\001\255\255\255\255\255\255\031\001\
\255\255\255\255\034\001\255\255\255\255\024\001\038\001\039\001\
\027\001\038\001\039\001\043\001\031\001\003\001\039\001\034\001\
\255\255\007\001\008\001\038\001\039\001\038\001\039\001\003\001\
\043\001\005\001\006\001\007\001\008\001\255\255\255\255\255\255\
\012\001\003\001\255\255\005\001\006\001\007\001\008\001\255\255\
\255\255\031\001\012\001\033\001\034\001\035\001\255\255\037\001\
\038\001\039\001\255\255\031\001\255\255\255\255\034\001\255\255\
\255\255\255\255\038\001\039\001\255\255\031\001\255\255\043\001\
\034\001\255\255\255\255\000\001\038\001\039\001\003\001\255\255\
\255\255\043\001\007\001\008\001\255\255\255\255\011\001\255\255\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\255\255\255\255\027\001\028\001\
\029\001\255\255\031\001\032\001\033\001\034\001\035\001\036\001\
\037\001\038\001\039\001\000\001\255\255\255\255\003\001\255\255\
\255\255\255\255\007\001\008\001\255\255\255\255\011\001\255\255\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\255\255\255\255\027\001\007\001\
\008\001\255\255\031\001\032\001\033\001\034\001\035\001\036\001\
\037\001\038\001\039\001\000\001\255\255\255\255\003\001\255\255\
\024\001\255\255\007\001\008\001\255\255\255\255\011\001\031\001\
\013\001\033\001\034\001\035\001\255\255\037\001\038\001\039\001\
\255\255\255\255\255\255\024\001\255\255\255\255\027\001\255\255\
\255\255\255\255\031\001\032\001\033\001\034\001\035\001\036\001\
\037\001\038\001\039\001\000\001\255\255\255\255\003\001\255\255\
\255\255\255\255\007\001\008\001\255\255\255\255\011\001\255\255\
\013\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\003\001\255\255\255\255\024\001\007\001\008\001\027\001\255\255\
\255\255\255\255\031\001\032\001\033\001\034\001\035\001\036\001\
\037\001\038\001\039\001\003\001\255\255\255\255\255\255\007\001\
\008\001\255\255\255\255\255\255\031\001\013\001\033\001\034\001\
\035\001\003\001\037\001\038\001\039\001\007\001\008\001\255\255\
\024\001\255\255\255\255\027\001\255\255\255\255\255\255\031\001\
\255\255\033\001\034\001\035\001\255\255\037\001\038\001\039\001\
\255\255\255\255\255\255\255\255\255\255\031\001\255\255\033\001\
\034\001\035\001\255\255\037\001\038\001\039\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\255\255\255\255\255\255\028\001\029\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\255\255\255\255\255\255\255\255\028\001\029\001"

let yynames_const = "\
  BOOL\000\
  INT\000\
  WRITE\000\
  READ\000\
  ASSIGN\000\
  LPAREN\000\
  RPAREN\000\
  LBRAC\000\
  RBRAC\000\
  EQ\000\
  LT\000\
  GT\000\
  GTE\000\
  LTE\000\
  NEQ\000\
  PLUS\000\
  MINUS\000\
  MUL\000\
  DIV\000\
  SEMICOLON\000\
  PERIOD\000\
  COLON\000\
  COMMA\000\
  EOF\000\
  AND\000\
  OR\000\
  NOT\000\
  WHILE\000\
  DO\000\
  OD\000\
  IF\000\
  ELSE\000\
  THEN\000\
  FI\000\
  END\000\
  PROC\000\
  REF\000\
  VAL\000\
  TYPEDEF\000\
  UNKNOWN\000\
  "

let yynames_block = "\
  BOOL_CONST\000\
  INT_CONST\000\
  IDENT\000\
  STRING_CONST\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typedefs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'procs) in
    Obj.repr(
# 51 "bean_parse.mly"
                 ( { typedefs = List.rev _1 ; procs = List.rev _2 } )
# 423 "bean_parse.ml"
               : Bean_ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typedefs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typedef) in
    Obj.repr(
# 54 "bean_parse.mly"
                     ( _2 :: _1 )
# 431 "bean_parse.ml"
               : 'typedefs))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "bean_parse.mly"
    ( [] )
# 437 "bean_parse.ml"
               : 'typedefs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typespec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "bean_parse.mly"
                           ( (_2, _3) )
# 445 "bean_parse.ml"
               : 'typedef))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "bean_parse.mly"
         ( Bool )
# 451 "bean_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "bean_parse.mly"
        ( Int )
# 457 "bean_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fielddefs) in
    Obj.repr(
# 63 "bean_parse.mly"
                          ( Flist (List.rev _2) )
# 464 "bean_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "bean_parse.mly"
          ( Id _1 )
# 471 "bean_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fielddefs) in
    Obj.repr(
# 66 "bean_parse.mly"
                              ( parse_error "expected '}'.")
# 478 "bean_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "bean_parse.mly"
                              ( parse_error "not a valid identifier." )
# 484 "bean_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typespec) in
    Obj.repr(
# 72 "bean_parse.mly"
                         ( (_1, _3) )
# 492 "bean_parse.ml"
               : 'fielddef))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typespec) in
    Obj.repr(
# 73 "bean_parse.mly"
                              ( parse_error "not a valid identifier." )
# 499 "bean_parse.ml"
               : 'fielddef))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fielddefs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fielddef) in
    Obj.repr(
# 76 "bean_parse.mly"
                             ( _3 :: _1 )
# 507 "bean_parse.ml"
               : 'fielddefs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fielddef) in
    Obj.repr(
# 77 "bean_parse.mly"
             ( [_1] )
# 514 "bean_parse.ml"
               : 'fielddefs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'procs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'proc) in
    Obj.repr(
# 81 "bean_parse.mly"
               ( _2 :: _1 )
# 522 "bean_parse.ml"
               : 'procs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proc) in
    Obj.repr(
# 82 "bean_parse.mly"
         ( [_1] )
# 529 "bean_parse.ml"
               : 'procs))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "bean_parse.mly"
                              ( parse_error "must not have empty typespec.")
# 535 "bean_parse.ml"
               : 'procs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'procheader) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'procbody) in
    Obj.repr(
# 87 "bean_parse.mly"
                                 ( (_2, _3) )
# 543 "bean_parse.ml"
               : 'proc))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'procheader) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'procbody) in
    Obj.repr(
# 88 "bean_parse.mly"
                              ( parse_error "expected 'end'." )
# 551 "bean_parse.ml"
               : 'proc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'params) in
    Obj.repr(
# 92 "bean_parse.mly"
                               ( { procname = _1; parameters = (List.rev _3) } )
# 559 "bean_parse.ml"
               : 'procheader))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'params) in
    Obj.repr(
# 93 "bean_parse.mly"
                              ( parse_error "expected ')'" )
# 567 "bean_parse.ml"
               : 'procheader))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'params) in
    Obj.repr(
# 94 "bean_parse.mly"
                              ( parse_error "expected '('" )
# 575 "bean_parse.ml"
               : 'procheader))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'params) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 97 "bean_parse.mly"
                       ( _3 :: _1 )
# 583 "bean_parse.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 98 "bean_parse.mly"
          ( [_1] )
# 590 "bean_parse.ml"
               : 'params))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "bean_parse.mly"
    ( [] )
# 596 "bean_parse.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'params) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 100 "bean_parse.mly"
                              ( parse_error "parameters should be ',' separated." )
# 604 "bean_parse.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'passspec) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typespec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 104 "bean_parse.mly"
                          ( (_1, _2, _3) )
# 613 "bean_parse.ml"
               : 'param))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "bean_parse.mly"
        ( Val )
# 619 "bean_parse.ml"
               : 'passspec))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "bean_parse.mly"
        ( Ref )
# 625 "bean_parse.ml"
               : 'passspec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vardecls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 111 "bean_parse.mly"
                 ( { vardecls = List.rev _1 ; stmts = List.rev _2 } )
# 633 "bean_parse.ml"
               : 'procbody))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vardecls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vardecl) in
    Obj.repr(
# 114 "bean_parse.mly"
                     ( _2 :: _1 )
# 641 "bean_parse.ml"
               : 'vardecls))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "bean_parse.mly"
    ( [] )
# 647 "bean_parse.ml"
               : 'vardecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typespec) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 118 "bean_parse.mly"
                             ( (_1, _2) )
# 655 "bean_parse.ml"
               : 'vardecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 122 "bean_parse.mly"
               ( _2 :: _1 )
# 663 "bean_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 123 "bean_parse.mly"
         ( [_1] )
# 670 "bean_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "bean_parse.mly"
                              ( parse_error "expected a statement." )
# 676 "bean_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'rvalue) in
    Obj.repr(
# 127 "bean_parse.mly"
                                   ( Assign (_1, _3) )
# 684 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'lvalue) in
    Obj.repr(
# 128 "bean_parse.mly"
                          ( Read _2 )
# 691 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 129 "bean_parse.mly"
                         ( Write _2 )
# 698 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 130 "bean_parse.mly"
                                 ( WriteS _2 )
# 705 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'exprs) in
    Obj.repr(
# 131 "bean_parse.mly"
                                        ( Call (_1, _3) )
# 713 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 132 "bean_parse.mly"
                          ( IfThen (_2, _4) )
# 721 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'stmts) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 133 "bean_parse.mly"
                                     ( IfThenElse (_2, _4, _6) )
# 730 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 134 "bean_parse.mly"
                           ( While (_2, _4) )
# 738 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "bean_parse.mly"
                              ( parse_error "do not accept empty statement.")
# 744 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "bean_parse.mly"
                              ( parse_error "expected a ';'.")
# 751 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 137 "bean_parse.mly"
                              ( parse_error "expected a ';'.")
# 758 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 138 "bean_parse.mly"
                              ( parse_error "expected a ';'.")
# 766 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "bean_parse.mly"
         ( Rexpr _1 )
# 773 "bean_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fieldinits) in
    Obj.repr(
# 142 "bean_parse.mly"
                           ( Rstruct _2 )
# 780 "bean_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "bean_parse.mly"
                              ( parse_error "unknown token when expecting an expression." )
# 786 "bean_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    Obj.repr(
# 144 "bean_parse.mly"
                              ( parse_error "do not accept empty right hand side." )
# 792 "bean_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fieldinits) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fieldinit) in
    Obj.repr(
# 147 "bean_parse.mly"
                               ( _3 :: _1 )
# 800 "bean_parse.ml"
               : 'fieldinits))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fieldinit) in
    Obj.repr(
# 148 "bean_parse.mly"
              ( [_1] )
# 807 "bean_parse.ml"
               : 'fieldinits))
; (fun __caml_parser_env ->
    Obj.repr(
# 149 "bean_parse.mly"
    ( [] )
# 813 "bean_parse.ml"
               : 'fieldinits))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fieldinits) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fieldinit) in
    Obj.repr(
# 151 "bean_parse.mly"
                              ( parse_error "should be ',' separated." )
# 821 "bean_parse.ml"
               : 'fieldinits))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fieldinits) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fieldinit) in
    Obj.repr(
# 153 "bean_parse.mly"
                              ( parse_error "should be ',' separated." )
# 829 "bean_parse.ml"
               : 'fieldinits))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 156 "bean_parse.mly"
                  ( (_1, _3) )
# 837 "bean_parse.ml"
               : 'fieldinit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 159 "bean_parse.mly"
          ( LId _1 )
# 844 "bean_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 160 "bean_parse.mly"
                        ( LField (_1, _3) )
# 852 "bean_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exprs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 163 "bean_parse.mly"
                     ( _3 :: _1 )
# 860 "bean_parse.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exprs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 164 "bean_parse.mly"
                              ( parse_error "expressions should be ',' separated." )
# 868 "bean_parse.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 165 "bean_parse.mly"
         ( [_1] )
# 875 "bean_parse.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    Obj.repr(
# 166 "bean_parse.mly"
    ( [] )
# 881 "bean_parse.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 169 "bean_parse.mly"
               ( Ebool _1 )
# 888 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 170 "bean_parse.mly"
              ( Eint _1 )
# 895 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 171 "bean_parse.mly"
           ( Elval _1 )
# 902 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'binop) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 173 "bean_parse.mly"
                    (Ebinop (_1,_2,_3))
# 911 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'binop) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 174 "bean_parse.mly"
                                  (Pebinop (_2, _3, _4))
# 920 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 175 "bean_parse.mly"
             ( Eunop (Op_not, _2) )
# 927 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 176 "bean_parse.mly"
                            ( Eunop (Op_minus, _2) )
# 934 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 180 "bean_parse.mly"
          (Op_sub)
# 940 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 181 "bean_parse.mly"
         (Op_add)
# 946 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 182 "bean_parse.mly"
        (Op_mul)
# 952 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 183 "bean_parse.mly"
        (Op_div)
# 958 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 184 "bean_parse.mly"
        (Op_eq)
# 964 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 185 "bean_parse.mly"
        (Op_neq)
# 970 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 186 "bean_parse.mly"
       (Op_lt)
# 976 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 187 "bean_parse.mly"
       (Op_gt)
# 982 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 188 "bean_parse.mly"
        (Op_lte)
# 988 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 189 "bean_parse.mly"
        (Op_gte)
# 994 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 190 "bean_parse.mly"
       (Op_or)
# 1000 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 191 "bean_parse.mly"
        (Op_and)
# 1006 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 192 "bean_parse.mly"
        (Op_lte)
# 1012 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 193 "bean_parse.mly"
        (Op_gte)
# 1018 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 194 "bean_parse.mly"
       (Op_or)
# 1024 "bean_parse.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 195 "bean_parse.mly"
        (Op_and)
# 1030 "bean_parse.ml"
               : 'binop))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Bean_ast.program)
