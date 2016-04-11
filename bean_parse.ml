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
let _ = parse_error;;
# 2 "bean_parse.mly"
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
# 69 "bean_parse.ml"
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
\007\000\007\000\007\000\006\000\006\000\006\000\006\000\003\000\
\003\000\008\000\009\000\009\000\009\000\011\000\011\000\011\000\
\012\000\013\000\013\000\010\000\014\000\014\000\016\000\015\000\
\015\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
\017\000\019\000\019\000\019\000\019\000\022\000\022\000\022\000\
\022\000\023\000\018\000\018\000\021\000\021\000\021\000\021\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\000\000"

let yylen = "\002\000\
\002\000\002\000\000\000\003\000\001\000\001\000\003\000\001\000\
\003\000\003\000\003\000\003\000\001\000\003\000\000\000\002\000\
\001\000\004\000\004\000\003\000\002\000\003\000\001\000\000\000\
\003\000\001\000\001\000\002\000\002\000\000\000\003\000\002\000\
\001\000\004\000\003\000\003\000\003\000\005\000\005\000\007\000\
\005\000\001\000\003\000\001\000\000\000\003\000\001\000\000\000\
\003\000\003\000\001\000\003\000\003\000\001\000\000\000\003\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000\002\000\002\000"

let yydefred = "\000\000\
\003\000\000\000\075\000\000\000\000\000\000\000\000\000\002\000\
\017\000\000\000\030\000\008\000\005\000\006\000\000\000\000\000\
\016\000\000\000\027\000\026\000\000\000\023\000\000\000\000\000\
\000\000\000\000\000\000\000\000\013\000\004\000\000\000\000\000\
\000\000\018\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\029\000\033\000\000\000\000\000\000\000\007\000\000\000\
\000\000\019\000\022\000\025\000\000\000\057\000\058\000\051\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\032\000\000\000\000\000\011\000\009\000\
\010\000\014\000\012\000\000\000\000\000\037\000\000\000\074\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\036\000\000\000\000\000\035\000\000\000\
\000\000\031\000\000\000\044\000\000\000\000\000\052\000\000\000\
\000\000\000\000\060\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\071\000\072\000\000\000\000\000\000\000\
\000\000\000\000\000\000\047\000\034\000\000\000\038\000\000\000\
\041\000\000\000\039\000\000\000\000\000\043\000\000\000\000\000\
\050\000\049\000\046\000\040\000"

let yydgoto = "\002\000\
\003\000\004\000\007\000\008\000\016\000\028\000\029\000\009\000\
\011\000\024\000\021\000\022\000\023\000\025\000\041\000\042\000\
\043\000\061\000\101\000\102\000\077\000\123\000\124\000"

let yysindex = "\014\000\
\000\000\000\000\000\000\004\255\041\255\008\255\010\255\000\000\
\000\000\255\254\000\000\000\000\000\000\000\000\254\254\058\255\
\000\000\240\254\000\000\000\000\042\255\000\000\008\255\045\255\
\087\000\072\255\076\255\022\000\000\000\000\000\057\255\240\254\
\121\255\000\000\122\255\137\000\130\255\106\000\106\000\131\255\
\230\255\000\000\000\000\001\255\149\000\008\255\000\000\254\254\
\254\254\000\000\000\000\000\000\106\000\000\000\000\000\000\000\
\113\255\106\000\106\000\106\000\123\255\212\000\068\255\177\000\
\154\000\132\255\122\255\000\000\064\255\152\255\000\000\000\000\
\000\000\000\000\000\000\228\000\021\255\000\000\196\000\000\000\
\003\001\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
\106\000\106\000\106\000\000\000\106\000\106\000\000\000\230\255\
\230\255\000\000\155\255\000\000\135\255\228\000\000\000\106\000\
\158\255\106\000\000\000\122\000\122\000\122\000\122\000\122\000\
\122\000\086\255\086\255\000\000\000\000\003\001\244\000\182\255\
\020\000\162\255\018\255\000\000\000\000\228\000\000\000\228\000\
\000\000\230\255\000\000\064\255\155\255\000\000\155\255\036\000\
\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\188\000\000\000\
\000\000\079\000\000\000\000\000\000\000\000\000\035\000\000\000\
\000\000\069\000\000\000\000\000\117\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\125\000\000\000\
\000\000\000\000\061\255\000\000\000\000\000\000\000\000\000\000\
\153\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\077\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\099\255\000\000\000\000\000\000\
\000\000\000\000\078\255\000\000\170\255\000\000\000\000\000\000\
\000\000\000\000\000\000\079\255\000\000\000\000\000\000\000\000\
\006\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\147\255\000\000\000\000\023\255\000\000\000\000\
\000\000\000\000\000\000\173\255\193\255\199\255\219\255\239\255\
\245\255\125\255\151\255\000\000\000\000\009\000\029\000\000\000\
\000\000\000\000\000\000\000\000\000\000\181\255\000\000\207\255\
\000\000\000\000\000\000\150\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\055\000\000\000\081\000\189\000\
\000\000\000\000\180\000\163\000\000\000\000\000\163\255\000\000\
\217\255\231\255\071\000\225\255\000\000\000\000\212\255"

let yytablesize = 538
let yytable = "\044\000\
\026\000\068\000\120\000\121\000\062\000\073\000\064\000\065\000\
\018\000\069\000\012\000\063\000\013\000\014\000\001\000\044\000\
\073\000\133\000\073\000\015\000\104\000\076\000\042\000\019\000\
\020\000\070\000\079\000\080\000\081\000\073\000\134\000\105\000\
\073\000\073\000\073\000\042\000\136\000\073\000\019\000\020\000\
\027\000\073\000\005\000\010\000\135\000\006\000\042\000\106\000\
\005\000\042\000\108\000\109\000\110\000\111\000\112\000\113\000\
\114\000\115\000\116\000\117\000\030\000\118\000\119\000\008\000\
\054\000\055\000\056\000\050\000\032\000\051\000\044\000\044\000\
\126\000\058\000\128\000\099\000\055\000\033\000\054\000\040\000\
\068\000\068\000\034\000\032\000\059\000\051\000\051\000\055\000\
\138\000\054\000\139\000\095\000\070\000\060\000\044\000\044\000\
\068\000\045\000\059\000\072\000\073\000\046\000\051\000\055\000\
\044\000\054\000\100\000\090\000\091\000\059\000\044\000\059\000\
\059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
\059\000\059\000\059\000\052\000\069\000\059\000\059\000\059\000\
\074\000\075\000\059\000\053\000\056\000\066\000\059\000\069\000\
\078\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
\069\000\069\000\048\000\070\000\069\000\045\000\070\000\069\000\
\069\000\069\000\103\000\098\000\069\000\122\000\125\000\048\000\
\069\000\070\000\045\000\070\000\070\000\070\000\070\000\070\000\
\070\000\070\000\070\000\070\000\063\000\048\000\070\000\132\000\
\045\000\070\000\070\000\070\000\056\000\127\000\070\000\063\000\
\067\000\063\000\070\000\001\000\036\000\037\000\028\000\056\000\
\065\000\045\000\051\000\017\000\063\000\031\000\067\000\063\000\
\063\000\063\000\137\000\065\000\063\000\065\000\053\000\056\000\
\063\000\067\000\000\000\067\000\038\000\000\000\129\000\039\000\
\065\000\053\000\068\000\065\000\065\000\065\000\067\000\000\000\
\065\000\067\000\067\000\067\000\065\000\068\000\067\000\068\000\
\067\000\053\000\067\000\000\000\036\000\037\000\066\000\000\000\
\000\000\000\000\068\000\000\000\064\000\068\000\068\000\068\000\
\000\000\066\000\068\000\066\000\000\000\000\000\068\000\064\000\
\000\000\064\000\000\000\000\000\038\000\000\000\066\000\039\000\
\062\000\066\000\066\000\066\000\064\000\000\000\066\000\064\000\
\064\000\064\000\066\000\062\000\064\000\062\000\067\000\000\000\
\064\000\000\000\036\000\037\000\061\000\000\000\000\000\000\000\
\062\000\000\000\047\000\062\000\062\000\062\000\067\000\061\000\
\062\000\061\000\036\000\037\000\062\000\048\000\000\000\015\000\
\049\000\000\000\038\000\000\000\061\000\039\000\130\000\061\000\
\131\000\061\000\015\000\000\000\061\000\015\000\000\000\000\000\
\061\000\000\000\038\000\000\000\000\000\039\000\000\000\024\000\
\140\000\024\000\024\000\024\000\024\000\000\000\000\000\024\000\
\024\000\024\000\000\000\024\000\024\000\024\000\024\000\000\000\
\000\000\035\000\024\000\013\000\014\000\036\000\037\000\024\000\
\000\000\000\000\015\000\024\000\000\000\000\000\024\000\000\000\
\000\000\024\000\054\000\055\000\056\000\024\000\000\000\000\000\
\024\000\000\000\000\000\058\000\000\000\038\000\000\000\021\000\
\039\000\021\000\021\000\021\000\021\000\000\000\059\000\020\000\
\021\000\020\000\020\000\020\000\020\000\000\000\000\000\060\000\
\020\000\054\000\055\000\056\000\057\000\088\000\089\000\090\000\
\091\000\000\000\058\000\021\000\071\000\000\000\021\000\012\000\
\000\000\013\000\014\000\020\000\000\000\059\000\020\000\000\000\
\015\000\000\000\000\000\000\000\000\000\000\000\060\000\082\000\
\083\000\084\000\085\000\086\000\087\000\088\000\089\000\090\000\
\091\000\000\000\000\000\000\000\000\000\093\000\094\000\000\000\
\000\000\000\000\000\000\000\000\000\000\097\000\082\000\083\000\
\084\000\085\000\086\000\087\000\088\000\089\000\090\000\091\000\
\000\000\000\000\000\000\000\000\093\000\094\000\107\000\000\000\
\096\000\082\000\083\000\084\000\085\000\086\000\087\000\088\000\
\089\000\090\000\091\000\000\000\000\000\000\000\000\000\093\000\
\094\000\082\000\083\000\084\000\085\000\086\000\087\000\088\000\
\089\000\090\000\091\000\092\000\000\000\000\000\000\000\093\000\
\094\000\082\000\083\000\084\000\085\000\086\000\087\000\088\000\
\089\000\090\000\091\000\000\000\000\000\000\000\000\000\093\000\
\094\000\082\000\083\000\084\000\085\000\086\000\087\000\088\000\
\089\000\090\000\091\000\000\000\000\000\000\000\000\000\093\000\
\082\000\083\000\084\000\085\000\086\000\087\000\088\000\089\000\
\090\000\091\000"

let yycheck = "\025\000\
\003\001\041\000\096\000\097\000\036\000\000\001\038\000\039\000\
\010\001\009\001\003\001\037\000\005\001\006\001\001\000\041\000\
\011\001\000\001\013\001\012\001\000\001\053\000\000\001\040\001\
\041\001\025\001\058\000\059\000\060\000\024\001\013\001\011\001\
\027\001\028\001\029\001\013\001\130\000\032\001\040\001\041\001\
\043\001\036\001\039\001\003\001\027\001\042\001\024\001\027\001\
\039\001\027\001\082\000\083\000\084\000\085\000\086\000\087\000\
\088\000\089\000\090\000\091\000\003\001\093\000\094\000\003\001\
\001\001\002\001\003\001\011\001\027\001\009\001\096\000\097\000\
\104\000\010\001\106\000\012\001\000\001\023\000\000\001\025\000\
\120\000\121\000\038\001\027\001\021\001\025\001\009\001\011\001\
\133\000\011\001\135\000\024\001\025\001\030\001\120\000\121\000\
\136\000\026\001\000\001\045\000\046\000\026\001\025\001\027\001\
\130\000\027\001\043\001\022\001\023\001\011\001\136\000\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\003\001\000\001\027\001\028\001\029\001\
\048\000\049\000\032\001\010\001\003\001\003\001\036\001\011\001\
\024\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\000\001\025\001\024\001\000\001\000\001\027\001\
\028\001\029\001\003\001\024\001\032\001\003\001\024\001\013\001\
\036\001\011\001\013\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\000\001\027\001\024\001\014\001\
\027\001\027\001\028\001\029\001\000\001\024\001\032\001\011\001\
\003\001\013\001\036\001\000\000\007\001\008\001\038\001\011\001\
\000\001\024\001\032\000\007\000\024\001\018\000\000\001\027\001\
\028\001\029\001\132\000\011\001\032\001\013\001\000\001\027\001\
\036\001\011\001\255\255\013\001\031\001\255\255\033\001\034\001\
\024\001\011\001\000\001\027\001\028\001\029\001\024\001\255\255\
\032\001\027\001\028\001\029\001\036\001\011\001\032\001\013\001\
\003\001\027\001\036\001\255\255\007\001\008\001\000\001\255\255\
\255\255\255\255\024\001\255\255\000\001\027\001\028\001\029\001\
\255\255\011\001\032\001\013\001\255\255\255\255\036\001\011\001\
\255\255\013\001\255\255\255\255\031\001\255\255\024\001\034\001\
\000\001\027\001\028\001\029\001\024\001\255\255\032\001\027\001\
\028\001\029\001\036\001\011\001\032\001\013\001\003\001\255\255\
\036\001\255\255\007\001\008\001\000\001\255\255\255\255\255\255\
\024\001\255\255\013\001\027\001\028\001\029\001\003\001\011\001\
\032\001\013\001\007\001\008\001\036\001\024\001\255\255\013\001\
\027\001\255\255\031\001\255\255\024\001\034\001\035\001\027\001\
\037\001\029\001\024\001\255\255\032\001\027\001\255\255\255\255\
\036\001\255\255\031\001\255\255\255\255\034\001\255\255\003\001\
\037\001\005\001\006\001\007\001\008\001\255\255\255\255\011\001\
\012\001\003\001\255\255\005\001\006\001\007\001\008\001\255\255\
\255\255\003\001\012\001\005\001\006\001\007\001\008\001\027\001\
\255\255\255\255\012\001\031\001\255\255\255\255\034\001\255\255\
\255\255\027\001\001\001\002\001\003\001\031\001\255\255\255\255\
\034\001\255\255\255\255\010\001\255\255\031\001\255\255\003\001\
\034\001\005\001\006\001\007\001\008\001\255\255\021\001\003\001\
\012\001\005\001\006\001\007\001\008\001\255\255\255\255\030\001\
\012\001\001\001\002\001\003\001\004\001\020\001\021\001\022\001\
\023\001\255\255\010\001\031\001\000\001\255\255\034\001\003\001\
\255\255\005\001\006\001\031\001\255\255\021\001\034\001\255\255\
\012\001\255\255\255\255\255\255\255\255\255\255\030\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\255\255\255\255\255\255\255\255\028\001\029\001\255\255\
\255\255\255\255\255\255\255\255\255\255\036\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\255\255\255\255\255\255\255\255\028\001\029\001\011\001\255\255\
\032\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\255\255\255\255\255\255\255\255\028\001\
\029\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\255\255\255\255\255\255\028\001\
\029\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\255\255\255\255\255\255\255\255\028\001\
\029\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\255\255\255\255\255\255\255\255\028\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001"

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
# 57 "bean_parse.mly"
                                  ( { typedefs = List.rev _1; procs = List.rev _2 } )
# 413 "bean_parse.ml"
               : Bean_ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typedefs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typedef) in
    Obj.repr(
# 61 "bean_parse.mly"
                                  ( _2 :: _1 )
# 421 "bean_parse.ml"
               : 'typedefs))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "bean_parse.mly"
                                  ( [] )
# 427 "bean_parse.ml"
               : 'typedefs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typespec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "bean_parse.mly"
                                  ( (_2, _3) )
# 435 "bean_parse.ml"
               : 'typedef))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "bean_parse.mly"
                                  ( Bool )
# 441 "bean_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "bean_parse.mly"
                                  ( Int )
# 447 "bean_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fielddefs) in
    Obj.repr(
# 71 "bean_parse.mly"
                                  ( Flist (List.rev _2) )
# 454 "bean_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 72 "bean_parse.mly"
                                  ( Id _1 )
# 461 "bean_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typespec) in
    Obj.repr(
# 76 "bean_parse.mly"
                                  ( (_1, _3) )
# 469 "bean_parse.ml"
               : 'fielddef))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typespec) in
    Obj.repr(
# 78 "bean_parse.mly"
                                  ( parse_error "not a valid identifier." )
# 476 "bean_parse.ml"
               : 'fielddef))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 79 "bean_parse.mly"
                                  ( parse_error "not a valid typespec.")
# 483 "bean_parse.ml"
               : 'fielddef))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fielddefs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fielddef) in
    Obj.repr(
# 82 "bean_parse.mly"
                                  ( _3 :: _1 )
# 491 "bean_parse.ml"
               : 'fielddefs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fielddef) in
    Obj.repr(
# 83 "bean_parse.mly"
                                  ( [_1] )
# 498 "bean_parse.ml"
               : 'fielddefs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fielddefs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fielddef) in
    Obj.repr(
# 85 "bean_parse.mly"
                                  ( parse_error "should be ',' separated." )
# 506 "bean_parse.ml"
               : 'fielddefs))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "bean_parse.mly"
                                  ( parse_error "must not be empty.")
# 512 "bean_parse.ml"
               : 'fielddefs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'procs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'proc) in
    Obj.repr(
# 90 "bean_parse.mly"
                                  ( _2 :: _1 )
# 520 "bean_parse.ml"
               : 'procs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proc) in
    Obj.repr(
# 91 "bean_parse.mly"
                                  ( [_1] )
# 527 "bean_parse.ml"
               : 'procs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'procheader) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'procbody) in
    Obj.repr(
# 94 "bean_parse.mly"
                                  ( (_2, _3) )
# 535 "bean_parse.ml"
               : 'proc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'params) in
    Obj.repr(
# 98 "bean_parse.mly"
                                  ( { procname = _1; parameters = (List.rev _3) } )
# 543 "bean_parse.ml"
               : 'procheader))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'params) in
    Obj.repr(
# 100 "bean_parse.mly"
                                  ( parse_error "expected ')'" )
# 551 "bean_parse.ml"
               : 'procheader))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'params) in
    Obj.repr(
# 101 "bean_parse.mly"
                                  ( parse_error "expected '('" )
# 559 "bean_parse.ml"
               : 'procheader))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'params) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 105 "bean_parse.mly"
                                  ( _3 :: _1 )
# 567 "bean_parse.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 106 "bean_parse.mly"
                                  ( [_1] )
# 574 "bean_parse.ml"
               : 'params))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "bean_parse.mly"
                                  ( [] )
# 580 "bean_parse.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'passspec) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typespec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 110 "bean_parse.mly"
                                  ( (_1, _2, _3) )
# 589 "bean_parse.ml"
               : 'param))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "bean_parse.mly"
                                  ( Val )
# 595 "bean_parse.ml"
               : 'passspec))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "bean_parse.mly"
                                  ( Ref )
# 601 "bean_parse.ml"
               : 'passspec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vardecls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 119 "bean_parse.mly"
                                  ( { vardecls = List.rev _1; stmts = List.rev _2 } )
# 609 "bean_parse.ml"
               : 'procbody))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vardecls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vardecl) in
    Obj.repr(
# 122 "bean_parse.mly"
                                  ( _2 :: _1 )
# 617 "bean_parse.ml"
               : 'vardecls))
; (fun __caml_parser_env ->
    Obj.repr(
# 123 "bean_parse.mly"
                                  ( [] )
# 623 "bean_parse.ml"
               : 'vardecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typespec) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 126 "bean_parse.mly"
                                  ( (_1, _2) )
# 631 "bean_parse.ml"
               : 'vardecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 129 "bean_parse.mly"
                                  ( _2 :: _1 )
# 639 "bean_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 130 "bean_parse.mly"
                                  ( [_1] )
# 646 "bean_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'rvalue) in
    Obj.repr(
# 134 "bean_parse.mly"
                                  ( Assign (_1, _3) )
# 654 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'lvalue) in
    Obj.repr(
# 135 "bean_parse.mly"
                                  ( Read _2 )
# 661 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 136 "bean_parse.mly"
                                  ( Write _2 )
# 668 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 137 "bean_parse.mly"
                                  ( WriteS _2 )
# 675 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'exprs) in
    Obj.repr(
# 139 "bean_parse.mly"
                                  ( Call (_1, List.rev _3) )
# 683 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 140 "bean_parse.mly"
                                  ( IfThen (_2, List.rev _4) )
# 691 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'stmts) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 142 "bean_parse.mly"
                                  ( IfThenElse (_2, List.rev _4, List.rev _6) )
# 700 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 143 "bean_parse.mly"
                                  ( While (_2, List.rev _4) )
# 708 "bean_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 147 "bean_parse.mly"
                                  ( Rexpr _1 )
# 715 "bean_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fieldinits) in
    Obj.repr(
# 148 "bean_parse.mly"
                                  ( Rstruct (List.rev _2) )
# 722 "bean_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    Obj.repr(
# 150 "bean_parse.mly"
                                  ( parse_error "unknown token when expecting an expression." )
# 728 "bean_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    Obj.repr(
# 151 "bean_parse.mly"
                                  ( parse_error "do not accept empty right hand side." )
# 734 "bean_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fieldinits) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fieldinit) in
    Obj.repr(
# 155 "bean_parse.mly"
                                  ( _3 :: _1 )
# 742 "bean_parse.ml"
               : 'fieldinits))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fieldinit) in
    Obj.repr(
# 156 "bean_parse.mly"
                                  ( [_1] )
# 749 "bean_parse.ml"
               : 'fieldinits))
; (fun __caml_parser_env ->
    Obj.repr(
# 157 "bean_parse.mly"
                                  ( [] )
# 755 "bean_parse.ml"
               : 'fieldinits))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fieldinits) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fieldinit) in
    Obj.repr(
# 159 "bean_parse.mly"
                                  ( parse_error "should be ',' separated." )
# 763 "bean_parse.ml"
               : 'fieldinits))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 162 "bean_parse.mly"
                                  ( (_1, _3) )
# 771 "bean_parse.ml"
               : 'fieldinit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 166 "bean_parse.mly"
                                  ( LId _1 )
# 778 "bean_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 167 "bean_parse.mly"
                                  ( LField (_1, _3) )
# 786 "bean_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exprs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 171 "bean_parse.mly"
                                  ( _3 :: _1 )
# 794 "bean_parse.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 172 "bean_parse.mly"
                                  ( [_1] )
# 801 "bean_parse.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    Obj.repr(
# 173 "bean_parse.mly"
                                  ( [] )
# 807 "bean_parse.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exprs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 175 "bean_parse.mly"
                                  ( parse_error "expressions should be ',' separated." )
# 815 "bean_parse.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 178 "bean_parse.mly"
                                  ( Ebool _1 )
# 822 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 179 "bean_parse.mly"
                                  ( Eint _1 )
# 829 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 180 "bean_parse.mly"
                                  ( Elval _1 )
# 836 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 181 "bean_parse.mly"
                                  ( _2 )
# 843 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 182 "bean_parse.mly"
                                  ( Ebinop (_1, Op_or, _3) )
# 851 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 183 "bean_parse.mly"
                                  ( Ebinop (_1, Op_and, _3) )
# 859 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 184 "bean_parse.mly"
                                  ( Ebinop (_1, Op_eq, _3) )
# 867 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 185 "bean_parse.mly"
                                  ( Ebinop (_1, Op_neq, _3) )
# 875 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 186 "bean_parse.mly"
                                  ( Ebinop (_1, Op_lt, _3) )
# 883 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 187 "bean_parse.mly"
                                  ( Ebinop (_1, Op_lte, _3) )
# 891 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 188 "bean_parse.mly"
                                  ( Ebinop (_1, Op_gt, _3) )
# 899 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 189 "bean_parse.mly"
                                  ( Ebinop (_1, Op_gte, _3) )
# 907 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 190 "bean_parse.mly"
                                  ( Ebinop (_1, Op_add, _3) )
# 915 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 191 "bean_parse.mly"
                                  ( Ebinop (_1, Op_sub, _3) )
# 923 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 192 "bean_parse.mly"
                                  ( Ebinop (_1, Op_mul, _3) )
# 931 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 193 "bean_parse.mly"
                                  ( Ebinop (_1, Op_div, _3) )
# 939 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 194 "bean_parse.mly"
                                  ( Eunop (Op_not, _2) )
# 946 "bean_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 195 "bean_parse.mly"
                                  ( Eunop (Op_minus, _2))
# 953 "bean_parse.ml"
               : 'expr))
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
