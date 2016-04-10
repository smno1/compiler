(* Specification of an AST for bean *)
type ident = string

type binop =
  | Op_add  | Op_sub  | Op_mul  | Op_div  | Op_eq 
  | Op_neq  | Op_lt   | Op_gt   | Op_lte  | Op_gte 
  | Op_and  | Op_or

type unop =
  | Op_minus
  | Op_not

type lvalue =
  | LId of ident
  | LField of (lvalue * ident)

type expr =
  | Ebool of bool
  | Eint of int
  | Elval of lvalue
  | Ebinop of (expr * binop * expr)
  | Eunop of (unop * expr)
  | Pebinop of (expr * binop * expr)

type rvalue =
  | Rexpr of expr
  | Rstruct of structinit
and structinit =
  fieldinit list
and fieldinit = 
  (ident * rvalue)
(* 
type fieldinit =
  FEQ of (ident * rvalue)

type structinit =
  fieldinit list *)

type passspec = 
  | Val
  | Ref

type typespec = 
  | Bool
  | Int
  | Flist of (fielddef list)
  | Id of ident
and fielddef = 
  (ident * typespec)

type stmt = 
  | Assign of (lvalue * rvalue)
  | Read of lvalue
  | Write of expr
  | WriteS of string
  | Call of (ident * expr list)
  | IfThen of (expr * stmt list)
  | IfThenElse of (expr * stmt list * stmt list)
  | While of (expr * stmt list)

type vardecl = 
  (typespec * ident)

type parameter =
  (passspec * typespec * ident)

type procbody = {
  vardecls : vardecl list ;
  stmts : stmt list
}

type procheader = {
  procname : ident ;
  parameters : parameter list;
}

type proc =
  (procheader * procbody)

type typedef =
  (typespec * ident)

type program = {
  typedefs : typedef list ;
  procs : proc list
}
