(* -------------------------------------------------------- *)
(* Specification of the pretty printer for bean language.   *)
(* This file defines the format and details for showing the *)
(* code. Especially, it defines the rules for removing      *)
(* redundant parens                                         *)
(* -------------------------------------------------------- *)
open Bean_ast
open Format

let pp_int fmt x=fprintf fmt "%d" x
let pp_string fmt x = fprintf fmt "%s" x
let pp_bool fmt x = fprintf fmt "%b" x

let rec pp_typespec fmt typespec=
    match typespec with
        Bool-> fprintf fmt "bool"
      | Int -> fprintf fmt "int"
      | Flist(flst)-> pp_fielddef_lst fmt flst
      | Id(id)-> pp_string fmt id

and pp_fielddef fmt first (id,typespec)=
    if not first then
        fprintf fmt ", ";
    fprintf fmt "%s : " id;
    pp_typespec fmt typespec

and pp_fielddef_lst fmt (first::flst)=
    fprintf fmt "{";
    pp_fielddef fmt true first;
    List.map (pp_fielddef fmt false) flst;
    fprintf fmt "}"

let pp_typedef fmt (typespec,ident)=
    fprintf fmt "typedef ";
    pp_typespec fmt typespec;
    fprintf fmt " %s@," ident


let pp_vardecl fmt (typespec,ident)=
    pp_typespec fmt typespec;
    fprintf fmt " %s;@," ident

let rec pp_lvalue fmt lvalue=
    match lvalue with
        LId(id)->pp_string fmt id
      | LField(lfield)-> pp_lfield fmt lfield
and pp_lfield fmt (lvalue,id)=
    pp_lvalue fmt lvalue;
    fprintf fmt ".%s" id

let precedence = function
    | Ebinop(_,Op_mul,_)->6
    | Ebinop(_,Op_div,_)->6
    | Ebinop(_,Op_add,_)->5
    | Ebinop(_,Op_sub,_)->5
    | Ebinop(_,Op_lt,_)->4
    | Ebinop(_,Op_lte,_)->4
    | Ebinop(_,Op_gte,_)->4
    | Ebinop(_,Op_gt,_)->4
    | Ebinop(_,Op_neq,_)->4
    | Ebinop(_,Op_eq,_)->4
    | Ebinop(_,Op_and,_)->2
    | Ebinop(_,Op_or,_)->1
    | Eunop(Op_minus,_)->7
    | Eunop(Op_not,_)->3
    | _->0

let precedence_op = function
    | Op_mul->6
    | Op_div->6
    | Op_add->5
    | Op_sub->5
    | Op_lte->4
    | Op_lt->4
    | Op_gte->4
    | Op_gt->4
    | Op_neq->4
    | Op_eq->4
    | Op_and->2
    | Op_or->1
    | _ -> 0

let precedence_uop = function
    | Op_minus->7
    | Op_not->3
    | _ -> 0

let rec pp_expr fmt first expr=
    if not first then 
        fprintf fmt ", ";
    match expr with
        Ebool(b)-> pp_bool fmt b
      | Eint(i)->pp_int fmt i
      | Elval(lva)-> pp_lvalue fmt lva
      | Ebinop(binop)->pp_binop fmt binop
      | Eunop(unop)->pp_unop fmt unop
and pp_binop fmt (expr1, binop, expr2)=
    let lpre=precedence expr1 in
    (if lpre < precedence_op binop && lpre <>0 then
        match expr1 with
          | Ebinop(pbinop) -> pp_pbinop fmt pbinop
          | Eunop(punop) -> pp_punop fmt punop
          | _ ->()
    else
        pp_expr fmt true expr1);

    (match binop with
        Op_add-> fprintf fmt " + "
      | Op_sub-> fprintf fmt " - "
      | Op_mul-> fprintf fmt " * "
      | Op_div-> fprintf fmt " / "
      | Op_eq-> fprintf fmt " = "
      | Op_neq-> fprintf fmt " != "
      | Op_lt-> fprintf fmt " < "
      | Op_gt-> fprintf fmt " > "
      | Op_lte-> fprintf fmt " <= "
      | Op_gte-> fprintf fmt " >= "
      | Op_and-> fprintf fmt " and "
      | Op_or-> fprintf fmt " or " );
    let rpre=precedence expr2 in
    if rpre <= precedence_op binop && rpre <>0 then
        match expr2 with
          | Ebinop(pbinop) -> pp_pbinop fmt pbinop
          | _ ->()
    else
        pp_expr fmt true expr2
and pp_unop fmt (unop, expr)=
    (match unop with
        Op_minus-> fprintf fmt "-"
      | Op_not  -> fprintf fmt "not "
        );
    match expr with
        Eint(i)->pp_int fmt i
      | Elval(lva)->pp_lvalue fmt lva
      | Ebool(b)-> pp_bool fmt b
      | Eunop(v)->pp_unop fmt v
      | _ ->fprintf fmt "(";
            pp_expr fmt true expr;
            fprintf fmt ")"
and pp_pbinop fmt pbinop=
    fprintf fmt "(";
    pp_binop fmt pbinop;
    fprintf fmt ")"    
and pp_punop fmt punop=
    fprintf fmt "(";
    pp_unop fmt punop;
    fprintf fmt ")"


let rec pp_rvalue fmt rvalue=
    match rvalue with
        Rexpr(expr)-> pp_expr fmt true expr
      | Rstruct(rs)-> pp_struct fmt rs
and pp_struct fmt (first::fieldinitlst)=
    fprintf fmt "{";
    pp_fieldinit fmt true first;
    List.map (pp_fieldinit fmt false) fieldinitlst;
    fprintf fmt "}"
and pp_fieldinit fmt first (id,rvalue)=
    if not first then 
        fprintf fmt ", ";
    fprintf fmt "%s = " id;
    pp_rvalue fmt rvalue

let pp_assign fmt (lvalue, rvalue)=
    pp_lvalue fmt lvalue;
    fprintf fmt " := ";
    pp_rvalue fmt rvalue;
    fprintf fmt ";"

let pp_read fmt lvalue=
    fprintf fmt "read ";
    pp_lvalue fmt lvalue;
    fprintf fmt ";"

let pp_write fmt expr=
    fprintf fmt "write ";
    pp_expr fmt true expr;
    fprintf fmt ";"

let pp_call fmt (id,exprlst)=
    pp_string fmt id;
    fprintf fmt "(";
    if (List.length exprlst) > 0 then (
        pp_expr fmt true (List.hd exprlst);
        List.map (pp_expr fmt false) (List.tl exprlst);
        ());
    fprintf fmt ");"

let rec pp_stmt fmt first stmt=
    if not first then 
        fprintf fmt "@,";
    match stmt with
        Assign(ass) -> pp_assign fmt ass
      | Read(rlv) -> pp_read fmt rlv
      | Write(wexpr)-> pp_write fmt wexpr
      | WriteS(ws)-> fprintf fmt "write %s;" ws
      | Call(c)-> pp_call fmt c
      | IfThen(ifth)-> pp_if_then fmt ifth
      | IfThen(ifth)-> pp_if_then fmt ifth
      | IfThenElse(ifte)->pp_if_then_else fmt ifte
      | While(w)-> pp_while fmt w
      (* | _ -> fprintf fmt "stmt" *)
and pp_if_then fmt (expr, (first::stmtlst))=
    fprintf fmt "if ";
    pp_expr fmt true expr;
    fprintf fmt " then@,";
    fprintf fmt "@[<v 4>    ";
    pp_stmt fmt true first;
    List.map (pp_stmt fmt false) stmtlst;
    fprintf fmt "@]@,fi"
and pp_if_then_else fmt (expr,(first1::stlst1),(first2::stlst2))=
    fprintf fmt "if ";
    pp_expr fmt true expr;
    fprintf fmt " then@,";
    fprintf fmt "@[<v 4>    ";
    pp_stmt fmt true first1;
    List.map (pp_stmt fmt false) stlst1;
    fprintf fmt "@]@,else@,";
    fprintf fmt "@[<v 4>    ";
    pp_stmt fmt true first2;
    List.map (pp_stmt fmt false) stlst2;
    fprintf fmt "@]@,fi"
and pp_while fmt (expr,(first::stmtlst))=
    fprintf fmt "while ";
    pp_expr fmt true expr;
    fprintf fmt " do@,";
    fprintf fmt "@[<v 4>    ";
    pp_stmt fmt true first;
    List.map (pp_stmt fmt false) stmtlst;
    fprintf fmt "@]@,od"

let pp_procbody fmt procbody =
    fprintf fmt "@[<v 4>    ";
    List.map (pp_vardecl fmt) procbody.vardecls;
    fprintf fmt "@,";
    if (List.length procbody.stmts) > 0 then (
        pp_stmt fmt true (List.hd procbody.stmts);
        List.map (pp_stmt fmt false) (List.tl procbody.stmts);
        ());
    fprintf fmt "@]@,"

let pp_parameter fmt first (passspec,typespec,ident)=
    if not first then 
        fprintf fmt ", ";
    (match passspec with
        Ref -> fprintf fmt "ref "
      | Val -> fprintf fmt "val ");
    pp_typespec fmt typespec;
    fprintf fmt " %s" ident

let pp_procheader fmt procheader =
    pp_string fmt procheader.procname;
    fprintf fmt "(";
    if (List.length procheader.parameters) > 0 then (
        pp_parameter fmt true (List.hd procheader.parameters);
        List.map (pp_parameter fmt false) (List.tl procheader.parameters);
        ());
    fprintf fmt ")"

let pp_proc fmt (procheader, procbody) =
    fprintf fmt "@,";
    fprintf fmt "proc ";
    pp_procheader fmt procheader;
    fprintf fmt "@,";
    pp_procbody fmt procbody;
    fprintf fmt "end@,"

let print_program fmt program = 
    fprintf fmt "@[<v>";
    List.map (pp_typedef fmt) program.typedefs;
    List.map (pp_proc fmt) program.procs;
    fprintf fmt "@]"
