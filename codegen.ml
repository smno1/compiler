open Bean_ast
open Format
open Symbol
open Type_checking

let labelnum = ref 0
let regparam = ref 0

let cg_bool fmt b reg = 
    if b then
        fprintf fmt "int_const r%d, 1\n" reg
    else
        fprintf fmt "int_const r%d, 0\n" reg

let cg_int fmt i reg = 
    fprintf fmt "int_const r%d, %d\n" reg i

let rec get_lvalue_list lvalue namelist = 
    match lvalue with
          LId(ident) -> ident :: namelist
        | LField((sublvalue,ident)) -> let newnamelist = ident :: namelist in
                                       get_lvalue_list sublvalue newnamelist

let cg_lvalue_load fmt lvalue scope reg = 
    let lsymbol = String.concat "." (get_lvalue_list lvalue []) in
    let lsymbolrecord = (Symbol.find_symbol lsymbol scope) in
    let lsymbolslot = lsymbolrecord.slot in
    if lsymbolrecord.pass_by_ref then
        (fprintf fmt "load r%d, %d\n" reg lsymbolslot;
         fprintf fmt "load_indirect r%d, r%d\n" reg reg)
    else
        (fprintf fmt "load r%d, %d\n" reg lsymbolslot)

let cg_lvalue_store fmt lvalue scope = 
    let lsymbol = String.concat "." (get_lvalue_list lvalue []) in
    let lsymbolrecord = (Symbol.find_symbol lsymbol scope) in
    let lsymbolslot = lsymbolrecord.slot in
    if lsymbolrecord.pass_by_ref then
        (fprintf fmt "load r1, %d\n" lsymbolslot;
         fprintf fmt "store_indirect r1, r0\n")
    else
        (fprintf fmt "store %d, r0\n" lsymbolslot)

let cg_uminus fmt (unop, expr) reg = 
    let reg1 = reg + 1 in
    fprintf fmt "int_const r%d, 0\n" reg1;
    fprintf fmt "sub_int r%d, r%d, r%d\n" reg reg1 reg

let rec cg_expr fmt expr scope reg = 
    match expr with
          Ebool(b) -> cg_bool fmt b reg
        | Eint(i)-> cg_int fmt i reg
        | Elval(lva)-> cg_lvalue_load fmt lva scope reg
        | Ebinop(binop)-> cg_binop fmt binop scope reg
        | Eunop(unop)-> cg_unop fmt unop scope reg
and cg_binop fmt (expr1, binop, expr2) scope reg =
    let reg1 = reg + 1 in
    cg_expr fmt expr1 scope reg;
    cg_expr fmt expr2 scope reg1;
    match binop with
          Op_add-> fprintf fmt "add_int r%d, r%d, r%d\n" reg reg reg1
        | Op_sub-> fprintf fmt "sub_int r%d, r%d, r%d\n" reg reg reg1
        | Op_mul-> fprintf fmt "mul_int r%d, r%d, r%d\n" reg reg reg1
        | Op_div-> fprintf fmt "div_int r%d, r%d, r%d\n" reg reg reg1
        | Op_eq-> fprintf fmt "cmp_eq_int r%d, r%d, r%d\n" reg reg reg1
        | Op_neq-> fprintf fmt "cmp_ne_int r%d, r%d, r%d\n" reg reg reg1
        | Op_lt-> fprintf fmt "cmp_lt_int r%d, r%d, r%d\n" reg reg reg1
        | Op_gt-> fprintf fmt "cmp_gt_int r%d, r%d, r%d\n" reg reg reg1
        | Op_lte-> fprintf fmt "cmp_le_int r%d, r%d, r%d\n" reg reg reg1
        | Op_gte-> fprintf fmt "cmp_ge_int r%d, r%d, r%d\n" reg reg reg1
        | Op_and-> fprintf fmt "and r%d, r%d, r%d\n" reg reg reg1
        | Op_or-> fprintf fmt "or r%d, r%d, r%d\n" reg reg reg1
and cg_unop fmt (unop, expr) scope reg = 
    cg_expr fmt expr scope reg;
    match unop with
          Op_minus-> cg_uminus fmt (unop, expr) reg
        | Op_not -> fprintf fmt "not r%d, r%d\n" reg reg

let cg_assign_atom fmt latom ridlist lbase rbase mode = 
        let llen = String.length latom.identifier in
        let ratom = List.filter
            (fun x -> 
                let rlen = String.length x.identifier in
                let left = String.sub latom.identifier lbase (llen-lbase) in
                let right = String.sub x.identifier rbase (rlen-rbase) in
                left = right;
            ) ridlist in
        match mode with
            | 0 -> fprintf fmt "load r0, %d\n" (List.hd ratom).slot;
                   fprintf fmt "store %d, r0\n" latom.slot
            | 1 -> fprintf fmt "load r%d, %d\n" !regparam (List.hd ratom).slot;
                   regparam := !regparam + 1
            | 2 -> fprintf fmt "load_address r%d, %d\n" !regparam (List.hd ratom).slot;
                   regparam := !regparam + 1
            | 3 -> fprintf fmt "load r0, %d\n" (List.hd ratom).slot;
                   fprintf fmt "load_indirect r0, r0\n";
                   fprintf fmt "load r1, %d\n" latom.slot;
                   fprintf fmt "store_indirect r1, r0\n"
            | 4 -> fprintf fmt "load r0, %d\n" (List.hd ratom).slot;
                   fprintf fmt "load r1, %d\n" latom.slot;
                   fprintf fmt "store_indirect r1, r0\n"
            | 5 -> fprintf fmt "load r0, %d\n" (List.hd ratom).slot;
                   fprintf fmt "load_indirect r0, r0\n";
                   fprintf fmt "store %d, r0\n" latom.slot
            | _ -> ()

let cg_assign_type fmt lid rid scope mode = 
    let lidlist = Symbol.get_leaf_symbol_by_super_symbol lid scope in
    let ridlist = Symbol.get_leaf_symbol_by_super_symbol rid scope in
    let lbase = String.length lid in
    let rbase = String.length rid in
    List.iter (fun x -> cg_assign_atom fmt x ridlist lbase rbase mode) lidlist

let rec cg_assign fmt llist rvalue scope = 
    let lsymbol = String.concat "." llist in
    let lsymbolrecord = Symbol.find_symbol lsymbol scope in
    let lsymbolslot = lsymbolrecord.slot in
    if lsymbolslot = -1 then
        (match rvalue with
            Rexpr(expr) -> 
                (match expr with
                    |Elval(rlvalue) ->
                        let rsymbol = String.concat "." (get_lvalue_list rlvalue []) in
                        let rsymbolrecord = Symbol.find_symbol rsymbol scope in
                        (match (lsymbolrecord.pass_by_ref, rsymbolrecord.pass_by_ref) with
                            | (true,true) -> cg_assign_type fmt lsymbol rsymbol scope 3
                            | (true,false) -> cg_assign_type fmt lsymbol rsymbol scope 4
                            | (false,true) -> cg_assign_type fmt lsymbol rsymbol scope 5
                            | (false,false) -> cg_assign_type fmt lsymbol rsymbol scope 0)
                    | _ -> ())
            | Rstruct(structinit) -> 
                List.iter (fun x ->
                            let (ident,subrvalue) = x in
                            let newlist = llist@[ident] in
                            cg_assign fmt newlist subrvalue scope) structinit)
    else
        (match rvalue with
            | Rexpr(expr) -> cg_expr fmt expr scope 0;
                             if lsymbolrecord.pass_by_ref then
                                (fprintf fmt "load r1, %d\n" lsymbolslot;
                                 fprintf fmt "store_indirect r1, r0\n")
                             else
                                (fprintf fmt "store %d, r0\n" lsymbolslot)
            | _ -> ())

let cg_read fmt lvalue scope = 
    let lvaluetype = Type_checking.match_lvalue scope lvalue in
    fprintf fmt "# read\n";
    match lvaluetype with
        | "bool" -> fprintf fmt "call_builtin read_bool\n";
        | "int" -> fprintf fmt "call_builtin read_int\n";
        | _ -> ();
    cg_lvalue_store fmt lvalue scope

let cg_write fmt wexpr scope = 
    fprintf fmt "# write\n";
    cg_expr fmt wexpr scope 0;
    let exprtype = Type_checking.match_expr scope wexpr in
    match exprtype with
        | "bool" -> fprintf fmt "call_builtin print_bool\n"
        | "int" -> fprintf fmt "call_builtin print_int\n"
        | _ -> ()

let cg_writes fmt ws = 
    fprintf fmt "# write\n";
    fprintf fmt "string_const r0, %s\n" ws;
    fprintf fmt "call_builtin print_string\n"

let cg_parameter_load fmt procname expr scope = 
    match expr with
        | Elval(lva) -> let rsymbol = String.concat "." (get_lvalue_list lva []) in
                        let rslot = (Symbol.find_symbol rsymbol scope).slot in
                        let lrecord = (Symbol.find_symbol_by_slot !regparam procname) in
                        if rslot = -1 then
                            (let lident = lrecord.identifier in
                             let llen = String.index lident '.' in
                             let lsymbol = String.sub lident 0 llen in
                             if lrecord.pass_by_ref then
                                (cg_assign_type fmt lsymbol rsymbol scope 2)
                             else
                                (cg_assign_type fmt lsymbol rsymbol scope 1)
                             )
                        else
                            if lrecord.pass_by_ref then
                                (fprintf fmt "load_address r%d, %d\n" !regparam rslot;
                                regparam := !regparam + 1)
                            else
                                (fprintf fmt "load r%d, %d\n" !regparam rslot;
                                regparam := !regparam + 1)
        | _ -> cg_expr fmt expr scope !regparam;
               regparam := !regparam + 1


let cg_call fmt (procname, exprs) scope = 
    fprintf fmt "# call\n";
    regparam := 0;
    List.iter (fun x -> cg_parameter_load fmt procname x scope) exprs;
    fprintf fmt "call proc_%s\n" procname

let rec cg_stmt fmt stmt scope= 
    match stmt with
        | Assign((lvalue,rvalue)) ->
            fprintf fmt "# assignment\n";
            let llist = get_lvalue_list lvalue [] in
            cg_assign fmt llist rvalue scope
        | Read(rlv) -> cg_read fmt rlv scope
        | Write(wexpr) -> cg_write fmt wexpr scope
        | WriteS(ws) -> cg_writes fmt ws
        | Call(c)-> cg_call fmt c scope
        | IfThen(ifth)-> cg_if_then fmt ifth scope
        | IfThenElse(ifte)-> cg_if_then_else fmt ifte scope
        | While(w)-> cg_while fmt w scope
and cg_if_then fmt (expr, stmts) scope = 
    labelnum := !labelnum + 1;
    let curlabelnum = !labelnum in
    fprintf fmt "# if\n";
    cg_expr fmt expr scope 0;
    fprintf fmt "branch_on_false r0, label%d\n" curlabelnum;
    List.iter (fun x -> (cg_stmt fmt) x scope) stmts;
    fprintf fmt "label%d:\n" curlabelnum
and cg_if_then_else fmt (expr, stmts1, stmts2) scope = 
    labelnum := !labelnum + 1;
    let curlabelnum = !labelnum in
    fprintf fmt "# if\n";
    cg_expr fmt expr scope 0;
    fprintf fmt "branch_on_false r0, label%delse\n" curlabelnum;
    List.iter (fun x -> (cg_stmt fmt) x scope) stmts1;
    fprintf fmt "branch_uncond label%dafter\n" curlabelnum;
    fprintf fmt "label%delse:\n" curlabelnum;
    List.iter (fun x -> (cg_stmt fmt) x scope) stmts2;
    fprintf fmt "label%dafter:\n" curlabelnum

and cg_while fmt (expr, stmts) scope = 
    labelnum := !labelnum + 1;
    let curlabelnum = !labelnum in
    fprintf fmt "# while\n";
    fprintf fmt "label%dcond:\n" curlabelnum;
    cg_expr fmt expr scope 0;
    fprintf fmt "branch_on_false r0, label%dafter\n" curlabelnum;
    List.iter (fun x -> (cg_stmt fmt) x scope) stmts;
    fprintf fmt "branch_uncond label%dcond\n" curlabelnum;
    fprintf fmt "label%dafter:\n" curlabelnum

let cg_vardecl fmt (typespec, ident) scope = 
    match typespec with
        | Bool -> let slotnum = (Symbol.find_symbol ident scope).slot in
                  fprintf fmt "store %d, r0\n" slotnum
        | Int -> let slotnum = (Symbol.find_symbol ident scope).slot in
                 fprintf fmt "store %d, r0\n" slotnum
        | Flist(fields) -> let symlist = Symbol.get_leaf_symbol_by_super_symbol ident scope in
                           List.iter (fun x -> fprintf fmt "store %d, r0\n" x.slot) symlist
        | Id(id) -> let symlist = Symbol.get_leaf_symbol_by_super_symbol ident scope in
                    List.iter (fun x -> fprintf fmt "store %d, r0\n" x.slot) symlist

let cg_procbody fmt procbody scope = 
    if (List.length procbody.vardecls) > 0 then (
        fprintf fmt "# variable declarations\n";
        fprintf fmt "int_const r0, 0\n";
        List.iter (fun x -> (cg_vardecl fmt) x scope) procbody.vardecls;
    );
    List.iter (fun x -> (cg_stmt fmt) x scope) procbody.stmts

let cg_parameter fmt (passspec,typespec,ident) scope = 
    match typespec with
        | Bool -> let slotnum = (Symbol.find_symbol ident scope).slot in
                  fprintf fmt "store %d, r%d\n" slotnum !regparam;
                  regparam := !regparam + 1
        | Int -> let slotnum = (Symbol.find_symbol ident scope).slot in
                  fprintf fmt "store %d, r%d\n" slotnum !regparam;
                  regparam := !regparam + 1
        | Flist(fields)-> let symlist = Symbol.get_leaf_symbol_by_super_symbol ident scope in
                          List.iter (fun x -> fprintf fmt "store %d, r%d\n" x.slot !regparam;
                                              regparam := !regparam + 1) symlist
        | Id(id) -> let symlist = Symbol.get_leaf_symbol_by_super_symbol ident scope in
                    List.iter (fun x -> fprintf fmt "store %d, r%d\n" x.slot !regparam;
                                        regparam := !regparam + 1) symlist

let cg_proc fmt (procheader,procbody) = 
    let framesize = (Symbol.find_proc procheader.procname).proc_size in
    fprintf fmt "proc_%s:\n" procheader.procname;
    fprintf fmt "# prologue\n";
    fprintf fmt "push_stack_frame %d\n" framesize;
    if (List.length procheader.parameters) > 0 then (
        regparam := 0;
        List.iter (fun x -> (cg_parameter fmt) x procheader.procname) procheader.parameters;
    );
    cg_procbody fmt procbody procheader.procname;
    fprintf fmt "# epilogue\n";
    fprintf fmt "pop_stack_frame %d\n" framesize;
    fprintf fmt "return\n"

let generate_program fmt program =
    fprintf fmt "call proc_main\n";
    fprintf fmt "halt\n";
    List.iter (cg_proc fmt) program.procs


