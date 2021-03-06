(* The type checker runs through the AST, at each node check its
 * well-formedness. It uses the symbol table created in analyze
 * and if any semantic error found, it throws an exception back
 * to bean.ml, where it is catched. The codegen.ml will not be
 * executed.
 * 
 * @author: Kylin - Yumeng Shi *)

(* This file contains all the well-formedness checkers *)
open Symbol
open Bean_ast
open Printf
open String
open Str

(* ================================================ *)
(* =====      Utility Functions         =========== *)
(* ================================================ *)

(* check if a symbol is in the scope *)
let check_symbol symbol scope = 
    let symbol_name = symbol.identifier in
    if not (List.exists (fun x -> x.identifier = symbol_name && x.scope = scope) Symbol.symbol_table.symbol_list) then
        failwith "Error: this symbol is not defined."

let check_symbol_id symbol_name scope = 
    (* print_string "Checking id ... "; *)
    (* Printf.printf "%s" symbol_name; *)
    (* print_string "\n"; *)
    if not (List.exists (fun x -> x.identifier = symbol_name && x.scope = scope) Symbol.symbol_table.symbol_list) then
        failwith "Error: this symbol is not defined."
    else
        ()
        (* print_string "Success !!\n" *);;

let check_field field_name type_name =
    if not (List.exists (fun x -> x.fieldname = field_name && x.belong_type = type_name) Symbol.fielddef_table.fielddef_list) then
        failwith "Error: this field is not defined in the type."

let check_callid id = 
    if not (List.exists (fun x -> x.proc_name = id) proc_table.proc_list) then
        failwith "Error: this proc is not defined."

let cut = function
    | [] -> []
    | _::xs -> xs

let replace input output =
    Str.global_replace (Str.regexp_string input) output;;


(* ================================================ *)
(* ========      Type Functions         =========== *)
(* ================================================ *)

let binop_type = function
    | Op_add-> "int"
    | Op_sub-> "int"
    | Op_mul-> "int"
    | Op_div-> "int"
    | Op_eq-> "bool"
    | Op_neq-> "bool"
    | Op_lt-> "bool"
    | Op_gt-> "bool"
    | Op_lte-> "bool"
    | Op_gte-> "bool"
    | Op_and-> "bool" (* bool type only with bool operands*)
    | Op_or-> "bool"

let binop_op_type = function
    | Op_eq-> "both" (* bool type with both types *)
    | Op_neq-> "both"
    | Op_lt-> "bi" (* bool type only with int operands *)
    | Op_gt-> "bi"
    | Op_lte-> "bi"
    | Op_gte-> "bi"
    | Op_and-> "bb" (* bool type only with bool operands *)
    | Op_or-> "bb"
    | Op_add | Op_sub | Op_mul | Op_div -> "int"

let unop_type = function
    | Op_minus-> "int"
    | Op_not-> "bool"

(* get the real type of symbol *)
let rec match_symbol supproc id =
    check_symbol_id id supproc;
    let symbol = find_symbol id supproc in
    let symbol_type = symbol.sym_typespec in
    let real_type = look_up_origin_type symbol_type in
    real_type


(* calculate the type of lvalue *)
let rec match_lvalue supproc lvalue =
    let id_list = [] in
    match lvalue with
        | LId(id) -> match_symbol supproc id
        | LField(lfield) -> match_symbol supproc (String.concat "." (
                            get_id_lfield supproc lfield id_list))
        
and get_id_lvalue supproc lvalue idlst =
    match lvalue with
        | LId(id) -> idlst@[id]
        | LField(lfield) -> get_id_lfield supproc lfield idlst
and get_id_lfield supproc (lvalue, id) idlst = 
    let newlst = get_id_lvalue supproc lvalue idlst in
    newlst@[id]
and find_expr_name supproc expr = 
    match expr with
        | Elval(lva) -> String.concat "." (get_id_lvalue supproc lva [])
        | _ -> failwith "Incorrect"

(* return the lvalue id *)
and get_name_lvalue supproc lvalue =
    match lvalue with
        | LId(id) -> [id]
        | LField(lfield) -> get_id_lfield supproc lfield []

(* calculate the type of an expression *)
let rec match_expr supproc expr =
    match expr with
        | Ebool(b) -> "bool"
        | Eint(i) -> "int"
        | Elval(lva) -> match_lvalue supproc lva
        | Ebinop(binop) -> match_binop supproc binop
        | Eunop(unop) -> match_unop supproc unop

(* calculate the type of binop expr *)
and match_binop supproc (expr1, binop, expr2) =
    let expr1_type = match_expr supproc expr1 in
    let expr2_type = match_expr supproc expr2 in
    if ((expr1_type = expr2_type) && (binop_type binop) = "bool") then (
        if (expr1_type = "int" && (binop_op_type binop) = "bb") ||
           (expr1_type = "bool" && (binop_op_type binop) = "bi") then
           failwith "Error: Operator/operands types mismatch."
        else
            "bool"
    )
    else (
        if (expr1_type = "int" && expr2_type = "int" && (binop_type binop) = "int") then
            "int"
        else
            failwith "Error: Operator/operands types mismatch."
    )

(* calculate the type of unop expr *)
and match_unop supproc (unop, expr) =
    let expr_type = match_expr supproc expr in
    if (expr_type = "bool" && (unop_type unop) = "bool") then
        "bool"
    else (
        if (expr_type = "int" && (unop_type unop) = "int") then
            "int"
        else
            failwith "Error: Operator/operands types mismatch."
    )

let match_basic_expr_type supproc expr =
    match expr with
        | Ebool(b) -> "bool"
        | Eint(i) -> "int"
        | Elval(lva) -> "lval"
        | Ebinop(binop) -> "binop"
        | Eunop(unop) -> "unop"

(* ================================================ *)
(* ========     Check Functions         =========== *)
(* ================================================ *)

(* check rvalue
 *      if rvalue is an expression --> call match_expr
 *      if rvalue is an struct --> call check_struct *)
let rec check_rvalue supproc type_name left_id_list rvalue =
    match rvalue with
        | Rexpr(expr) -> (let right_type = match_expr supproc expr in 
                          if type_name <> right_type then 
                                failwith "Error: Assignment type mismatch."
                          else (
                            if type_name = "record" then
                                check_identifier_type supproc supproc (String.concat "." left_id_list) (find_expr_name supproc expr);
                          )
                         )
        | Rstruct(rs) -> (if type_name <> "record" then 
                            failwith "Error: Assignment type mismatch, record != simple type."
                          else (
                            let type_name = check_struct supproc left_id_list rs in
                            match type_name with
                                |_ -> ()))(* check_struct supproc type_name rs *)
(* check struct initialization:
 *      check if the field name matches the field name defined *)
and check_struct supproc left_id_list fieldlst =
    (* check field duplication *)
    List.map (check_fieldinit supproc left_id_list) fieldlst
(* check if the lvalue names in the init is in the typedef *)
and check_fieldinit supproc left_id_list (id, rvalue) = 
    let full_id_list = left_id_list@[id] in
    let full_id = String.concat "." full_id_list in
    let left_type = (find_symbol full_id supproc).sym_typespec in
    check_rvalue supproc left_type full_id_list rvalue

(* check if an identifier is of same type of lvalue *)
and check_identifier_type lsupproc rsupproc lident rident =
    let lsymlst = List.filter (fun x -> (x.identifier = lident || x.super_symbol = lident) && x.scope = lsupproc && x.slot <> -1) symbol_table.symbol_list in
    let rsymlst = List.filter (fun x -> (x.identifier = rident || x.super_symbol = rident) && x.scope = rsupproc && x.slot <> -1) symbol_table.symbol_list in
    let llength = List.length lsymlst in
    let rlength = List.length rsymlst in
    let match_num = ref 0 in
    if llength <> rlength then
        failwith "Error: lvalue is of different record type from rvalue."
    else (
        for i = 0 to (llength - 1) do
            let lid_remain = replace lident "" (List.nth lsymlst i).identifier in
            for i = 0 to (llength - 1) do
                let rid_remain = replace rident "" (List.nth rsymlst i).identifier in
                if lid_remain = rid_remain then
                    match_num := !match_num + 1;
            done
        done;
        if !match_num <> llength then
            failwith "Error: lvalue is of different record type from rvalue. XXX"
    )


(* check each statement, matching with varies statement types *)
let rec check_stmt supproc stmt =
    match stmt with
        | Assign(ass) -> check_assign supproc ass
        | Read(rlv) -> check_read supproc rlv
        | Write(wexpr) -> check_write supproc wexpr
        | WriteS(ws) -> () (* skip checking string *)
        | Call(c) -> check_call supproc c
        | IfThen(ifth) -> check_ifthen supproc ifth
        | IfThenElse(ifte) -> check_ifthenelse supproc ifte
        | While(w) -> check_while supproc w

(* check assignment: check lvalue's type match rvalue's type *)
and check_assign supproc (lvalue, rvalue) =
    let left_type = match_lvalue supproc lvalue in
    let left_id_list = get_name_lvalue supproc lvalue in
    check_rvalue supproc left_type left_id_list rvalue;
    ()

(* check read statement: check the lvalue is bool/int or alias of these *)
and check_read supproc lvalue =
    let lvalue_type = match_lvalue supproc lvalue in
    if not (lvalue_type = "bool" || lvalue_type = "int") then
    begin
        failwith "Error: Can only read bool or int values."
    end

(* check write statement: check symbols in expr --> call check_expr *)
and check_write supproc expr =
    let type_name = match_expr supproc expr in
    match type_name with
        | _ -> ()

(* check call statement: 
 *      check if the proc name exists
 *      check if the params are in scope
 *      check if the params type and sequence *)
and check_call supproc (id, exprlst) =
    let proc_name = id in
    let param_list = find_all_params proc_name in
    let sorted_param_list = param_list in
    let call_num = List.length exprlst in
    let define_num = List.length param_list in
    check_callid proc_name; (* check proc name *)
    if call_num <> define_num then (* check param list length*)
        failwith "Error: Function call parameter number does not match.";
    for i = 0 to (call_num - 1) do (* check each param*)
        let define_type = (List.nth sorted_param_list i).sym_typespec in
        let call_type = match_expr supproc (List.nth exprlst i) in
        let define_pass = (List.nth sorted_param_list i).pass_by_ref in
        let call_pass = match_basic_expr_type supproc (List.nth exprlst i) in
        if define_pass = true && call_pass <> "lval" then
            failwith "Error: Cannot pass non-address type value into ref parameter."
        else
            if call_type <> define_type then
                failwith "Error: Function call with wrong param types.";
            if call_type = "record" then
                check_identifier_type proc_name supproc (List.nth sorted_param_list i).identifier (find_expr_name supproc (List.nth exprlst i));
    done
        
(* check `if then` statement:
 *      check if `if` expr is bool
 *      check `then` block --> recursive call check_stmt *)
and check_ifthen supproc (expr, stmtlst) = 
    let expr_type = match_expr supproc expr in
    if not(expr_type = "bool") then
        failwith "Error: if statement can only take bool type.";
    List.iter (fun x -> check_stmt supproc x) stmtlst

(* check `if then else` statement:
 *      check if `if` expr is bool
 *      check `then` block --> recursive call check_stmt
 *      check `else` block --> recursive call check_stmt *)
and check_ifthenelse supproc (expr, then_stmtlst, else_stmtlst) = 
    let expr_type = match_expr supproc expr in
    if not(expr_type = "bool") then
        failwith "Error: if statement can only take bool type.";
    List.iter (fun x -> check_stmt supproc x) then_stmtlst;
    List.iter (fun x -> check_stmt supproc x) else_stmtlst

(* check `while` statement:
 *      check `while` expr is bool
 *      check `do` block --> recursive call check_stmt *)
and check_while supproc (expr, stmtlst) = 
    let expr_type = match_expr supproc expr in
    if not(expr_type = "bool") then
        failwith "Error: while statement can only take bool type.";
    List.iter (fun x -> check_stmt supproc x) stmtlst

(* Check the procbody, iter through all statements *)
let check_procbody procname procbody =
    List.iter (check_stmt procname) procbody.stmts

(* Check procs *)
let check_proc (procheader, procbody) = 
    let procname = procheader.procname in
    check_procbody procname procbody

(* Entry point: given the program go check all its children *)
let check_program program =
    List.iter check_proc program.procs
