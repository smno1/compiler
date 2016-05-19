(* This file contains all the well-formedness checkers *)
open Symbol_table
open Bean_ast


(* ================================================ *)
(* =====      Utility Functions         =========== *)
(* ================================================ *)

let rec convert_to_primitive type_instance = []
(* 	let fieldsList = find_all_fields type_instance.typename in
	let primitiveList = [] in
	List.iter (fun x -> if x.typespec = "bool" || x.typespec = "int" 
											then primitiveList <- x.typespec::primitiveList
											else primitiveList <- (convert_to_primitive (find_typedef x.typespec))::primitiveList;) fieldsList;
	!primitiveList
 *)

(* compare 2 lists with primitive types *)
let compare_list list1 list2 = 
	if List.length list1 != List.length list2 then false else
	if (List.filter (fun x -> x.typespec = "bool") list1) = 
	   (List.filter (fun x -> x.typespec = "bool") list2) then true else false

(* sort the param list according to its sequence field *)
let rec sort = function
    | [] -> []
    | x :: l -> insert x (sort l)
  and insert elem = function
    | [] -> [elem]
    | x :: l -> if elem.slot < x.slot then elem :: x :: l
                else x :: insert elem l;;

(* Ensures that a type congruent to an expected type *)
let compare_type type1 type2 = 
	if type1.typename = type2.typename then 
		true
	else ( (* if two type names equals, two types are the same *)
		if type1.sub_type != type2.sub_type then 
			false 
		else ( (* if two type are on different lvls, they are different *)
			if not (type1.sub_type = true && type2.sub_type = true) then 
				false
			else (
				if compare_list (convert_to_primitive type1) (convert_to_primitive type2) then 
					true
				else
					false
			)
		)
	)

(* check each expression, matching with vasries expression types *)
let check_expr expr = 
	match expr with
		| Ebool(b) -> 
		| Eint(i) -> 
		| Elval(lva) -> check_lvalue
		| Ebinop(binop) -> check_binop
		| Eunop(unop) -> check_unop


(* check each statement, matching with varies statement types *)
let check_stmt supproc stmt =
	match stmt with
		| Assign(ass) -> check_assign supproc ass
		| Read(rlv) -> check_read supproc rlv
		| Write(wexpr) -> check_write supproc wexpr
		| WriteS(ws) -> () (* skip checking string *)
		| Call(c) -> check_call supproc c
		| IfThen(ifth) -> check_ifthen ifth
		| IfThenElse(ifte) -> check_ifThenElse ifte
		| While(w) -> check_while w

(* check assignment: check lvalue's type match rvalue's type *)
and check_assign supproc (lvalue, rvalue) =
	let lvalue_symbol = find_symbol lvalue supproc in
	let rvalue_symbol = get_expression_type rvalue supproc in (* TODO: get_expression_type *)
  	let lvalue_origin_type = get_leaf_symbol_by_super_symbol lvalue_symbol.sym_typespec supproc in (* TODO: get structured real type list *)
  	let rvalue_origin_type = get_leaf_symbol_by_super_symbol rvalue_symbol.sym_typespec supporc in
  	if not(compare_list lvalue_origin_type rvalue_origin_type) then 
  		failwith "Error: LHS and RHS of assignment should be of same type."

(* check read statement: check the lvalue is bool/int or alias of these *)
and check_read supproc lvalue =
	let lvalue_symbol = find_symbol lvalue supproc in
	let lvalue_type = find_typedef lvalue_symbol.sym_typespec in
	if lvalue_type.sub_field = true then
		failwith "Error: Can only read bool or int values."

(* check write statement: check symbols in expr --> call check_expr *)
and check_write supproc expr =
	check_expr supproc expr

(* check call statement: 
 * 		check if the proc name exists
 *		check if the params are in scope
 * 		check if the params type and sequence *)
and check_call supporc (id, exprlst) =
	let proc_name = find_proc id in
	let param_list = find_all_param call_name in
	let match_num = ref 0 in
	begin (* TODO: check record types *)
		param_list <- sort param_list;
		for i = 0 to (List.length param_list - 1) do
			if (List.nth param_list i).sym_typespec = List.nth call_param i 
		done
	end

(* check `if then` statement:
 * 		check if `if` expr is bool
 *		check `then` block --> recursive call check_stmt *)
and check_ifthen supproc (expr, stmtlst) = 
	let expr_type = get_expression_type expr supproc in
	begin
		if not(expr_type = "bool") then
			failwith "Error: if statement can only take bool type."
		List.iter (fun x -> check_stmt supproc x) stmtlst
	end

(* check `if then else` statement:
 * 		check if `if` expr is bool
 * 		check `then` block --> recursive call check_stmt
 * 		check `else` block --> recursive call check_stmt *)
and check_ifthenelse supproc (expr, then_stmtlst, else_stmtlst) = 
	let expr_type = get_expression_type expr supproc in
	begin
		if not(expr_type = "bool") then
			failwith "Error: if statement can only take bool type."
		List.iter (fun x -> check_stmt supproc x) then_stmtlst
		List.iter (fun x -> check_stmt supproc x) else_stmtlst
	end

(* check `while` statement:
 * 		check `while` expr is bool
 * 		check `do` block --> recursive call check_stmt *)
and check_while supproc (expr, stmtlst) = 
	let expr_type = get_expression_type expr supproc in
	begin
		if not(expr_type = "bool") then
			failwith "Error: while statement can only take bool type."
		List.iter (fun x -> check_stmt supproc x) stmtlst
	end


(* Check the procbody, iter through all statements *)
let check_procbody procname procbody =
	List.iter (check_stmt procname) procbody.stmts

(* Check procs *)
let check_proc (procheader, procbody) = 
	let procname = procheader.procname in
	check_procheader procheader;
	check_procbody procname procbody

(* Entry point: given the program go check all its children *)
let check_program program =
	(* List.iter check_typedef program.typedefs; skip the typedef for now *)
	List.iter check_proc program.procs
