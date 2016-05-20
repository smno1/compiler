(* This file contains all the well-formedness checkers *)
open Symbol
open Bean_ast
open Printf
open String

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
	print_string "Matching symbol ... ";
	Printf.printf "%s" id;
	print_string "\n";
	check_symbol_id id supproc;
	let symbol = find_symbol id supproc in
	let symbol_type = symbol.sym_typespec in
	let real_type = look_up_origin_type symbol_type in
	real_type

(* calculate the type of lvalue *)
let rec match_lvalue supproc lvalue =
	print_string "Checking lvalue ... ";
	let id_list = [] in
	match lvalue with
		| LId(id) -> match_symbol supproc id
		| LField(lfield) -> ((* Printf.printf "%s" (String.concat "." (
							get_id_lfield supproc lfield id_list));
							print_string "\n"; *)
							match_symbol supproc (String.concat "." (
							get_id_lfield supproc lfield id_list)))
		
and get_id_lvalue supproc lvalue idlst =
	match lvalue with
		| LId(id) -> idlst@[id]
		| LField(lfield) -> get_id_lfield supproc lfield idlst
and get_id_lfield supproc (lvalue, id) idlst = 
	let newlst = get_id_lvalue supproc lvalue idlst in
	newlst@[id]

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


(* ================================================ *)
(* ========     Check Functions         =========== *)
(* ================================================ *)

(* check lvalue: check if lvalue is defined in the scope *)
let rec check_lvalue supproc lvalue =
	match lvalue with
		| LId(id) -> check_symbol_id id supproc
		| LField(lfield) -> ()

(* check each expression, matching with vasries expression types *)
let rec check_expr supproc expr =
	match expr with
		| Ebool(b) -> () (* if the element is matched with bool *)
		| Eint(i) -> () (* if the element is matched with int *)
		| Elval(lva) -> () (* check_lvalue *)
		| Ebinop(binop) -> () (* check_binop *)
		| Eunop(unop) -> () (* check_unop *)

(* check rvalue
 *		if rvalue is an expression --> call check_expr
 * 		if rvalue is an struct --> call check_struct *)
let rec check_rvalue supproc type_name left_id_list rvalue =
	print_string "Checking rvalue .................... ";
	match rvalue with
		| Rexpr(expr) -> (let right_type = match_expr supproc expr in 
						 (Printf.printf "%s" right_type; 
						  print_string "\n";
						  if type_name <> right_type then 
						  	failwith "Error: Assignment type mismatch."))
		| Rstruct(rs) -> (print_string "Entering struct ..."; 
						  if type_name <> "record" then 
						  	failwith "Error: Assignment type mismatch, record != simple type."
						  else (
						  	check_struct supproc left_id_list rs;
						  	()))(* check_struct supproc type_name rs *)
(* check struct initialization:
 * 		check if the field name matches the field name defined *)
and check_struct supproc left_id_list fieldlst =
	(* check field duplication *)
 	List.map (check_fieldinit supproc left_id_list) fieldlst
(* check if the lvalue names in the init is in the typedef *)
and check_fieldinit supproc left_id_list (id, rvalue) = 
	print_string " ---------------------\n";
	let full_id_list = left_id_list@[id] in
	let full_id = String.concat "." full_id_list in
	let left_type = (find_symbol full_id supproc).sym_typespec in
	check_rvalue supproc left_type full_id_list rvalue


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
and check_assign supproc (lvalue, rvalue) = (* TODO: match rvalue with expr/struct *)
	print_string "============================================>";
	Printf.printf "%s" supproc;
	(* print_string "\n"; *)
	let left_type = match_lvalue supproc lvalue in
	let left_id_list = get_name_lvalue supproc lvalue in
	Printf.printf " xxx  %s" left_type;
	print_string "\n";
	check_rvalue supproc left_type left_id_list rvalue;
	()
	(* let lvalue_type = match_lvalue supproc lvalue in
	check_rvalue supproc lvalue_type rvalue;
	let rvalue_type = match_rvalue supproc rvalue in
	if not (lvalue_type = rvalue_type) then
		failwith "Error: The assignment is invalid.\n" *)

	(* let lvalue_symbol = find_symbol_instance lvalue supproc in
	let rvalue_symbol = get_expression_type rvalue supproc in (* TODO: get_expression_type *)
  	let lvalue_origin_type = get_leaf_symbol_by_super_symbol lvalue_symbol.sym_typespec supproc in TODO: get structured real type list
  	let rvalue_origin_type = get_leaf_symbol_by_super_symbol rvalue_symbol.sym_typespec supporc in
  	if not(compare_list lvalue_origin_type rvalue_origin_type) then 
  		failwith "Error: LHS and RHS of assignment should be of same type."
	 *)

(* check read statement: check the lvalue is bool/int or alias of these *)
and check_read supproc lvalue =
	print_string "Checking read statement ... \n";
	let lvalue_type = match_lvalue supproc lvalue in
	if not (lvalue_type = "bool" || lvalue_type = "int") then
	begin
		failwith "Error: Can only read bool or int values."
	end

	(* let lvalue_symbol = find_symbol lvalue supproc in
	let lvalue_type = find_typedef lvalue_symbol.sym_typespec in
	if lvalue_type.sub_field = true then
		failwith "Error: Can only read bool or int values." *)

(* check write statement: check symbols in expr --> call check_expr *)
and check_write supproc expr =
	check_expr supproc expr

(* check call statement: 
 * 		check if the proc name exists
 *		check if the params are in scope
 * 		check if the params type and sequence *)
and check_call supporc (id, exprlst) = ()
	(* let proc_name = find_proc id in
	let param_list = find_all_params id in
	let match_num = ref 0 in
	begin (* TODO: check record types *)
		param_list <- sort exprlst;
		for i = 0 to (List.length param_list - 1) do
			if (List.nth param_list i).sym_typespec != List.nth call_param i then
				failwith "Error: Function call with wrong params."
		done
	end *)

(* check `if then` statement:
 * 		check if `if` expr is bool
 *		check `then` block --> recursive call check_stmt *)
and check_ifthen supproc (expr, stmtlst) = 
	let expr_type = match_expr supproc expr in
	begin
		if not(expr_type = "bool") then
			failwith "Error: if statement can only take bool type."
		(* List.iter (fun x -> check_stmt supproc x) stmtlst *)
	end

(* check `if then else` statement:
 * 		check if `if` expr is bool
 * 		check `then` block --> recursive call check_stmt
 * 		check `else` block --> recursive call check_stmt *)
and check_ifthenelse supproc (expr, then_stmtlst, else_stmtlst) = 
	let expr_type = match_expr supproc expr in
	begin
		if not(expr_type = "bool") then
			failwith "Error: if statement can only take bool type."
		(* List.iter (fun x -> check_stmt supproc x) then_stmtlst
		List.iter (fun x -> check_stmt supproc x) else_stmtlst *)
	end

(* check `while` statement:
 * 		check `while` expr is bool
 * 		check `do` block --> recursive call check_stmt *)
and check_while supproc (expr, stmtlst) = 
	print_string "Checking While statement .......\n";
	let expr_type = match_expr supproc expr in
	(begin
		if not(expr_type = "bool") then
			failwith "Error: while statement can only take bool type."
		(* List.iter (fun x -> check_stmt supproc x) stmtlst *)
	end);
	print_string "Exiting While statement ......."


(* Check the procbody, iter through all statements *)
let check_procbody procname procbody =
	List.iter (check_stmt procname) procbody.stmts

(* Check procs *)
let check_proc (procheader, procbody) = 
	print_string "Checking proc ... ";
	let procname = procheader.procname in
	Printf.printf "%s" procname;
	print_string "\n";
	check_procbody procname procbody

(* Entry point: given the program go check all its children *)
let check_program program =
	print_string "Checking program ...\n";
	List.iter check_proc program.procs
