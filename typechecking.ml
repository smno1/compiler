(* This file contains all the well-formedness checkers *)
open Symbol_table
open Bean_ast

(* Needs correction here *)
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

(* Types utilities *)
type t = 
	| Int of int
	| Bool of bool
	| String of string
	| Record of symbol list
	;;

let rec to_string = function
  | Int -> "int"
  | Bool -> "bool"
  | String -> "string";;

let rec real_type = function
	| Int -> Int
	| Bool -> Bool
	| 

let rec check expected actual =
	match (expected, actual) with =
	| (Int, Int) 					-> true
	| (Bool, Bool) 				-> true
	| (String, String)    -> true
  | _										-> false


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

(* check call: check if calling the proc is with proper name / params *)
let check_call call_name call_param =
	if not(List.exists(fun x -> x.identifier = call_name) symbol_table.symbol_list)
	then failwith "Error: The proc called is not defined.\n"
	let param_list = find_all_param call_name in
	let match_num = ref 0 in
	begin
		param_list <- sort param_list;
		for i = 0 to (List.length param_list - 1) do
			if (List.nth param_list i).sym_typespec = List.nth call_param i 
		done
	end

(* check write/read: read int/bool, write expression/string *)
let check_read read_object =
	if not(to_string read_object.t = "bool" or to_string read_object.t = "int") 
	then failwith "Error: Can only read "

let check_write wexpr =



(* check if expressions are valid *)
let check_expr expr = 
	match expr with
		| Ebool(b) -> 
		| Eint(i) -> 
		| Elval(lva) -> check_lvalue
		| Ebinop(binop) -> check_binop
		| Eunop(unop) -> check_unop

let check_stmt stmt =
	match stmt with
		|	Assign(ass) -> check_assign
		| Read(rlv) -> check_read
		| Write(wexpr) -> check_write
		| WriteS(ws) -> check_writeS
		| Call(c) -> check_call
		| IfThen(ifth) -> check_ifthen
		| IfThenElse(ifth) -> check_ifThenElse
		| While(w) -> check_while


let check_procbody procname procbody =
	List.iter (check_stmt procname) procbody.stmts


let check_proc (procheader, procbody) = 
	let procname = procheader.procname in
	check_procheader procheader;
	check_procbody procname procbody


let check_program program =
	List.iter check_typedef program.typedefs;
	List.iter check_proc program.procs
