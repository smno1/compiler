(* This file contains all the well-formedness checkers *)
open Symbol_table

(* Needs correction here *)
let rec convert_to_primitive type_instance = []
(* 	let fieldsList = find_all_fields type_instance.typename in
	let primitiveList = [] in
	List.iter (fun x -> match x.typespec with
												| bool | int -> x.typespec::primitiveList
												| 
		if x.typespec = "bool" || x.typespec = "int" 
											then x.typespec::primitiveList
											else (convert_to_primitive (find_typedef x.typespec))::primitiveList;) fieldsList;
	!primitiveList *)


let compare_list list1 list2 = 
	if List.length list1 != List.length list2 then false else
	if (List.filter (fun x -> x.typespec = "bool") list1) = 
	   (List.filter (fun x -> x.typespec = "bool") list2) then true else false

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


(* check assignment  *)

(* check *)