(* Define the types for each symbol table element *)
type symbol={identifier:string; slot:string; typespec:string; scope:string; init:bool }
type typedef={typename:string;  typespec:string; size:int }
type fielddef={fieldname:string;  typespec:string; belong_type:string }
type proc={procname:string;  typespec:string; size:int }


(* Define the 4 types of symbol tables *)
type symbol_stack={mutable symbol_list : symbol list}
type typedef_stack={mutable typedef_list : typedef list}
type fielddef_stack={mutable fielddef_list : fielddef list}
type proc_stack={mutable proc_list : proc list}


(* default elements *)
let symbol_not_found={identifier="not_found_404";slot="not_found";typespec="not_found"; scope="not_found";init=false}
let typedef_not_found={typename="not_found_404";  typespec="not_found"; size=0 }
let fielddef_not_found={fieldname="not_found_404";  typespec="not_found"; belong_type="not_found" }
let proc_not_found={procname="not_found_404";  typespec="not_found"; size=0 }


let symbol_table= {symbol_list=[]}
let typedef_table= {typedef_list=[]}
let fielddef_table= {fielddef_list=[]}
let proc_table= {proc_list=[]}


(* init function *)
let init symbol_list typedef_list fielddef_list proc_list=
    symbol_table.symbol_list <- symbol_list
    typedef_table.typedef_list <- typedef_list
    fielddef_table.fielddef_list <- fielddef_list
    proc_table.proc_list <- proc_list


(* init primitive types *)
let init_d typedef_list = 
	let int_instance = { typename="int"; typespec="int"; size=1 } in 
	let bool_instance = { typename="bool"; typespec="bool"; size=1 } in
	add_typedef int_instance
	add_typedef bool_instance


(* all the `find element` functions *)
let find_symbol id scope =
    try (List.find (fun s->s.identifier=id; s.scope=scope) symbol_table.symbol_list) with Not_found -> symbol_not_found

let find_typedef id =
    try (List.find (fun s->s.typename=id) typedef_table.typedef_list) with Not_found -> typedef_not_found

let find_fielddef id belong_type=
    try (List.find (fun s->s.fieldname=id; s.belong_type=belong_type) fielddef_table.fielddef_list) with Not_found -> fielddef_not_found

let find_proc id =
    try (List.find (fun s->s.procname=id) proc_table.proc_list) with Not_found -> proc_not_found


(* all the `add element` functions *)
let add_symbol x =
    if not (List.exists (fun s->s.identifier=x.identifier) symbol_table.symbol_list) then
        symbol_table.symbol_list <- x::symbol_table.symbol_list

let add_typedef x =
    if not (List.exists (fun s->s.typename=x.typename) typedef_table.typedef_list) then
        typedef_table.typedef_list <- x::typedef_table.typedef_list

let add_fielddef x =
    if not (List.exists (fun s->s.fieldname=x.fieldname) fielddef_table.fielddef_list) then
        fielddef_table.fielddef_list <- x::fielddef_table.fielddef_list

let add_proc x =
    if not (List.exists (fun s->s.procname=x.procname) proc_table.proc_list) then
        proc_table.proc_list <- x::proc_table.proc_list


(* `look for all element in certain scope` functions for proc -> symbol and type -> field *)
let find_all_fields type_name = 
	let result = List.filter (fun x -> x.belong_type = type_name) fielddef_table.fielddef_list in
	result

let find_all_symbol proc_name = 
	let result = List.filter (fun x -> x.scope = proc_name) proc_table.proc_list in
	result


(* `calculate the size it takes` functions for proc and typedef *)
let calc_size_type type_name = 
	let all_elements = find_all_fields type_name in
	let total_size = 0 in
	List.iter (fun x -> total_size + x.size) all_elements
	total_size

let calc_size_proc proc_instance = 
	let all_elements = find_all_symbol proc_name in
	let total_size = 0 in
	List.iter (fun x -> total_size + (calc_size_type x.typespec)) all_elements
	total_size