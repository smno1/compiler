open Symbol_table
open Bean_ast

let proc_slot_count=ref 0;

let rec alyz_fielddef_list suptype flist =
    List.iter (alyz_fielddef suptype) flist
and alyz_fielddef suptype (iden,fieldtype)=
    match fieldtype with
        Bool-> Symbol_table.add_fielddef {fieldname=iden;  field_typespec="bool"; belong_type=suptype; 
                field_size=1;sub_field=false }
      | Int -> Symbol_table.add_fielddef {fieldname=iden;  field_typespec="int"; belong_type=suptype; 
                field_size=1; sub_field=false}
      | Flist(flst)-> alyz_fielddef_list iden flst;
                      let typesize=(calc_size_type iden) in
                      Symbol_table.add_fielddef{fieldname=iden; field_typespec=iden; belong_type=suptype; 
                      field_size=typesize; sub_field=true} 
      | Id(id)-> let origin_type=Symbol_table.look_up_origin_type id in
                    Symbol_table.add_fielddef {fieldname=iden;  
                    field_typespec=origin_type;
                    belong_type=suptype; field_size=(find_typedef origin_type).type_size;
                    sub_field=(origin_type != "int" && origin_type != "bool") }


let alyz_typedef (typespec,ident)=
    match typespec with
        Bool-> Symbol_table.add_typedef {typename=ident;  typespec="bool"; type_size=1; sub_type=false }
      | Int -> Symbol_table.add_typedef {typename=ident;  typespec="int"; type_size=1; sub_type=false }
      | Flist(flst)-> alyz_fielddef_list ident flst;
                      (* type checking can do here by make typespec literally assign *)
                      let typesize=(calc_size_type ident) in
                      Symbol_table.add_typedef{typename=ident; typespec=ident; type_size=typesize; sub_type=true} 
      (* if type define using an id which means that it is an alias *)
      | Id(id)-> let origin_type=Symbol_table.look_up_origin_type id in
                    Symbol_table.add_typedef {typename=ident;  typespec=origin_type; 
                    type_size=(find_typedef origin_type).type_size; 
                    sub_type=(origin_type != "int" && origin_type != "bool") }



let alyze_parameter supproc (passspec,typespec,ident)=
    match passspec with
        (* if it is a ref record store all the sub filed variable as ref? *)
        Ref ->  match typespec with
                    Bool-> Symbol_table.add_symbol {identifier=ident; slot=!proc_slot_count; sym_typespec="not_found"; 
                            scope=supproc; sym_size=0; pass_by_ref=ref_flag}
                  | Int -> ()
                  | Flist(flst)-> ()
                  | Id(id)-> ()  
      | Val ->  false
    (* ref doesn not need slot? *)
    (* TO DO!! type value check: using init part, not assign yet  *)
    if ref_flag then 
      Symbol_table.add_symbol {identifier=ident; slot=!proc_slot_count; sym_typespec=""; 
                  scope=supproc; sym_size=0; pass_by_ref=true}
                proc_slot_count := !proc_slot_count+1 
    else 
      match typespec with
          Bool-> Symbol_table.add_symbol {identifier=ident; slot=!proc_slot_count; sym_typespec="not_found"; 
                  scope=supproc; sym_size=0; pass_by_ref=ref_flag}
        | Int -> ()
        | Flist(flst)-> ()
        | Id(id)-> ()
      (* haven't consider the parameter type like {int a} *)
      (* let symbol_type = look_up_origin_type *)
      Symbol_table.add_symbol {identifier=ident; slot=!proc_slot_count; sym_typespec="not_found"; 
          scope=supproc; sym_size=0; pass_by_ref=ref_flag}
    

let alyz_procheader procheader=
    List.iter (alyze_parameter procheader.procname) procheader.parameters;
        
let alyz_proc (procheader, procbody) =
    proc_slot_count := ;
    alyz_procheader procheader;
    alyz_procbody procbody;
    let procname=procheader.procname in
    Symbol_table.add_proc {procname=procname; proc_size=(Symbol_table.calc_size_proc procname)}

let alyz_program program = 
    Symbol_table.init();
    List.iter alyz_typedef program.typedefs;
    List.iter alyz_proc program.procs

let show_table () =
    print_string "======type table=======\n";
    List.iter (fun x -> print_string (x.typename^" ")) Symbol_table.typedef_table.typedef_list;
    print_string "\n======fielsd table=======\n";
    List.iter (fun x -> print_string (x.fieldname^" ")) Symbol_table.fielddef_table.fielddef_list;
    ()






