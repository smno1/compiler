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

let rec alyz_subfieldlst supproc supsymbl ident typespec ref_flag flst=
    (* add an overall then add sub and the slot number for overall is -1*)
    Symbol_table.add_symbol{identifier=ident; slot=(-1); sym_typespec=typespec; 
        scope=supproc; sym_size=(calc_size_type typespec);pass_by_ref=ref_flag;super_symbol=supsymbl};
    List.iter (alyz_subfield supproc ident ref_flag) flst
and alyz_subfield supproc supsymbl ref_flag field=
    let fid=supsymbl^"."^field.fieldname in 
    match field.field_typespec with
        Bool-> Symbol_table.add_symbol {identifier=fid; slot=!proc_slot_count; sym_typespec="bool"; 
                scope=supproc; sym_size=1;pass_by_ref=ref_flag;super_symbol=supsymbl};
              proc_slot_count := !proc_slot_count+1
      | Int -> Symbol_table.add_symbol {identifier=fid; slot=!proc_slot_count; sym_typespec="int"; 
                scope=supproc; sym_size=1;pass_by_ref=ref_flag;super_symbol=supsymbl};
              proc_slot_count := !proc_slot_count+1
      | Flist(flst)-> alyz_subfieldlst_record supproc supsymbl fid "record" ref_flag flst
      | Id(id)-> let parameter_type=Symbol_table.find_typedef id in
                    if parameter_type.sub_type then
                      let flst=Symbol_table.find_all_fields id in
                        alyz_subfieldlst supproc supsymbl fid parameter_type.typename ref_flag flst
                    else
                      let origin_type=Symbol_table.look_up_origin_type id in
                          Symbol_table.add_symbol{identifier=fid; slot=!proc_slot_count; sym_typespec=origin_type; 
                            scope=supproc; sym_size=1; pass_by_ref=ref_flag;super_symbol=supsymbl;};
                          proc_slot_count := !proc_slot_count+1
and alyz_subfieldlst_record supproc supsymbl ident ref_flag flst=
    List.iter (alyz_subfield supproc ident ref_flag) flst;
    Symbol_table.add_symbol{identifier=ident; slot=(-1); sym_typespec="record"; 
        scope=supproc; sym_size=(calc_size_record_by_super_symbol ident);pass_by_ref=ref_flag;super_symbol=supsymbl}


let alyz_symbol_typespec supproc,ref_flag, typespec,ident=
    match typespec with
        Bool-> Symbol_table.add_symbol {identifier=ident; slot=!proc_slot_count; sym_typespec="bool"; 
                            scope=supproc; sym_size=1; pass_by_ref=ref_flag;super_symbol=""};
                          proc_slot_count := !proc_slot_count+1
      | Int -> Symbol_table.add_symbol {identifier=ident; slot=!proc_slot_count; sym_typespec="int"; 
                            scope=supproc; sym_size=1; pass_by_ref=ref_flag;super_symbol=""};
                          proc_slot_count := !proc_slot_count+1
      | Flist(flst)-> alyz_subfieldlst supproc "" ident "record" ref_flag flst
      | Id(id)-> let parameter_type=Symbol_table.find_typedef id in
                    if parameter_type.sub_type then
                      (* add an overall then add sub and the slot number for overall is -1*)
                      let flst=Symbol_table.find_all_fields id in
                        alyz_subfieldlst supproc "" ident parameter_type.typename ref_flag flst
                      (* ......add each field to symbol table *)
                    else
                      let origin_type=Symbol_table.look_up_origin_type id in
                          Symbol_table.add_symbol{identifier=ident; slot=!proc_slot_count; sym_typespec=origin_type; 
                            scope=supproc; sym_size=1; pass_by_ref=ref_flag;super_symbol="";};
                          proc_slot_count := !proc_slot_count+1

let alyz_parameter supproc (passspec,typespec,ident)=
    let ref_flag=(match passspec with Ref ->  true | Val ->  false) in
    alyz_symbol_typespec supproc,ref_flag, typespec,ident

let alyz_procheader procheader=
    List.iter (alyz_parameter procheader.procname) procheader.parameters;

let alyz_vardecl supproc (typespec,ident)=
    alyz_symbol_typespec supproc,false, typespec,ident
    
let alyz_procbody procname procbody=
    List.iter (alyz_vardecl procname) procbody.vardecls;
        
let alyz_proc (procheader, procbody) =
    let procname=procheader.procname in
    proc_slot_count := 0;
    alyz_procheader procheader;
    alyz_procbody procname procbody;
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






