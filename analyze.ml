open Symbol_table

let alyz_fielddef suptype (iden,fieldtype)=
  match fieldtype with
        Bool-> Symbol_table.add_fielddef {fieldname=iden;  field_typespec="bool"; belong_type=suptype; 
                field_size=1;sub_field=false }
      | Int -> Symbol_table.add_fielddef {fieldname=iden;  field_typespec="int"; belong_type=suptype; 
                field_size=1; sub_field=false}
      | Flist(flst)-> alyz_fielddef_list iden flst;
                      let typesize=(calc_size_type iden) in
                      Symbol_table.add_fielddef{fieldname=iden; field_typespec=iden; type_size=typesize; sub_field=true} 
      | Id(id)-> let origin_type=Symbol_table.look_up_origin_type id in
                    Symbol_table.add_fielddef {fieldname=iden;  
                    field_typespec=origin_type;
                    belong_type=suptype; field_size=(find_typedef origin_type).type_size;
                    sub_field=(origin_type != "int" && origin_type != "bool") }
and alyz_fielddef_list suptype flist =
  List.map (alyz_fielddef suptype) flist;
  

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

let alyz_program program = 
    Symbol_table.init();
    List.map alyz_typedef program.typedefs
    (* List.map cg_proc program.procs; *)