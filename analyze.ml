(* ===========================================================================*)
(* Symbol table analyze and generation for the bean compiler                  *)
(* ---------------------------------------------------------------------------*)
(* It reads the syntax tree after parser and analyze the tree to generate     *)
(* symbol table                                                               *)
(* FileName: analyze.ml                                                       *)
(* Author: team Kylin                                                         *)
(* ===========================================================================*)

open Symbol
open Bean_ast

(* used to count the slot number in proc, reset to 0 once proc end *)
let proc_slot_count=ref 0

(* generate fielddef table *)
let rec alyz_fielddef_list suptype flist =
    List.iter (alyz_fielddef suptype) flist
and alyz_fielddef suptype (iden,fieldtype)=
    match fieldtype with
        Bool-> Symbol.add_fielddef {fieldname=iden;  field_typespec="bool"; 
                belong_type=suptype; field_size=1;sub_field=false }
      | Int -> Symbol.add_fielddef {fieldname=iden;  field_typespec="int"; 
                belong_type=suptype; field_size=1; sub_field=false}
      | Flist(flst)-> alyz_fielddef_list iden flst;
                      let typesize=(calc_size_type iden) in
                      Symbol.add_fielddef{fieldname=iden; field_typespec=iden; 
                      belong_type=suptype; field_size=typesize; sub_field=true}
      | Id(id)-> let origin_type=Symbol.look_up_origin_type id in
                    Symbol.add_fielddef {fieldname=iden;  
                    field_typespec=origin_type; belong_type=suptype; 
                    field_size=(find_typedef origin_type).type_size;
                    sub_field=(origin_type <> "int" && origin_type <> "bool") }

(* generate typedef table *)
let alyz_typedef (typespec,ident)=
    match typespec with
        Bool-> Symbol.add_typedef {typename=ident;  typespec="bool"; 
                type_size=1; sub_type=false }
      | Int -> Symbol.add_typedef {typename=ident;  typespec="int"; 
                type_size=1; sub_type=false }
      (* didn't check the original type of record fliedlist here*)
      | Flist(flst)-> alyz_fielddef_list ident flst;
                      let typesize=(calc_size_type ident) in
                      Symbol.add_typedef{typename=ident; typespec=ident; 
                      type_size=typesize; sub_type=true} 
      (* if type define using an id which means that it is an alias *)
      | Id(id)-> let origin_type=Symbol.look_up_origin_type id in
                    Symbol.add_typedef {typename=ident;  typespec=origin_type; 
                    type_size=(find_typedef origin_type).type_size; 
                    sub_type=(origin_type <> "int" && origin_type <> "bool") }

(* Call when a parameter or varaible is declared with identifer refered to a  *)
(* structure recursive from structure symbol to all its children to add       *)
(* symbols to symbol table when a symbol is a identifer symbol, then store    *)
(* its type with slot -1 in the symbol table                                  *)
let rec add_symbol_from_an_identifer_record supproc 
          supsymbl ref_flag param_flag field=
    let fid=supsymbl^"."^field.fieldname in
    if field.sub_field then 
      let flst=Symbol.find_all_fields field.field_typespec in
        List.iter (add_symbol_from_an_identifer_record supproc f
                    id ref_flag param_flag) flst;
        Symbol.add_symbol {identifier=fid; slot=(-1); sym_typespec="record"; 
                          scope=supproc; 
                          sym_size=(calc_size_record_by_super_symbol fid);
                          pass_by_ref=ref_flag; super_symbol=supsymbl; 
                          param=param_flag}
    else
      (Symbol.add_symbol {identifier=fid; slot = !proc_slot_count; 
                          sym_typespec=field.field_typespec; 
                          scope=supproc; sym_size=1; pass_by_ref=ref_flag;
                          super_symbol=supsymbl; param=param_flag};
                          proc_slot_count := !proc_slot_count+1)

(* Call when a parameter or varaible is declared with a field list structure  *)
(* recursive from structure symbol to all its children to add symbols to      *)
(* symbol table when a symbol is a fieldlst symbol, then store it as record   *)
(* type with slot -1 in the symbol table                                      *)
let rec alyz_subfield supproc supsymbl ref_flag param_flag (id,typespec)=
    let fid=(if supsymbl="" then id else supsymbl^"."^id) in 
    match typespec with
        Bool-> Symbol.add_symbol {identifier=fid; slot = !proc_slot_count; 
                                  sym_typespec="bool"; scope=supproc; 
                                  sym_size=1; pass_by_ref=ref_flag;
                                  super_symbol=supsymbl; param=param_flag};
              proc_slot_count := !proc_slot_count+1
      | Int -> Symbol.add_symbol {identifier=fid; slot = !proc_slot_count; 
                                  sym_typespec="int"; scope=supproc; 
                                  sym_size=1; pass_by_ref=ref_flag;
                                  super_symbol=supsymbl; param=param_flag};
              proc_slot_count := !proc_slot_count+1
      | Flist(flst)-> alyz_subfieldlst_record supproc supsymbl 
                                                fid ref_flag param_flag flst
      | Id(alias)-> let parameter_type=Symbol.find_typedef alias in
                    if parameter_type.sub_type then
                      let flst=Symbol.find_all_fields parameter_type.typespec in
                        List.iter (add_symbol_from_an_identifer_record supproc 
                          fid ref_flag param_flag) flst;
                        Symbol.add_symbol 
                          {identifier=id; slot=(-1); sym_typespec="record"; 
                          scope=supproc; 
                          sym_size=(calc_size_record_by_super_symbol id);
                          pass_by_ref=ref_flag; super_symbol=supsymbl; 
                          param=param_flag}
                    else
                      let origin_type=parameter_type.typespec in
                          Symbol.add_symbol
                            {identifier=fid; slot = !proc_slot_count; 
                             sym_typespec=origin_type; scope=supproc; 
                             sym_size=1; pass_by_ref=ref_flag; 
                             super_symbol=supsymbl; param=param_flag};
                          proc_slot_count := !proc_slot_count+1
and alyz_subfieldlst_record supproc supsymbl ident ref_flag param_flag flst=
    (* let fid=(if supsymbl="" then ident else supsymbl^"."^ident) in *)
    List.iter (alyz_subfield supproc ident ref_flag param_flag) flst;
    Symbol.add_symbol{identifier=ident; slot=(-1); sym_typespec="record"; 
                      scope=supproc; 
                      sym_size=(calc_size_record_by_super_symbol ident);
                      pass_by_ref=ref_flag; super_symbol=supsymbl; 
                      param=param_flag}

(* analyze proc parameters *)
let alyz_parameter supproc (passspec,typespec,ident)=
    let ref_flag=(match passspec with Ref ->  true | Val ->  false) in
      alyz_subfield supproc "" ref_flag true (ident,typespec)

(* analyze proc *)
let alyz_procheader procheader=
    List.iter (alyz_parameter procheader.procname) procheader.parameters

(* analyze proc variable declation *)
let alyz_vardecl supproc (typespec,ident)=
    alyz_subfield supproc "" false false (ident,typespec) 
    
(* analyze proc body  *)
let alyz_procbody procname procbody=
    List.iter (alyz_vardecl procname) procbody.vardecls
        
let alyz_proc (procheader, procbody) =
    let procname=procheader.procname in
    proc_slot_count := 0;
    alyz_procheader procheader;
    alyz_procbody procname procbody;
    Symbol.add_proc {proc_name=procname; 
                     proc_size=(Symbol.calc_size_proc procname)}

let alyz_program program = 
    Symbol.init();
    List.iter alyz_typedef program.typedefs;
    List.iter alyz_proc program.procs

(* a function to show current current symbol table *)
let show_table () =
    print_string "======type table=======\n";
    Symbol.print_type_list Symbol.typedef_table.typedef_list;
    print_string "\n======fielsd table=======\n";
    Symbol.print_field_list Symbol.fielddef_table.fielddef_list;
    print_string "\n======symbol table=======\n";
    Symbol.print_symbol_list Symbol.symbol_table.symbol_list;
    print_string "\n======proc table=======\n";
    Symbol.print_proc_list Symbol.proc_table.proc_list;
