(* --------------------------------------------- *)
(* Main file of the bean compiler, which calls   *)
(* the ocamllex and ocamlyacc to do their job    *)
(* Also to mention, this version works for ocaml *)
(* 3.11.2                                        *)
(* --------------------------------------------- *)
open Printexc
module P = Bean_parse
module F = Format
module CG = Codegen 
module A = Analyze
module TC = Type_checking

(* Exception *)
exception Semantic_Error of string;;

(* Argument parsing code *)
let infile_name = ref None
type compiler_mode = PrettyPrint | Compile
let mode = ref Compile

(* --------------------------------------------- *)
(*  Specification for command-line options       *)
(* --------------------------------------------- *)
let (speclist:(Arg.key * Arg.spec * Arg.doc) list) =
  ["-p",
     Arg.Unit(fun () -> mode := PrettyPrint),
     " Run the compiler in pretty-printer mode"
  ]

let main () =
  (* Parse the command-line arguments *)
  Arg.parse speclist
      (begin fun fname -> infile_name := Some fname end)
      "bean [-p] [bean source]" ;
  (* Open the input file *)
  let infile = match !infile_name with
  | None -> stdin
  | Some fname -> open_in fname in
  (* Initialize lexing buffer *)
  let lexbuf = Lexing.from_channel infile in
    try
      (* Call the parser *)
      let prog = Bean_parse.program Bean_lex.token lexbuf in
      (* If the mode is pp, then call the print_program; 
       * otherwise, print the error msg *)
      match !mode with
      | PrettyPrint ->
            (prerr_string "Pretty Print mode is disabled.")
      | Compile ->
          begin
            A.alyz_program prog;
            (* A.show_table(); *)
            try
              TC.check_program prog
            with exn -> ignore
                        (print_string (Printexc.to_string exn); 
                         print_string "\n";
                         raise (Semantic_Error "e"));
            CG.generate_program F.std_formatter prog
          end
    with exn -> ()

let _ = main ()
