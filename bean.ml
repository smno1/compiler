(* --------------------------------------------- *)
(* Main file of the bean compiler, which calls   *)
(* the ocamllex and ocamlyacc to do their job    *)
(* Also to mention, this version works for ocaml *)
(* 3.11.2                                        *)
(* --------------------------------------------- *)
module P = Bean_parse
module F = Format
module PP = Bean_pprint

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
        PP.print_program F.std_formatter prog 
      | Compile -> 
        prerr_string "Sorry, cannot generate code yet.\n"
    with exn -> ()

let _ = main ()
