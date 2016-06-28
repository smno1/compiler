
With this README file you should also have found these:

Makefile:  
    A makefile for compiler

Makefile.depend:
    A listing of the file dependencies

bean.ml:
    The main module

bean_ast.ml:
    The data structures that make up the (currently limited) AST

bean_ast.mli:
    The interface file for bean_ast.ml

bean_lex.mll:
    An ocamllex specification for bean

bean_parse.mly:
    An ocamlyacc specification for bean

bean_pprint.ml:
    A pretty-printer 

bean_pprint.mli:
    The interface file for bean_pprint.ml

analyze.ml:
    Getting through program to generate symbol table
    
symbol.ml:
    symbol table definition and util function
    
codegen.ml:
    generator code based syntax rule
    
type_checking.ml:
    catch syntax error before code generation
