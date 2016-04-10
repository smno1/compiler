
With this README file you should also have found these:

Makefile:  
    A makefile for the COMP90045 project 2016

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
    A pretty-printer - well, not really; for now this is just a stub

bean_pprint.mli:
    The interface file for bean_pprint.ml

To get started, study these files, in particular bean_ast.ml,
bean_lex.mll, and bean_parse.mly.  On a Unix machine you should 
be able to just type

    make

and that should generate some files for you, including the executable
bean. 

Write a small bean program, like this:

int mung;
read mung;
mung := 2*mung + 42;
write mung;

Say this program is in file mung.bean; now you should be able to run

    bean -p mung.bean

and something will happen (actually nothing very interesting, since
there is no real pretty-printer yet).  But at least you should not 
get error messages.

If your Unix system doesn't seem to recognise `bean', that
could be because your PATH variable hasn't been set correctly.
For now, just try instead

    ./bean -p mung.bean

