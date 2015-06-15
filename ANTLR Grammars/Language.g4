grammar Language;

import base, Keywords;

@header{package antlr;}

program: line+;

line: FUNC STR LPAR STRING? RPAR LCBR block RCBR
    | VAR STR BE (INT | BOOL | STR)
    | STRING
    ;

block: IF LPAR STR RPAR LCBR block RCBR
     | STR
     ;

test : STRP ;