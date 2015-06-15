lexer grammar base;

INT:    [0-9]+ ;
BOOL:   'Aye' | 'Nay' ;
STR:    [a-zA-Z]+ ;
STRP:   ~('"')+ ;
STRING: '"' STRP '"' ;
ARRAY:  STR '[' INT? ']' ;

INTS:   'int' | 'integer' ;
BOOLS:  'bool' | 'boolean' ;
STRS:   'str' | 'string' ;
ARRAYS: 'arr' | 'array' ;

LPAR:   '(' ;
RPAR:   ')' ;
LBRA:   '[' ;
RBRA:   ']' ;
LCBR:   '{' ;
RCBR:   '}' ;
MINUS:  '-' ;
PLUS:   '+' ;
TIMES:  '*' ;
DIV:    '/' ;
EQ:     '=' ;
LT:     '<' ;
GT:     '>' ;
BE:     'be' ;

WS : [ \t\n\r] -> skip;