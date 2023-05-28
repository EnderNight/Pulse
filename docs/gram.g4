grammar gram;


prog
	: (statement)+ EOF
	;


function
	: FUN refName '(' ARGS ')' DEF (statement)+ END
	;



statement
	: expr
	| if
	| while
	;


while
	: WHILE boolExpr DO (statement)+ END
	;


if
	: IF boolExpr DO (statement)+ (END|else)
	;

else
	: ELSE (statement)+ END
	;


boolExpr
	: expr B_EQ expr
	;


expr
	: multExpr ((PLUS|MINUS) multExpr)*
	| KEYWORD:'var' IDENTIFIER EQ expr
	;

multExpr
	: powExpr ((MULT|DIV|MOD) powExpr)*
	;

factor
	: (PLUS|MINUS) factor
	| powExpr
	;

powExpr
	: atom (POW factor)*
	;

atom
	: number
	| LPAREN expr RPAREN
	| bool
	;




PLUS : '+';
MINUS : '-';
MULT : '*';
DIV : '/';
MOD : '%';
POW : '^';
LPAREN : '(';
RPAREN : ')';
B_EQ : '==';
EQ : '=';


/* KEYWORD */
IF : 'if';
ELSE : 'else';
DO : 'do';
END : 'end';

WHILE : 'while';

FUN : 'fun';
DEF : 'def';

VAR : 'var';


ARGS
	: refName (',' refName)*
	;


refName
	: VALID_START VALID_CHAR*
	;


fragment VALID_START
	: 'a'..'z' | 'A'..'Z' | '_'
	;

fragment VALID_CHAR
	: VALID_START
	| '0'..'9'
	;


bool
	: 'True'
	| 'False'
	;

number
	: INT
	| FLOAT
	;

FLOAT
	: INT ('.' INT)?
	;

INT
	: '0'..'9'+
	;


WS : [ \t\r\n]+ -> skip;
