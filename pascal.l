%{
#include "AST.h"
#include <iostream>
#include <stdlib.h>
#include "pascal.hpp"

extern "C"
{
    int yywrap(void);
    int yylex(void);
}
%}

%%
"("     return LP;
")"     return RP;
"["     return LB;
"]"     return RB;
"."     return DOT;
".."    return DOTDOT;
","     return COMMA;
":"     return COLON;
"*"     return MUL;
"/"     return DIV;
"+"     return PLUS;
"-"     return MINUS;
">="    return GE;
">"     {return GT;}
"<="    return LE;
"<"     return LT;
"="     return EQUAL;
"!="    return UNEQUAL;
":="     return ASSIGN;
"~"     return NOT;
";"     return SEMI;
"begin" return START;
"end"   return END;
"var"   return VAR;
"boolean"   return BOOLEAN;
"char"  return CHAR;
"integer" return INTEGER;
"real"  return REAL;
"false" return FALSE;
"true"  return TRUE;
"maxint"    return MAXINT;
"type"  return TYPE;
"to"    return TO;
"downto"    return DOWNTO;
"else"  return ELSE;
"read"  return READ;
"repeat"    return REPEAT;
"then"  return THEN;
"until" return UNTIL;
"program" return PROGRAM;
"if"    return IF;

[a-zA-Z]([a-zA-Z0-9_])* {
	yylval.string_value = yytext;
	// cout << yytext << endl;
    return ID;
}

[0-9]+ {
    yylval.int_value = atoi(yytext);
    // cout<<"int"<<endl;
    return INT_LITERAL;

}

([1-9][0-9]*)|0|([0-9]+\.[0-9]+) {
    yylval.double_value = atof(yytext);
    return DOUBLE_LITERAL;
}

\"(\\.|[^"\\])*\" {
    yylval.string_value = yytext;
    return STRING_LITERAL;
}

\'(.)\' {
    yylval.string_value = yytext;
    return CHAR_LITERAL;
}

"function" return FUNCTION;

[ \t] ;
. {
    fprintf(stderr, "lexical error.\n");
    exit(1);
}

"boolean"|"char"|"integer"|"real"|"longint"    {return SYS_TYPE;}
%%

int yywrap(void) {
    return 1;
}