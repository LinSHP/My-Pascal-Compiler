#include <iostream>
#include <stdlib.h>
#include "y.tab.h"

int yywrap(void)
{
    return 1;
}

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
">"     return GT;
"<="    return LE;
"<"     return LT;
"="     return EQUAL;
"!="    return UNEQUAL;
"<"     return ASSIGN;
"~"     return NOT;
";"     return SEMI;
"begin" return BEGIN;
"end"   return END:
"var"   return VAR;
"boolean"   return BOOLEAN;
"char"  return CHAR;
"integer" return INTEGER;
"real"  return REAL;
"false" return FALSE;
"true"  return TRUE;
"maxint"    return MAXINT;
"type"  return TYPE;

[0-9]+ {
    yylval.int_value = atoi(yytext);
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
    yylval.char_value = yytext;
    return CHAR_LITERAL;
}

"function" return FUNCTION;

[ \t] ;
. {
    fprintf(stderr, "lexical error.\n");
    exit(1);
}
%%