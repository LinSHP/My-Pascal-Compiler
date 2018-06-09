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
"("     return MY_LP;
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
"<="    return MY_LE;
"<"     return MY_LT;
"="     return EQUAL;
"!="    return UNEQUAL;
":="     { cout << yytext << endl; return ASSIGN; }
"~"     return NOT;
";"     return SEMI;
"begin" return START;
"end"   return END;
"var"   return VAR;
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
"function" return FUNCTION;
"boolean"|"char"|"integer"|"real"|"longint"    { yylval.string_value = strdup(yytext); return SYS_TYPE;}

([_a-zA-Z])([_a-zA-Z0-9])* {
	yylval.string_value = strdup(yytext);
	cout << yytext << endl;
    return MY_ID;
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
    yylval.string_value = strdup(yytext);
    return STRING_LITERAL;
}

\'(.)\' {
    yylval.string_value = strdup(yytext);
    return CHAR_LITERAL;
}

[ \t] ;
. {
    fprintf(stderr, "lexical error.\n");
    exit(1);
}

%%

int yywrap(void) {
    return 1;
}