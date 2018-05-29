%{
#include <iostream>
#include <TreeNode.h>
%}

%union{
    int int_value;
    double double_value;
    char* string_value;
}

%token <double_value> DOUBLE_LITERAL
%token <int_value> INT_LITERAL
%token <string_value>STRING_LITERAL
%token LP,RP,LB,RB,DOT,COMMA,COLON,UL,DIV,PLUS,MINUS,GE,GT,LE,LT,EQUAL,ASSIGN,SEMI

%%
stmt_list:
    stmt_list  stmt  SEMI  {}
    | ε {}
    ;
stmt:
    INTEGER COLON non_label_stmt {}
    | non_label_stmt {}
    ;
non_label_stmt:
    assign_stmt {}
    | proc_stmt {}
    | compound_stmt {}
    | if_stmt {}
    | repeat_stmt {}
    | while_stmt {}
    | for_stmt {}
    | case_stmt {}
    | goto_stmt {}
    ;
assign_stmt:
    ID  ASSIGN  expression
    | ID LB expression RB ASSIGN expression
    | ID  DOT  ID  ASSIGN  expression
    ;
proc_stmt: ID
    | ID  LP  args_list  RP
    | SYS_PROC
    | SYS_PROC  LP  expression_list  RP
    | READ  LP  factor  RP
    ;
if_stmt: 
    IF expression  THEN  stmt  else_clause
    ;
else_clause: 
    ELSE stmt 
    | ε
    ;
repeat_stmt: 
    REPEAT  stmt_list  UNTIL  expression
    ;
while_stmt: 
    WHILE  expression  DO stmt
    ;
for_stmt: 
    FOR  ID  ASSIGN  expression  direction  expression  DO stmt
    ;
direction: 
    TO 
    | DOWNTO
    ;
case_stmt: 
    CASE expression OF case_expr_list  END
    ;
case_expr_list: 
    case_expr_list  case_expr  
    |  case_expr
    ;
case_expr: 
    const_value  COLON  stmt  SEMI
    |  ID  COLON  stmt  SEMI
    ;
goto_stmt: 
    GOTO  INTEGER 
    ;
expression_list: 
    expression_list  COMMA  expression  
    |  expression
    ;
expression: 
    expression  GE  expr  {$$ = TreeNode($1, $3, GE)}
    | expression  GT  expr  {$$ = TreeNode($1, $3, GT)}
    | expression  LE  expr  {$$ = TreeNode($1, $3, LE)}
    | expression  LT  expr  {$$ = TreeNode($1, $3, LT)}
    | expression  EQUAL  expr  {$$ = TreeNode($1, $3, EQUAL)}
    | expression  UNEQUAL  expr  {$$ = TreeNode($1, $3, UNEQUAL)}  
    | expr
    ;
expr: 
    expr  PLUS  term  {$$ = TreeNode($1, $3, PLUS)}
    | expr  MINUS  term  {$$ = TreeNode($1, $3, MINUS)}
    | expr  OR  term  {$$ = TreeNode($1, $3, OR)}
    | term
    ;
term: 
    term  MUL  factor  {$$ = TreeNode($1, $3, MUL)}
    | term  DIV  factor  {$$ = TreeNode($1, $3, DIV)}
    | term  MOD  factor  {$$ = TreeNode($1, $3, MOD)}
    | term  AND  factor  {$$ = TreeNode($1, $3, AND)}
    | factor
    ;
factor: 
    NAME  
    | NAME  LP  args_list  RP  
    | SYS_FUNCT 
    | SYS_FUNCT  LP  args_list  RP  
    | const_value  
    | LP  expression  RP {$$ = $2}
    | NOT  factor  {$$ = TreeNode($1, NULL, NOT)}
    | MINUS  factor  {$$ = TreeNode($1, NULL, NOT)}
    | ID  LB  expression  RB 
    | ID  DOT  ID
    ;
args_list: 
    args_list  COMMA  expression  
    | expression
    ;

