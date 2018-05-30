%{
#include <iostream>
#include <TreeNode.h>

using namespace std;
%}

%union{
    int int_value;
    double double_value;
    char* string_value;
    char char_value;
    TreeNode treeNode;
}

%token <double_value> DOUBLE_LITERAL
%token <int_value> INT_LITERAL
%token <string_value>STRING_LITERAL ID
%token <char_value>CHAR_LITERAL
%token LP RP LB RB DOT COMMA COLON UL DIV PLUS MINUS GE GT LE LT
%token EQUAL ASSIGN SEMI ELSE  READ REPEAT THEN IF UNTIL NOT
%token ARRAY WHILE SYS_CON SYS_FUNCT MOD INTEGER PROGRAM RECORD
%token GOTO SYS_PROC FUNCTION CASE TO DOWNTO MUL PROCEDURE CONST DOTDOT
%token END OR OF TYPE DO UNEQUAL AND VAR BEGIN FOR SYS_TYPE
%token NAME //NAME应该属于type，这里先不实现

%type <treeNode> program procedure_head routine routine_head routine_part function_decl function_head
%type <treeNode> procedure_decl parameters para_decl_list para_type_list var_para_list val_para_list name_list
%type <treeNode> var_part var_decl_list var_decl routine_body label_part const_part const_expr_list type_part
%type <treeNode> type_decl_list type_definition type_decl record_type_decl field_decl_list field_decl array_type_decl
%type <treeNode> simple_type_decl assign_stmt if_stmt else_clause repeat_stmt while_stmt for_stmt expression_list
%type <treeNode> case_stmt case_expr_list case_expr expression expr term factor const_value  stmt stmt_list 

%%
program:
    program_head    routine DOT {int a = 1;}
    ;
program_head:
    PROGRAM ID  SEMI    {}
    ;
routine:
    routine_head    routine_body    {}
    ;
routine_head:
    label_part  const_part  type_part   var_part    routine_part    {}
    ;
routine_part:
    routine_part    function_decl   {}
    | routine_part  procedure_decl  {}
    | function_decl {}
    | procedure_decl    {}
    ;
function_decl:
    function_head   SEMI    routine SEMI    {}
    ;
function_head:
    FUNCTION    ID  parameters  COLON   simple_type_decl    {}
    ;
procedure_decl:
    procedure_head  SEMI    routine SEMI    {}
    ;
procedure_head:
    PROCEDURE   ID  parameters  {}
    ;
parameters:
    LP  para_decl_list  RP  {}
    |      {}
    ;
para_decl_list:
    para_decl_list  SEMI    para_type_list  {}
    | para_type_list    {}
    ;
para_type_list:
    var_para_list COLON simple_type_decl    {}
    | val_para_list COLON simple_type_decl  {}
    ;
var_para_list:
    VAR name_list   {};
    ;
val_para_list:
    name_list   {}
    ;
var_part:
    VAR var_decl_list   {}
    |  {}
    ;
var_decl_list:
    var_decl_list   var_decl    {}
    | var_decl  {}
    ;
var_decl:
    name_list   COLON   type_decl   SEMI    {}
    ;
routine_body:
    compound_stmt   {}
    ;
label_part:
       {}
    ;
const_part:
    CONST   const_expr_list {}
    |  {}
    ;
const_expr_list:
    const_expr_list NAME    EQUAL   const_value SEMI    {}
    | NAME EQUAL    const_value SEMI    {}
    ;
type_part:
    TYPE    type_decl_list  {}
    |  {}
    ;
type_decl_list:
    type_decl_list  type_definition {}
    | type_definition   {}
    ;
type_definition:
    NAME    EQUAL   type_decl   SEMI    {}
    ;
type_decl:
    simple_type_decl    {}
    | array_type_decl   {}
    | record_type_decl  {}
    ;
record_type_decl:
    RECORD  LB  simple_type_decl RB OF  type_decl   {}
    ;
field_decl_list:
    field_decl_list field_decl  {}
    | field_decl    {}
    ;
field_decl:
    name_list   COLON   type_decl   SEMI    {}
    ;
array_type_decl: 
    ARRAY   LB  simple_type_decl  RB  OF  type_decl   {$$ = TreeNode($3, $6, ARRAY)}
    ;
simple_type_decl:
    SYS_TYPE  {}  //这里的SYS_TYPE和NAME还是有问题的
    | NAME  {}
    | LP name_list RP   {$$ = $2}
    | const_value   DOTDOT  const_value {$$ = TreeNode($1, $3, BETWEEN)}
    | MINUS const_value DOTDOT const_value  {
                                            TreeNode temp(MINUS);
                                            temp->childNode.append($2);
                                            $$ = TreeNode(temp, $4, BETWEEN)
                                            }
    | MINUS const_value DOTDOT MINUS const_value {
                                            TreeNode temp(MINUS);
                                            temp->childNode.append($2);
                                            TreeNode temp2(MINUS);
                                            temp->childNode.append($5);
                                            $$ = TreeNode(temp1, temp2, BETWEEN)
                                            }
    | NAME  DOTDOT NAME {}
    ;
name_list:
    name_list   COMMA   ID  {}
    | ID    {}
    ;
compound_stmt:
    BEGIN   stmt_list   END {}
    ;
stmt_list:
    stmt_list  stmt  SEMI  {}
    |  {}
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
    ID  ASSIGN  expression {$$ = TreeNode($1, $3, ASSIGN);}
    | ID LB expression RB ASSIGN expression {}
    | ID  DOT  ID  ASSIGN  expression {}
    ;
proc_stmt: ID  {}
    | ID  LP  args_list  RP {}
    | SYS_PROC {}
    | SYS_PROC  LP  expression_list  RP {}
    | READ  LP  factor  RP {}
    ;
if_stmt: 
    IF expression  THEN  stmt  else_clause {$$ = TreeNode($2, $4, IF);
                                            $$->childNode.append($5)}
    ;
else_clause: 
    ELSE stmt {$$ = $2}
    | {}
    ;
repeat_stmt: 
    REPEAT  stmt_list  UNTIL  expression {$$ = TreeNode($2, $4, REPEAT)}
    ;
while_stmt: 
    WHILE  expression  DO stmt {$$ = TreeNode($2, $4, WHILE)}
    ;
for_stmt: 
    FOR  ID  ASSIGN  expression  direction  expression  DO stmt {
                                                                    $$ = TreeNode(For);
                                                                    $$->childNode.append($2);
                                                                    if (direction == TO){
                                                                        $$->childNode.append($4);
                                                                        $$->childNode.append($6);
                                                                    }
                                                                    else if (direction == DOWNTO){
                                                                        $$->childNode.append($6);
                                                                        $$->childNode.append($4);
                                                                    }
                                                                    $$->childNode.append($8);
                                                                    
                                                                }
    ;
direction: 
    TO {}
    | DOWNTO {}
    ;
case_stmt: 
    CASE expression OF case_expr_list  END {$$ = TreeNode($2, $4, SWITCH_CASE)}
    ;
case_expr_list: 
    case_expr_list  case_expr  {
                                    TreeNode temp = $1;
                                    if (temp->siblingNode!=NULL){
                                        temp=temp->siblingNode;
                                    }
                                    temp->siblingNode = $2
                                    $$ = $1
                                }
    |  case_expr {$$ = $1}
    ;
case_expr: 
    const_value  COLON  stmt  SEMI {$$ = TreeNode($1, $3, CASE)}
    |  ID  COLON  stmt  SEMI {$$ = TreeNode($1, $3, CASE)}
    ;
goto_stmt: 
    GOTO  INTEGER {}
    ;
expression_list: 
    expression_list  COMMA  expression  {}
    | expression {$$ = $1}
    ;
expression: 
    expression  GE  expr  {$$ = TreeNode($1, $3, GE)}
    | expression  GT  expr  {$$ = TreeNode($1, $3, GT)}
    | expression  LE  expr  {$$ = TreeNode($1, $3, LE)}
    | expression  LT  expr  {$$ = TreeNode($1, $3, LT)}
    | expression  EQUAL  expr  {$$ = TreeNode($1, $3, EQUAL)}
    | expression  UNEQUAL  expr  {$$ = TreeNode($1, $3, UNEQUAL)}  
    | expr {$$ = $1}
    ;
expr: 
    expr  PLUS  term  {$$ = TreeNode($1, $3, PLUS)}
    | expr  MINUS  term  {$$ = TreeNode($1, $3, MINUS)}
    | expr  OR  term  {$$ = TreeNode($1, $3, OR)}
    | term {$$ = $1}
    ;
term: 
    term  MUL  factor  {$$ = TreeNode($1, $3, MUL)}
    | term  DIV  factor  {$$ = TreeNode($1, $3, DIV)}
    | term  MOD  factor  {$$ = TreeNode($1, $3, MOD)}
    | term  AND  factor  {$$ = TreeNode($1, $3, AND)}
    | factor {$$ = $1}
    ;
factor: 
    NAME  {}
    | NAME  LP  args_list  RP  {}
    | SYS_FUNCT {}
    | SYS_FUNCT  LP  args_list  RP  {}
    | const_value  {}
    | LP  expression  RP {$$ = $2}
    | NOT  factor  {
                    $$ = TreeNode(NOT);
                    $$->childNode.append($2);
                    }
    | MINUS  factor  {
                    $$ = TreeNode(MINUS);
                    $$->childNode.append($2);
                    }
    | ID  LB  expression  RB {}
    | ID  DOT  ID {}
    ;
const_value:
    INT_LITERAL {$$ = TreeNode($1)}
    | DOUBLE_LITERAL    {$$ = TreeNode($1)}
    | STRING_LITERAL    {$$ = TreeNode($1)}
    | CHAR_LITERAL  {$$ = TreeNode($1)}
    ;
args_list: 
    args_list  COMMA  expression  {}
    | expression {}
    ;
%%

int yyerror(char const *str)
{
        extern char *yytext;
        fprintf(stderr, "parser error near %s\n", yytext);
        return 0;
}

int main(void)
{
        extern int yyparse(void);
        extern FILE *yyin;
        yyin = stdin;
        if (yyparse()) {
                fprintf(stderr, "Error!\n");
                exit(1);
        }
}

