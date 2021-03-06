%{
#include "AST.h"
#include <iostream>
#include "pascal.hpp"

using namespace std;

extern "C" {
    int yyerror(const char*);
    extern int yylex(void);
}
Node *tree;
Program* root;
%}

%union{
    int int_value;
    double double_value;
    char* string_value;
    ConstValue* AST_ConstValue;
    Expression* AST_Expression;
    ExpressionList* AST_ExpressionList;
    Statement* AST_Statement;
    StatementList* AST_StatementList;
    AssignStmt* AST_AssignStmt;
    IfStmt* AST_IfStmt;
    CaseStmt* AST_CaseStmt;
    CaseStmtList* AST_CaseStmtList;
    NameList* AST_NameList;
    TypeDecl* AST_TypeDecl;
    FieldDecl* AST_FieldDecl;
    FieldList* AST_FieldList;
    TypeDef* AST_TypeDef;
    TypeDefList* AST_TypeDefList;
    ConstDeclList* AST_ConstDeclList;
    LabelDecl* AST_LabelDecl;
    VarDecl* AST_VarDecl;
    VarDeclList* AST_VarDeclList;
    Routine* AST_Routine;
    RoutineList* AST_RoutineList;
    Program* AST_Program;
}

%token MY_ID
%token <double_value> DOUBLE_LITERAL
%token <int_value> INT_LITERAL FALSE TRUE MAXINT
%token <string_value> MY_ID STRING_LITERAL
%token <string_value> CHAR_LITERAL BOOL_LITERAL
%token MY_LP RP LB RB DOT COMMA COLON UL DIV PLUS MINUS GE GT MY_LE MY_LT
%token EQUAL ASSIGN SEMI ELSE REPEAT THEN IF UNTIL NOT
%token ARRAY WHILE SYS_CON MOD INTEGER PROGRAM RECORD
%token GOTO FUNCTION CASE DOWNTO TO MUL PROCEDURE CONST DOTDOT
%token END OR OF TYPE DO UNEQUAL AND VAR START FOR CHAR REAL BOOLEAN
%token <string_value> SYS_TYPE SYS_PROC READ SYS_FUNCT

%type <string_value> NAME
%type <AST_ConstValue> const_value
%type <AST_Expression> factor term expr expression
%type <AST_ExpressionList> args_list expression_list
%type <AST_Statement> goto_stmt case_stmt for_stmt while_stmt repeat_stmt if_stmt proc_stmt
%type <AST_Statement> non_label_stmt stmt
%type <AST_AssignStmt> assign_stmt
%type <AST_IfStmt> else_clause
%type <AST_CaseStmt> case_expr
%type <AST_CaseStmtList> case_expr_list
%type <AST_StatementList> stmt_list compound_stmt routine_body
%type <AST_NameList> name_list val_para_list var_para_list
%type <AST_TypeDecl> simple_type_decl array_type_decl record_type_decl type_decl
%type <AST_FieldDecl> field_decl
%type <AST_FieldList> field_decl_list
%type <AST_TypeDef> type_definition
%type <AST_TypeDefList> type_decl_list type_part
%type <AST_ConstDeclList> const_expr_list const_part
%type <AST_LabelDecl> label_part
%type <AST_VarDecl> var_decl para_type_list
%type <AST_VarDeclList> var_decl_list var_part para_decl_list parameters
%type <AST_Routine> procedure_head procedure_decl function_head function_decl
%type <AST_RoutineList> routine_part
%type <AST_Program> routine_head routine program_head program

%%
program: program_head    routine DOT {$$ = $2;tree=$$; root=$$;}
    ;
program_head:
    PROGRAM MY_ID  SEMI    {}
    | {}
    ;
routine:
    routine_head    routine_body    {
        $$ = $1;
        $$->routineBody = $2;
        $$->addChild($2);
    }
    ;
routine_head:
    label_part  const_part  type_part   var_part    routine_part    {
        $$ = new Program($1, $2, $3, $4, $5, nullptr);
    }
    ;
routine_part:
    routine_part    function_decl   { $$->addChild($2);}
    | routine_part  procedure_decl  { $$->addChild($2);}
    | { $$ = new RoutineList();}
    ;
function_decl:
    function_head   SEMI    routine SEMI    {
        $$ = new Routine($1, $3);
    }
    ;
function_head:
    FUNCTION    MY_ID  parameters  COLON   simple_type_decl    {
        $$ = new Routine(RoutineType::function, new Identifier($2), $3, $5);

    }
    ;
procedure_decl:
    procedure_head  SEMI    routine SEMI    {
        $$ = new Routine($1, $3);
    }
    ;
procedure_head:
    PROCEDURE   MY_ID  parameters  {
        $$ = new Routine(RoutineType::procedure, new Identifier($2), $3, nullptr);
    }
    ;
parameters:
    MY_LP  para_decl_list  RP  { $$ = $2; }
    |      { $$ = new VarDeclList(); }
    ;
para_decl_list:
    para_decl_list  SEMI    para_type_list  { $$->addChild($3); }
    | para_type_list    { $$ = new VarDeclList($1); }
    ;
para_type_list:
    var_para_list COLON simple_type_decl    {
        $$ = new VarDecl($1, $3);
    }
    | val_para_list COLON simple_type_decl  {
        $$ = new VarDecl($1, $3);
    }
    ;
var_para_list:
    VAR name_list   { $$ = $2; };
    ;
val_para_list:
    name_list   { $$ = $1; }
    ;
var_part:
    VAR var_decl_list   { $$ = $2; }
    |  { $$ = new VarDeclList(); }
    ;
var_decl_list:
    var_decl_list   var_decl    { $$->addChild($2); }
    | var_decl  { $$ = new VarDeclList($1); }
    ;
var_decl:
    name_list   COLON   type_decl   SEMI    {
        $$ = new VarDecl($1, $3);
    }
    ;
routine_body:
    compound_stmt   { $$ = $1; }
    ;
label_part:
    { $$ = new LabelDecl(); }
    ;
const_part:
    CONST   const_expr_list { $$ = $2; }
    |  { $$ = new ConstDeclList(); }
    ;
const_expr_list:
    const_expr_list NAME    EQUAL   const_value SEMI    {
        $$->addChild(new ConstDecl(new Identifier($2), $4));
    }
    | NAME EQUAL    const_value SEMI    {
        $$ = new ConstDeclList(new ConstDecl(new Identifier($1), $3));
    }
    ;
type_part:
    TYPE    type_decl_list  { $$ = $2; }
    |  { $$ = new TypeDefList(); }
    ;
type_decl_list:
    type_decl_list  type_definition { $$->addChild($2); }
    | type_definition   { $$ = new TypeDefList($1); }
    ;
type_definition:
    NAME    EQUAL   type_decl   SEMI    {
        $$ = new TypeDef(new Identifier($1), $3);
    }
    ;
type_decl:
    simple_type_decl    { $$ = $1; }
    | array_type_decl   { $$ = $1; }
    | record_type_decl  { $$ = $1; }
    ;
record_type_decl:
    RECORD  field_decl_list  END   { $$ = new TypeDecl(new RecordType($2)); }
    ;
field_decl_list:
    field_decl_list field_decl  { $$->addChild($2); }
    | field_decl    { $$ = new FieldList($1); }
    ;
field_decl:
    name_list   COLON   type_decl   SEMI    {
        $$ = new FieldDecl($1, $3);
    }
    ;
array_type_decl: 
    ARRAY   LB  simple_type_decl  RB  OF  type_decl   {
        $$ = new TypeDecl(new ArrayType($3, $6));
    }
    ;
simple_type_decl:
    SYS_TYPE  {
        $$ = new TypeDecl($1);
    }  //这里的SYS_TYPE和NAME还是有问题的
    | NAME  { $$ = new TypeDecl($1); }
    | MY_LP name_list RP   {} //enum 先不写
    | INT_LITERAL   DOTDOT  INT_LITERAL { $$ = new TypeDecl(new RangeType($1, $3)); }
    | MINUS INT_LITERAL DOTDOT INT_LITERAL  { $$ = new TypeDecl(new RangeType(-$2, $4)); }
    | MINUS INT_LITERAL DOTDOT MINUS INT_LITERAL { $$ = new TypeDecl(new RangeType(-$2, -$5)); }
    | NAME  DOTDOT NAME { $$ = new TypeDecl(new RangeType($1, $3)); }
    ;
name_list:
    name_list   COMMA   MY_ID  { $$->addChild(new Identifier($3)); }
    | MY_ID    { $$ = new NameList(new Identifier($1)); }
    ;
compound_stmt:
    START   stmt_list   END { $$ = $2;}
    ;
stmt_list:
    stmt_list  stmt  SEMI  { $$->addChild($2); }
    |  { $$ = new StatementList(); }
    ;
stmt:
    INT_LITERAL COLON non_label_stmt { $$ = new LabelStmt($1, $3); }
    | non_label_stmt { $$ = $1; }
    ;
non_label_stmt:
    assign_stmt { $$ = $1;}
    | proc_stmt { $$ = $1; }
    | compound_stmt { $$ = $1; }
    | if_stmt { $$ = $1; }
    | repeat_stmt { $$ = $1; }
    | while_stmt { $$ = $1; }
    | for_stmt { $$ = $1; }
    | case_stmt { $$ = $1; }
    | goto_stmt { $$ = $1; }
    ;
assign_stmt:
    MY_ID  ASSIGN  expression {
        $$ = new AssignStmt(new Identifier($1), $3);
    }
    | MY_ID LB expression RB ASSIGN expression {
        $$ = new AssignStmt(new MyArrayRef(new Identifier($1), $3), $6);
    }
    | MY_ID  DOT  MY_ID  ASSIGN  expression {
        $$ = new AssignStmt(new RecordRef(new Identifier($1), new Identifier($3)), $5);
    }
    ;
proc_stmt: MY_ID  {}
    | MY_ID  MY_LP  args_list  RP { $$ = new ProcCall(new Identifier($1), $3); }
    | SYS_PROC { $$ = new ProcCall(new Identifier($1), nullptr); }
    | SYS_PROC  MY_LP  expression_list  RP { $$ = new ProcCall(new Identifier($1), $3); }
    | READ  MY_LP  factor  RP { $$ = new ProcCall(new Identifier($1), new ExpressionList($3)); }
    ;
if_stmt: 
    IF expression  THEN  stmt  else_clause {
        $5->condition = $2;
        $5->ifBody = $4;
        $5->addChild($2);
        $5->addChild($4);
        $$ = $5;
    }
    ;
else_clause:
    ELSE stmt { $$ = new IfStmt(nullptr, nullptr, $2); }
    ;
repeat_stmt: 
    REPEAT  stmt_list  UNTIL  expression { $$ = new RepeatStmt($2, $4); }
    ;
while_stmt: 
    WHILE  expression  DO stmt { $$ = new WhileStmt($2, $4); }
    ;
for_stmt:
    FOR  MY_ID  ASSIGN  expression  TO  expression  DO stmt {
        $$ = new ForStmt(new Identifier($2), $4, $6, 1, $8);
    }
    | FOR  MY_ID  ASSIGN  expression  DOWNTO  expression  DO stmt {
        $$ = new ForStmt(new Identifier($2), $4, $6, 0, $8);
    }
    ;
case_stmt: 
    CASE expression OF case_expr_list  END { $$ = new SwitchStmt($2, $4); }
    ;
case_expr_list: 
    case_expr_list  case_expr  { $$->addChild($2); }
    |  case_expr { $$ = new CaseStmtList($1); }
    ;
case_expr: 
    const_value  COLON  stmt  SEMI { $$ = new CaseStmt($1, $3); }
    |  MY_ID  COLON  stmt  SEMI { $$ = new CaseStmt(new Identifier($1), $3); }
    ;
goto_stmt: 
    GOTO  INT_LITERAL { $$ = new GotoStmt($2); }
    ;
expression_list: 
    expression_list  COMMA  expression  { $$->addChild($3); }
    | expression { $$ = new ExpressionList($1); }
    ;
args_list: 
    args_list  COMMA  expression  { $$->addChild($3); }
    | expression { $$ = new ExpressionList($1); }
    ;
expression: 
    expression  GE  expr  {$$ = new MyBinaryOperator(OpType::ge, $1, $3); }
    | expression  GT  expr  {$$ = new MyBinaryOperator(OpType::gt, $1, $3); }
    | expression  MY_LE  expr  {$$ = new MyBinaryOperator(OpType::le, $1, $3); }
    | expression  MY_LT  expr  {$$ = new MyBinaryOperator(OpType::lt, $1, $3); }
    | expression  EQUAL  expr  {$$ = new MyBinaryOperator(OpType::eq, $1, $3); }
    | expression  UNEQUAL  expr  {$$ = new MyBinaryOperator(OpType::ne, $1, $3); }
    | expr { $$ = $1; }
    ;
expr: 
    expr  PLUS  term  {$$ = new MyBinaryOperator(OpType::plus, $1, $3);}
    | expr  MINUS  term  {$$ = new MyBinaryOperator(OpType::minus, $1, $3); }
    | expr  OR  term  {$$ = new MyBinaryOperator(OpType::OP_OR, $1, $3); }
    | term { $$ = $1; }
    ;
term: 
    term  MUL  factor  {$$ = new MyBinaryOperator(OpType::mul, $1, $3); }
    | term  DIV  factor  {$$ = new MyBinaryOperator(OpType::div, $1, $3); }
    | term  MOD  factor  {$$ = new MyBinaryOperator(OpType::mod, $1, $3); }
    | term  AND  factor  {$$ = new MyBinaryOperator(OpType::OP_AND, $1, $3); }
    | factor { $$ = $1; }
    ;
factor: 
    MY_ID  {
        $$ = new Identifier($1);
    }
    | NAME  MY_LP  args_list  RP  { $$ = new FuncCall(new Identifier($1), $3); }
    | SYS_FUNCT { $$ = new FuncCall(new Identifier($1)); }
    | SYS_FUNCT  MY_LP  args_list  RP  { $$ = new FuncCall(new Identifier($1), $3); }
    | const_value  { $$ = $1; }
    | MY_LP  expression  RP { $$ = $2; }
    | NOT  factor  { $$ = new UnaryOperator(OpType::OP_NOT, $2); }
    | MINUS  factor  {
                    $$ = new UnaryOperator(OpType::minus, $2);
                    }
    | MY_ID  LB  expression  RB { $$ = new MyArrayRef(new Identifier($1), $3); }
    | MY_ID  DOT  MY_ID {$$ = new RecordRef(new Identifier($1), new Identifier($3)); }
    ;
const_value:
    INT_LITERAL { $$ = new IntLiteral($1);}
    | DOUBLE_LITERAL    { $$ = new DoubleLiteral($1);}
    | CHAR_LITERAL  { $$ = new CharLiteral($1); }
    | BOOL_LITERAL  { $$ = new BoolLiteral($1); }
    ;
NAME:
    MY_ID { $$ = $1; }
    ;
%%

int yyerror(char const *str)
{
        extern char *yytext;
        fprintf(stderr, "parser error near %s, %s\n", yytext, str);
        return 0;
}

void printPascalTree(Node *tree){
    cout<<"now print tree"<<endl;
    if (tree == NULL)
        return;
    queue<Node *> nodeQueue;
    nodeQueue.push(tree);
    while (nodeQueue.size()){
        Node *nowNode = nodeQueue.front();
        cout<< nowNode->repr()<<"-> ";
        nodeQueue.pop();
        for (int i=0; i < nowNode->children.size(); i++){
            if (nowNode->children[i]!=NULL){
                cout<<nowNode->children[i]->repr()<<" ";
                nodeQueue.push(nowNode->children[i]);
            }

        }
        cout<<endl;
    }
}

string printTree2(Node *tree, int indent){
    string res="";
    if (tree!=NULL){
        for (int i=0; i<indent;i++){
            res+='\t';
        }
        if (tree->children.size()==0){
            res+="\""+tree->repr()+"\":{}\n";
            return res;
        }
        res+="\""+tree->repr()+"\":{\n";
        // cout<<tree->repr()<<endl;
        for (int i=0; i < tree->children.size(); i++){
            if (tree->children[i]!=NULL){
                if (i!=tree->children.size()-1)
                    res+=printTree2(tree->children[i], indent+1)+",";
                else
                    res+=printTree2(tree->children[i], indent+1);
            }
        }
        res+="}";
    }
    return res;
}


void printTreet(Node *tree, int indent){
    if (tree!=NULL){
        // cout<<tree->children.size();
        for (int i=0; i<indent;i++){
            cout<<'\t';
        }
        cout<<tree->repr()<<endl;
        for (int i=0; i < tree->children.size(); i++){
            // cout<<"1 ";
            if (tree->children[i]!=NULL){
                printTreet(tree->children[i], indent+1);
            }
        }

    }
}



