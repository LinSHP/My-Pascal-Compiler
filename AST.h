#ifndef _AST_H
#define _AST_H
/*
    The Abstract Syntax Tree
*/
#include <string>
#include <map>
#include <utility>
#include <vector>
#include <iostream>
#include <queue>
//#include <llvm/IR/Value.h>
//#include <llvm/IR/Instructions.h>

using namespace std;

class ASTContext;
class Node;

class Expression;
class ExpressionList;
class Identifier;
class NameList;
class ConstValue;
class IntLiteral;
class DoubleLiteral;
class StringLiteral;
class CharLiteral;
class BoolLiteral;
class ArrayRef;
class RecordRef;
class FuncCall;
class UnaryOperator;
class BinaryOperator;

class Statement;
class StatementList;
class GotoStmt;
class CaseStmt;
class CaseStmtList;
class SwitchStmt;
class ForStmt;
class WhileStmt;
class RepeatStmt;
class IfStmt;
class ProcCall;
class AssignStmt;
class LabelStmt;
class TypeDecl;
class RangeType;
class ArrayType;
class RecordType;
class ConstDecl;
class ConstDeclList;
class VarDecl;
class VarDeclList;
class FieldDecl;
class FieldList;
class TypeDef;
class TypeDefList;
class LabelDecl;
class Routine;
class RoutineList;
class Program;

enum class Type : int{
    integer,
    real,
    string,
    character,
    boolean,
    range,
    array,
    record,
    userDefined
};

enum class OpType: int {
    plus, minus, mul, div, mod,
    eq, ne, lt, gt, le, ge,
    OP_AND, OP_OR, OP_NOT,
};

static map<OpType, string> opString = {
        {OpType::plus, "+"},
        {OpType::minus, "-"},
        {OpType::mul, "*"},
        {OpType::div, "/"},
        {OpType::mod, "%"},
        {OpType::eq, "="},
        {OpType::ne, "<>"},
        {OpType::lt, "<"},
        {OpType::gt, ">"},
        {OpType::le, "<="},
        {OpType::ge, ">="},
        {OpType::OP_AND, "and"},
        {OpType::OP_OR, "or"},
        {OpType::OP_NOT, "not"},
};

enum class RoutineType {
    function, procedure
};

// 纯虚，所有节点的根节点
class Node {
public:
    const string name;
    vector<Node*> children;
    virtual ~Node() = default;
    virtual string repr() = 0;
    // TODO: implement CodeGen
    // virtual llvm::Value *CodeGen(ASTContext& context) = 0;
    virtual void addChild(Node* node) {
        children.push_back(node);
    }
    virtual void printPascalTree(Node *tree){
        cout<<"now print tree"<<endl;
        if (tree == NULL)
            return;
        queue<Node *> nodeQueue;
        nodeQueue.push(tree);
        while (nodeQueue.size()){
            Node *nowNode = nodeQueue.front();
            cout<< nowNode->repr()<<endl;
            nodeQueue.pop();
            cout<<nowNode->children.size()<<endl;
            for (int i=0; i < nowNode->children.size(); i++){
                if (nowNode->children[i]!=NULL){
                    cout<<nowNode->children[i]->repr()<<endl;
                    nodeQueue.push(nowNode->children[i]);
                    cout<<nodeQueue.size()<<endl;
                }
                
            }
            cout<<endl;
            cout<<endl;
        }
    }
};



// Expression的根节点
class Expression: public Node {
public:
    string repr() override {
        return "expression";
    }
};

class ExpressionList: public Node {
public:
    explicit ExpressionList(Expression* expression) {
        addChild(expression);
    }

    string repr() override {
        return "expression list";
    }
};

class Identifier: public Expression {
public:
    const string name;

    explicit Identifier(string name): name(std::move(name)) {}
    explicit Identifier(const char* s): name(s) {}

    string repr() override {
        return name;
    }
};

class NameList: public Expression {
public:
    NameList(Identifier *identifier) {
        addChild(identifier);
    }

    string repr() override {
        return "name list";
    }
};

// ConstValue的根节点
class ConstValue: public Expression{
public:
    Type valueType;
};

class IntLiteral: public ConstValue{
public:
    const int value;

    explicit IntLiteral(int value): value(value) {
        valueType = Type::integer;
    }

    string repr() override {
        return "int: " + to_string(value);
    }
};

class DoubleLiteral: public ConstValue {
public:
    const double value;

    explicit DoubleLiteral(double value): value(value) {
        valueType = Type::real;
    }

    string repr() override  {
        return "double: " + to_string(value);
    }
};

class StringLiteral: public ConstValue {
public:
    const string literal;

    explicit StringLiteral(string literal): literal(std::move(literal)) {
        valueType = Type::string;
    }

    explicit StringLiteral(const char* s): literal(s) {
        valueType = Type::string;
    }

    string repr() override {
        return "string: " + literal;
    }
};

class CharLiteral: public ConstValue {
public:
    const char literal;

    explicit CharLiteral(const char* literal): literal(literal[0]) {
        valueType = Type::character;
    }

    string repr() override {
        return "char: " + string(1, literal);
    }
};

class RecordRef: public Expression {
public:
    Identifier *recordName, *fieldName;

    RecordRef(Identifier* recordName, Identifier* fieldName): recordName(recordName),
                                                              fieldName(fieldName) {
                                                                  addChild(recordName);
                                                                  addChild(fieldName);
                                                              }

    string repr() override {
        return recordName->repr() + "." + fieldName->repr();
    }
};

class ArrayRef: public Expression {
public:
    Identifier* arrayName;
    Expression* index;

    ArrayRef(Identifier* arrayName, Expression* index): arrayName(arrayName), index(index) {
        addChild(arrayName);
        addChild(index);
    }

    string repr() override {
        return arrayName->repr() + "[" + index->repr() + "]";
    }
};

class UnaryOperator: public Expression {
public:
    OpType op;
    Expression* operand;

    UnaryOperator(OpType op, Expression* operand): op(op), operand(operand) {
        addChild(operand);
    }

    string repr() override {
        return opString[op] + operand->repr();
    }
};

class BinaryOperator: public Expression {
public:
    OpType op;
    Expression *operand1, *operand2;

    BinaryOperator(OpType op, Expression* op1, Expression* op2): op(op), operand1(op1), operand2(op2) {
        addChild(operand1);
        addChild(operand2);
    }

    string repr() override {
        return operand1->repr() + opString[op] + operand2->repr();
    }
};

class FuncCall: public Expression {
public:
    Identifier* identifier;
    ExpressionList* args = nullptr;

    FuncCall(Identifier* id, ExpressionList* args): identifier(id), args(args) {
        addChild(identifier);
        addChild(args);
    }

    explicit FuncCall(Identifier* id): identifier(id) {
        addChild(identifier);
    }

    string repr() override {
        return identifier->repr() + "(" + args->children[0]->repr() + "...)";
    }
};

class Statement: public Node {
public:
    string repr() override {
        return "statement";
    }
};

class StatementList: public Statement {
public:
    explicit StatementList(Statement* statement) {
        addChild(statement);
    }

    StatementList() = default;

    string repr() override {
        return "statement list";
    }
};

class GotoStmt: public Statement {
public:
    int label;

    GotoStmt(int label): label(label) {}

    string repr() override {
        return "goto" + to_string(label);
    }
};

class CaseStmt: public Statement {
public:
    Expression* expression;
    Statement* statement;

    CaseStmt(Expression* ex, Statement* stmt): expression(ex), statement(stmt) {
        addChild(ex);
        addChild(stmt);
    }

    string repr() override {
        return "case " + expression->repr();
    }
};

class CaseStmtList: public Statement {
public:
    CaseStmtList(CaseStmt* caseStmt) {
        addChild(caseStmt);
    }

    string repr() override {
        return "case statement list";
    }
};

class SwitchStmt: public Statement {
public:
    Expression* expression;
    CaseStmtList* switchBody;

    SwitchStmt(Expression* ex, CaseStmtList* body): expression(ex), switchBody(body) {
        addChild(expression);
        addChild(switchBody);
    }

    string repr() override {
        return "switch statement";
    }
};

class ForStmt: public Statement {
public:
    Identifier* variable;
    Expression *left, *right;
    int direction; // 1 TO 0 DOWN

    ForStmt(Identifier* id, Expression* ex1, Expression* ex2, int direction):
            variable(id), left(ex1), right(ex2), direction(direction) {
                addChild(variable);
                addChild(left);
                addChild(right);
            }

    string repr() override {
        return "for statement";
    }
};

class WhileStmt: public Statement {
public:
    Expression* condition;
    Statement* statement;

    WhileStmt(Expression* ex, Statement* stmt): condition(ex), statement(stmt) {
        addChild(ex);
        addChild(stmt);
    }

    string repr() override {
        return "while statement";
    }
};

class RepeatStmt: public Statement {
public:
    StatementList* statementList;
    Expression* condition;

    RepeatStmt(StatementList* stmtList, Expression* ex): statementList(stmtList), condition(ex) {
        addChild(stmtList);
        addChild(ex);
    }

    string repr() override {
        return "repeat statement";
    }
};

class IfStmt: public Statement {
public:
    Expression* condition;
    Statement *ifBody, *elseBody;

    IfStmt(Expression* ex, Statement* ifBody, Statement* elseBody):
            condition(ex), ifBody(ifBody), elseBody(elseBody) {
                addChild(ex);
                addChild(ifBody);
                addChild(elseBody);
            }

    string repr() override {
        return "if statement";
    }
};

class ProcCall: public Statement {
public:
    Identifier *procName;
    ExpressionList *args;

    ProcCall(Identifier* name, ExpressionList* args): procName(name), args(args) {
        addChild(name);
        addChild(args);
    }

    string repr() override {
        return "procedure call";
    }
};

class AssignStmt: public Statement {
public:
    Expression *left, *right;
    int leftType;// 1 ID 2 Array 3 Record

    AssignStmt(Expression* left, Expression* right, int leftType):
            left(left), right(right), leftType(leftType) {
                addChild(left);
                addChild(right);
            }

    string repr() override {
        return left->repr() + " = " + right->repr();
    }
};

class LabelStmt: public Statement {
public:
    int label;
    Statement* statement;

    LabelStmt(int label, Statement* stmt): label(label), statement(stmt) {
        addChild(statement);
    }

    string repr() override {
        return to_string(label) + statement->repr();
    }
};

class TypeDecl: public Statement {
public:
    string typeName;
    RangeType* rangeType = nullptr;
    ArrayType* arrayType = nullptr;
    RecordType* recordType = nullptr;
    int type; //0 sysType or user defined type 1 rangeType 2 ArrayType 3 RecordType

    explicit TypeDecl(string typeName): typeName(std::move(typeName)) { type = 0; }
    explicit TypeDecl(const char* typeName): typeName(typeName) { type = 0; }
    explicit TypeDecl(RangeType* rangeType): rangeType(rangeType) { type = 1; }
    explicit TypeDecl(ArrayType* arrayType): arrayType(arrayType) { type = 2; }
    explicit TypeDecl(RecordType* recordType): recordType(recordType)  { type = 3; }

    string repr() override {
        if (type == 0)
            return "typedecl: " + typeName;
        else if (type == 1)
            return "range type";
        else if (type == 2)
            return "array type";
        else
            return "record type";
    }

};

class RangeType: public Statement {
public:
    int low, high;
    string lowStr, highStr;
    int type; // 0 int 1 str

    RangeType(int low, int high): low(low), high(high) {
        type = 0;
    }
    RangeType(string lowStr, string highStr): lowStr(std::move(lowStr)), highStr(std::move(highStr)) {
        low = high = -1;
        type = 1;
    }
    RangeType(const char* lowStr, const char* highStr): lowStr(lowStr), highStr(highStr) {
        low = high = -1;
        type = 1;
    }

    string repr() override {
        if (type == 0)
            return to_string(low) + "..." + to_string(high);
        else
            return lowStr + "..." + highStr;
    }
};

class ArrayType: public Statement {
public:
    TypeDecl *index, *arrayType;

    ArrayType(TypeDecl* index, TypeDecl* type): index(index), arrayType(type) {
        addChild(index);
        addChild(type);
    }

    string repr() override {
        return index->repr() + " " + arrayType->repr();
    }
};

class FieldDecl: public Statement {
public:
    NameList* fieldNames;
    TypeDecl* fieldType;

    FieldDecl(NameList* names, TypeDecl* type): fieldNames(names), fieldType(type) {
        addChild(names);
        addChild(type);
    }

    string repr() override {
        return fieldNames->repr() + " " + fieldType->repr();
    }
};

class FieldList: public Statement {
public:
    explicit FieldList(FieldDecl* field) {
        addChild(field);
    }

    string repr() override {
        return "field List";
    }
};

class RecordType: public Statement {
public:
    FieldList* fields;

    explicit RecordType(FieldList* fields): fields(fields) {
        addChild(fields);
    }

    string repr() override {
        return "record Type";
    }
};

class TypeDef: public Statement {
public:
    Identifier* typeName;
    TypeDecl* type;

    TypeDef(Identifier* id, TypeDecl* type): typeName(id), type(type) {
        addChild(typeName);
        addChild(type);
    }

    string repr() override {
        return typeName->repr() + ": " + type->repr();
    }
};

class TypeDefList: public Statement {
public:
    explicit TypeDefList(TypeDef* typeDef) {
        addChild(typeDef);
    }

    TypeDefList() = default;

    string repr() override {
        return "type def list";
    }
};

class ConstDecl: public Statement {
public:
    Identifier* name;
    ConstValue* value;

    ConstDecl(Identifier* name, ConstValue* value): name(name), value(value) {
        addChild(name);
        addChild(value);
    }

    string repr() override {
        return "const " + name->repr() + " " + value->repr();
    }
};

class ConstDeclList: public Statement {
public:

    ConstDeclList() = default;
    explicit ConstDeclList(ConstDecl* constDecl) {
        addChild(constDecl);
    }

    string repr() override {
        return "const decl List";
    }
};

class LabelDecl: public Statement {
public:
    LabelDecl() = default;
    string repr() override {
        return "type decl";
    }
};

class VarDecl: public Statement {
public:
    NameList* names;
    TypeDecl* type;

    VarDecl(NameList* names, TypeDecl* type): names(names), type(type) {
        addChild(names);
        addChild(type);
    }

    string repr() override {
        return "var " + names->repr() + " " + type->repr();
    }
};

class VarDeclList: public Statement {
public:
    explicit VarDeclList(VarDecl* varDecl) {
        addChild(varDecl);
    }

    VarDeclList() = default;

    string repr() override {
        return "var decl list";
    }
};

class Program: public Node {
public:
    LabelDecl* labelPart;
    ConstDeclList* constPart;
    TypeDefList* typePart;
    VarDeclList* varPart;
    StatementList* routineBody;
    RoutineList* routineList;

    Program() = default;
    Program(LabelDecl* labelPart, ConstDeclList* constPart, TypeDefList* typePart,
            VarDeclList* varPart, RoutineList* routineList, StatementList* routineBody):
            labelPart(labelPart), constPart(constPart), typePart(typePart), varPart(varPart),
            routineBody(routineBody), routineList(routineList) {
                addChild(labelPart);
                addChild(constPart);
                addChild(typePart);
                addChild(varPart);
                addChild((Node *)routineList);
            }

    string repr() override {
        return "Program";
    }
};

class Routine: public Program {
public:
    RoutineType routineType;
    Identifier* routineName;
    TypeDecl* returnType;
    VarDeclList* args;

    Routine(RoutineType routineType, Identifier* id, VarDeclList* args, TypeDecl* returnType):
            routineType(routineType), routineName(id), returnType(returnType), args(args) {
                addChild(routineName);
                addChild(returnType);
                addChild(args);
            }

    Routine(Routine* head, Program *routine): Program(*routine), routineName(head->routineName),
                                              routineType(head->routineType), returnType(head->returnType),
                                              args(head->args) {
                                                addChild(routineName);
                                                addChild(returnType);
                                                addChild(args);
                                              }

    string repr() override {
        return "routine";
    }
};

class RoutineList: public Node {
public:
    explicit RoutineList(Routine* routine) {
        addChild(routine);
    }

    RoutineList() = default;

    string repr() override {
        return "routine list";
    }
};



#endif