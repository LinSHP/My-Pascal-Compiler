#ifndef TREE_NODE
#define TREE_NODE
#include <iostream>
#include <vector>
#include <string>
#include <stdio.h>

using namespace std;

enum OperatorType{
    GE,
    GT,
    LE,
    LT,
    PLUS,
    MINUS,
    OR,
    AND,
    MUL,
    DIV,
    BETWEEN,     //1..2这种区间
    SWITCH_CASE, //case中的 'case 表达式 of'
    CASE,       //case中的 '常量:语句'
    FOR,
    WHILE

};

enum DataType{
    INT,
    STRING,
    DOUBLE,
    CHAR
};

struct DataValue{
    int _int;
    string _string;
    double _double;
    char _char;
};

class TreeNode{
public:
    vector<TreeNode> childNode;
    TreeNode* siblingNode = NULL;
    OperatorType nodeType;
    DataType dataType;
    DataValue dataValue;

    TreeNode(OperatorType op){
        this->nodeType = op;
    }

    TreeNode(TreeNode left, TreeNode right, OperatorType op){
        this->nodeType = op;
        this->childNode.push_back(left);
        this->childNode.push_back(right);
    }

    TreeNode(int data){                     //其实以下的构造方法都是没必要的
        this->dataType = INT;
        this->dataValue._int =  data;
    }

    TreeNode(string data){
        this->dataType = STRING;
        this->dataValue._string =  data;
    }
    TreeNode(double data){
        this->dataType = DOUBLE;
        this->dataValue._double =  data;
    }
    TreeNode(char data){
        this->dataType = CHAR;
        this->dataValue._char = data;
    }
};

#endif