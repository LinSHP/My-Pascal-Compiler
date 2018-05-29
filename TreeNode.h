#include <iostream>
#include <vector>
#include <string>

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
    DIV

}

struct DataType{
    int _int;
    string _string;
    double _double;
}

class TreeNode{
    vector<TreeNode> childNode;
    Operator nodeType;
    DataType data;

    TreeNode(TreeNode left, TreeNode right, OperatorType op){
        this.nodeType = op;
        this.childNode.push_back(left);
        this.childNode.push_back(right);
    }
}