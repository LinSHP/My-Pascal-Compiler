#include <iostream>
#include <stdexcept>

#include "AST.h"
#include "codegen.h"
#include "pascal.hpp"

using namespace std;

extern int yyparse();
extern Program* root;
string printTree2(Node *tree, int indent);

int main(int argc, char* argv[]) {
    yyparse();
    cout<<"{"+printTree2(root,0)+"}";

    CodeGenContext context;
    context.generateCode(*root);
    context.runCode();

    return 0;
}