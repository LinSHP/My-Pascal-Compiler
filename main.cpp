#include <iostream>
#include <stdexcept>

#include "AST.h"
#include "codegen.h"
#include "pascal.hpp"

using namespace std;

extern int yyparse();
extern Program* root;

int main(int argc, char* argv[]) {
    yyparse();
    cout << root->repr() << endl;

    CodeGenContext context;
    context.generateCode(*root);
    context.runCode();

    return 0;
}