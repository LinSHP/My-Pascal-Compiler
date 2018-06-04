//
// Created by 林世鹏 on 2018/6/2.
//
#include "AST.h"
#include "codegen.h"

using namespace std;

void CodeGenContext::generateCode(Program &root) {
    cout << "Generate Code...\n";

    vector<Type*> argTypes;
    FunctionType *functionType = FunctionType::get(Type::getVoidTy(llvmContext), makeArrayRef(argTypes), false);
    mainFunc = Function::Create(functionType, GlobalValue::InternalLinkage, "main", module);
    BasicBlock *basicBlock = BasicBlock::Create(llvmContext, "entry", mainFunc, nullptr);

    pushBlock(basicBlock);
}