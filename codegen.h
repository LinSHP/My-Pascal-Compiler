//
// Created by 林世鹏 on 2018/6/2.
//

#ifndef MY_PASCAL_CODEGEN_H
#define MY_PASCAL_CODEGEN_H

#include <stack>
#include <typeinfo>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/Support/raw_ostream.h>
#include "AST.h"
#include <llvm/IR/Constants.h>

using namespace llvm;

static LLVMContext llvmContext;

class CodeGenBlock {
public:
    BasicBlock *block;
    Value* returnValue;
    std::map<std::string, Value*> locals;
};

class CodeGenContext {
    std::stack<CodeGenBlock*> blocks;
    Function *mainFunc;
public:
    Module* module;
    CodeGenContext() {
        module = new Module("main", llvmContext);
    }
    void generateCode(Program& root);
    GenericValue runCode();
    std::map<std::string, Value*>& locals() {
        return blocks.top()->locals;
    };
    void pushBlock(BasicBlock *block) {
        blocks.push(new CodeGenBlock());
        blocks.top()->returnValue = NULL;
        blocks.top()->block = block;
    }
    void popBlock() {
        CodeGenBlock *top = blocks.top();
        blocks.pop();
        delete top;
    }
    void setCurrentReturnValue(Value *value) {
        blocks.top()->returnValue = value;
    }
    Value* getCurrentReturnValue() {
        return blocks.top()->returnValue;
    }
};


#endif //MY_PASCAL_CODEGEN_H
