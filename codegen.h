//
// Created by 林世鹏 on 2018/6/2.
//

#ifndef MY_PASCAL_CODEGEN_H
#define MY_PASCAL_CODEGEN_H

#include <stack>
#include <typeinfo>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/ValueSymbolTable.h>
#include <llvm/IR/Instructions.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/Support/raw_ostream.h>
#include "AST.h"
#include <llvm/IR/Constants.h>

using namespace llvm;

static LLVMContext llvmContext;

class CodeGenBlock {
public:
    BasicBlock *block;
    Value *returnValue;
    CodeGenBlock *parent;
    std::map<std::string, Value *> locals;
    std::map<std::string, ConstValue *> const_locals;
};

class CodeGenContext {
    std::stack<CodeGenBlock *> blocks;

public:
    static std::vector<int> labels;
    std::map<Function *, Function *> parent;
    llvm::BasicBlock *labelBlock[10000];
    Function *currentFunction;
    Function *mainFunction;
    static llvm::Function *printf;
    Module *module;

    CodeGenContext() { module = new Module("main", llvmContext); }

    void generateCode(Program &root);

    GenericValue runCode();

    Value *getValue(const std::string &name) {
        std::cout << "Start getValue for " << name << std::endl;
        //currentFunction->getValueSymbolTable().dump();
        std::cout << "found:" << currentFunction->getValueSymbolTable()->lookup(name);
        std::cout << "main:" << mainFunction << "\n";
        std::cout << "current:" << currentFunction << "\n";
        llvm::Function *nowFunc = currentFunction;
        if ((nowFunc->getValueSymbolTable()->lookup(name)) == nullptr) {

            if (module->getGlobalVariable(name) == NULL) {
                throw std::logic_error("Undeclared variable " + name);
            }
            return module->getGlobalVariable(name);
        }
        return nowFunc->getValueSymbolTable()->lookup(name);
        // return nowBlock->locals[name];
    }

    ConstValue *getConstValue(std::string name) {
        auto n_block = blocks.top();
        while (n_block->const_locals.find(name) == n_block->const_locals.end()) {
            if (n_block->parent == nullptr) {
                throw std::logic_error("Undeclared const " + name);
                return nullptr;
            } else
                n_block = n_block->parent;
        }
    }

    void insert(std::string name, Value *alloc) {
        // blocks.top()->locals[name] = alloc;
    }

    void insertConst(std::string name, ConstValue *const_v) { blocks.top()->const_locals[name] = const_v; }

    std::map<std::string, Value *> &locals() { return blocks.top()->locals; }

    BasicBlock *currentBlock() { return blocks.top()->block; }

    void pushBlock(BasicBlock *block) {
        CodeGenBlock *newb = new CodeGenBlock();

        blocks.push(newb);
        blocks.top()->returnValue = nullptr;
        blocks.top()->block = block;
    }

    void popBlock() {
        CodeGenBlock *top = blocks.top();
        blocks.pop();
        delete top;
    }

    void setCurrentReturnValue(Value *value) { blocks.top()->returnValue = value; }

    Value *getCurrentReturnValue() { return blocks.top()->returnValue; }
};


#endif //MY_PASCAL_CODEGEN_H
