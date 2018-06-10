//
// Created by 林世鹏 on 2018/6/2.
//
#include "AST.h"
#include "codegen.h"

using namespace std;

llvm::Function* CodeGenContext::printf;
std::vector<int> CodeGenContext::labels;
llvm::Function* createPrintf(CodeGenContext& context) {
    std::vector<llvm::Type *> printf_arg_types;
    printf_arg_types.push_back(llvm::Type::getInt8PtrTy(llvmContext));
    auto printf_type = llvm::FunctionType::get(llvm::Type::getInt32Ty(llvmContext), printf_arg_types, true);
    auto func = llvm::Function::Create(printf_type, llvm::Function::ExternalLinkage, llvm::Twine("printf"), context.module);
    func->setCallingConv(llvm::CallingConv::C);
    return func;
}

void CodeGenContext::generateCode(Program& root)
{

    std::cout << "Generating code...\n";

    std::vector<Type*> argTypes;
    FunctionType *ftype = FunctionType::get(Type::getVoidTy(llvmContext), makeArrayRef(argTypes), false);
    mainFunction = Function::Create(ftype, GlobalValue::ExternalLinkage, "main", module);
    BasicBlock *bblock = BasicBlock::Create(llvmContext, "entry", mainFunction, 0);

    CodeGenContext::printf = createPrintf(*this);

    pushBlock(bblock);
    currentFunction = mainFunction;
    for (auto label:labels){
        labelBlock[label]=BasicBlock::Create(llvmContext, "label", mainFunction, 0);
    }
    root.codeGen(*this);
    ReturnInst::Create(llvmContext, currentBlock());
    popBlock();

    std::cout << "Code is generated.\n";
    legacy::PassManager pm;
    pm.add(createPrintModulePass(outs()));

    std::cout<<"code is gen~~~\n";
    module->print(llvm::errs(), nullptr);
    std::cout<<"code is gen~!~\n";
}

GenericValue CodeGenContext::runCode() {
    std::cout << "Running begining...\n";
    std::cout <<
              "========================================" << std::endl;
    ExecutionEngine *ee = EngineBuilder(unique_ptr<Module>(module)).create();
    std::vector<GenericValue> noargs;
    GenericValue v = ee->runFunction(mainFunction, noargs);
    std::cout << "========================================" << std::endl;
    std::cout << "Running end.\n";
    return v;
}
