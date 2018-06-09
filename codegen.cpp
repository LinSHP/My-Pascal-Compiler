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

/* Compile the AST into a module */
void CodeGenContext::generateCode(Program& root)
{

    std::cout << "Generating code...\n";

    /* Create the top level interpreter function to call as entry */
    std::vector<Type*> argTypes;
    FunctionType *ftype = FunctionType::get(Type::getVoidTy(llvmContext), makeArrayRef(argTypes), false);
    // change GlobalValue::InternalLinkage into ExternalLinkage
    mainFunction = Function::Create(ftype, GlobalValue::ExternalLinkage, "main", module);
    BasicBlock *bblock = BasicBlock::Create(llvmContext, "entry", mainFunction, 0);

    CodeGenContext::printf = createPrintf(*this);

    /* Push a new variable/block context */
    pushBlock(bblock);
    currentFunction = mainFunction;
    for (auto label:labels){
        labelBlock[label]=BasicBlock::Create(llvmContext, "label", mainFunction, 0);
    }
    root.codeGen(*this); /* emit bytecode for the toplevel block */
    ReturnInst::Create(llvmContext, currentBlock());
    popBlock();
    // popBlock();

    /* Print the bytecode in a human-readable format
       to see if our program compiled properly
     */
    std::cout << "Code is generated.\n";
    legacy::PassManager pm;
    pm.add(createPrintModulePass(outs()));
    //pm.run(*module);

    // write IR to stderr
    std::cout<<"code is gen~~~\n";
    module->print(llvm::errs(), nullptr);
    std::cout<<"code is gen~!~\n";
}

/* Executes the AST by running the main function */
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
