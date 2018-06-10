//
// Created by 林世鹏 on 2018/6/4.
//
#include "AST.h"
#include "codegen.h"


using namespace llvm;
using std::cout;

Value *Identifier::codeGen(CodeGenContext &context) {
    cout << "creating identifier: " << name << endl;
    context.getValue(name);
    return new LoadInst(context.getValue(name), "", context.currentBlock());
}

Value *IntLiteral::codeGen(CodeGenContext &context) {
    cout << "creating int: " << value << endl;
    return ConstantInt::get(Type::getInt32Ty(llvmContext), value, true);
}

Value *DoubleLiteral::codeGen(CodeGenContext &context) {
    cout << "createt double: " << value << endl;
    return ConstantFP::get(llvmContext, llvm::APFloat(value));
}

Value *CharLiteral::codeGen(CodeGenContext &context) {
    cout << "creating char: " << literal << endl;
    return ConstantInt::get(Type::getInt1Ty(llvmContext), literal, true);
}

Value *BoolLiteral::codeGen(CodeGenContext &context) {
    cout << "creating bool: " << value << endl;
    return ConstantInt::get(Type::getInt1Ty(llvmContext), value, true);
}

Value *RangeType::codeGen(CodeGenContext &context) {
    if (this->type == 1) {
        auto left = context.getConstValue(this->lowStr);
        auto right = context.getConstValue(this->highStr);
        if (!left->isRangeType || !right->isRangeType) {
            throw std::domain_error("not a valid range type: double");
        }
        this->low = context.getConstValue(this->lowStr)->rangeValue;
        this->high = context.getConstValue(this->highStr)->rangeValue;
    }
    if (this->low > this->high) {
        throw std::logic_error("range low > high");
    }
    cout << "create range from " << this->low << " to " << this->high;
}

Value *MyBinaryOperator::codeGen(CodeGenContext &context) {
    llvm::Instruction::BinaryOps binaryOp;
    auto op1Value = operand1->codeGen(context);
    auto op2Value = operand2->codeGen(context);

    if (op1Value->getType()->isDoubleTy() || op2Value->getType()->isDoubleTy()) {
        switch (op) {
            case OpType::plus :
                return BinaryOperator::Create(
                        Instruction::FAdd, op1Value, op2Value, "", context.currentBlock());
            case OpType::minus :
                return BinaryOperator::Create(
                        Instruction::FSub, op1Value, op2Value, "", context.currentBlock());
            case OpType::mul :
                return BinaryOperator::Create(
                        Instruction::FMul, op1Value, op2Value, "", context.currentBlock());
            case OpType::div :
                return BinaryOperator::Create(
                        Instruction::FDiv, op1Value, op2Value, "", context.currentBlock());
            case OpType::mod :
                return BinaryOperator::Create(
                        Instruction::SRem, op1Value, op2Value, "", context.currentBlock());
            case OpType::OP_AND :
                return BinaryOperator::Create(
                        Instruction::And, op1Value, op2Value, "", context.currentBlock());
            case OpType::OP_OR :
                return BinaryOperator::Create(
                        Instruction::Or, op1Value, op2Value, "", context.currentBlock());
            case OpType::eq:
                return CmpInst::Create(
                        Instruction::ICmp, CmpInst::ICMP_EQ, op1Value, op2Value, "", context.currentBlock());
            case OpType::ne:
                return CmpInst::Create(
                        Instruction::ICmp, CmpInst::ICMP_NE, op1Value, op2Value, "", context.currentBlock());
            case OpType::lt:
                return CmpInst::Create(
                        Instruction::ICmp, CmpInst::ICMP_SLT, op1Value, op2Value, "", context.currentBlock());
            case OpType::gt:
                return CmpInst::Create(
                        Instruction::ICmp, CmpInst::ICMP_SGT, op1Value, op2Value, "", context.currentBlock());
            case OpType::le:
                return CmpInst::Create(
                        Instruction::ICmp, CmpInst::ICMP_SLE, op1Value, op2Value, "", context.currentBlock());
            case OpType::ge:
                return CmpInst::Create(
                        Instruction::ICmp, CmpInst::ICMP_SGE, op1Value, op2Value, "", context.currentBlock());
            default:
                // TODO: throw right error
                throw std::logic_error("not support binary operator");
        }
    } else {
        switch (op) {
            case OpType::plus:
                return BinaryOperator::Create(
                        Instruction::Add, op1Value, op2Value, "", context.currentBlock());
            case OpType::minus:
                return BinaryOperator::Create(
                        Instruction::Sub, op1Value, op2Value, "", context.currentBlock());
            case OpType::mul:
                return BinaryOperator::Create(
                        Instruction::Mul, op1Value, op2Value, "", context.currentBlock());
            case OpType::div:
                return BinaryOperator::Create(
                        Instruction::SDiv, op1Value, op2Value, "", context.currentBlock());
            case OpType::mod:
                return BinaryOperator::Create(
                        Instruction::SRem, op1Value, op2Value, "", context.currentBlock());
            case OpType::OP_AND:
                return BinaryOperator::Create(
                        Instruction::And, op1Value, op2Value, "", context.currentBlock());
            case OpType::OP_OR:
                return BinaryOperator::Create(
                        Instruction::Or, op1Value, op2Value, "", context.currentBlock());
            case OpType::eq:
                return CmpInst::Create(
                        Instruction::ICmp, CmpInst::ICMP_EQ, op1Value, op2Value, "", context.currentBlock());
            case OpType::ne:
                return CmpInst::Create(
                        Instruction::ICmp, CmpInst::ICMP_NE, op1Value, op2Value, "", context.currentBlock());
            case OpType::lt:
                return CmpInst::Create(
                        Instruction::ICmp, CmpInst::ICMP_SLT, op1Value, op2Value, "", context.currentBlock());
            case OpType::gt:
                return CmpInst::Create(
                        Instruction::ICmp, CmpInst::ICMP_SGT, op1Value, op2Value, "", context.currentBlock());
            case OpType::le:
                return CmpInst::Create(
                        Instruction::ICmp, CmpInst::ICMP_SLE, op1Value, op2Value, "", context.currentBlock());
            case OpType::ge:
                return CmpInst::Create(
                        Instruction::ICmp, CmpInst::ICMP_SGE, op1Value, op2Value, "", context.currentBlock());
            default:
                throw std::logic_error("not support binary type");
        }
    }
}

Value *AssignStmt::codeGen(CodeGenContext &context) {
    if (this->leftType == 2)
        std::cout << "Creating assignment for array: " << this->leftName << "[]" << std::endl;
    else
        std::cout << "Creating assignemnt for id " << this->leftName << std::endl;

    if (this->leftType == 2)
        return new llvm::StoreInst(right->codeGen(context), ((MyArrayRef *) left)->getRef(context), false,
                                   context.currentBlock());

    return new llvm::StoreInst(right->codeGen(context), context.getValue(this->leftName), false,
                               context.currentBlock());
}

llvm::Type *TypeDecl::toLLvmType() {
    switch (type) {
        case MY_Type::integer:
            return llvm::Type::getInt32Ty(llvmContext);
        case MY_Type::real:
            return llvm::Type::getDoubleTy(llvmContext);
        case MY_Type::character:
            return llvm::Type::getInt8Ty(llvmContext);
        case MY_Type::boolean:
            return llvm::Type::getInt1Ty(llvmContext);
        case MY_Type::range:
            return llvm::Type::getInt32Ty(llvmContext);
        case MY_Type::array:
            return llvm::ArrayType::get(this->arrayType->arrayType->toLLvmType(),
                                        this->arrayType->index->rangeType->size());
        default:
            return llvm::Type::getVoidTy(llvmContext);
    }
}

Value *ConstDecl::codeGen(CodeGenContext &context) {
    std::cout << "Creating constant declaration " << this->name->name << std::endl;
    auto alloc = new llvm::AllocaInst(this->type->toLLvmType(), 32, this->name->name.c_str(), context.currentBlock());
    context.insert(this->name->name, alloc);
    auto store = new llvm::StoreInst(this->value->codeGen(context), alloc, false, context.currentBlock());

    context.insertConst(this->name->name, this->value);
    return store;
}

llvm::Value *VarDecl::codeGen(string name, CodeGenContext &context) {
    llvm::Value *alloc;
    if (isGlobal) {
        if (this->type->type == MY_Type::array) {
            auto sub_type = this->type->arrayType->arrayType->type;
            auto vec = std::vector<llvm::Constant *>();

            llvm::Constant *ele_of_arr;
            switch (sub_type) {
                case MY_Type::integer:
                    ele_of_arr = llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvmContext), 0, true);
                    break;
                case MY_Type::real:
                    ele_of_arr = llvm::ConstantFP::get(llvm::Type::getDoubleTy(llvmContext), 0);
                    break;
            }
            for (int i = 0; i < this->type->arrayType->index->rangeType->size(); i++) {
                vec.push_back(ele_of_arr);
            }
            auto arr_type_0 = (llvm::ArrayType *) this->type->toLLvmType();
            auto arr_const = llvm::ConstantArray::get(arr_type_0, vec);

            auto go = new llvm::GlobalVariable(*context.module, this->type->toLLvmType(), false,
                                               llvm::GlobalValue::ExternalLinkage, arr_const, name);
            alloc = go;
        } else {
            auto go = new llvm::GlobalVariable(*context.module, this->type->toLLvmType(), false,
                                               llvm::GlobalValue::ExternalLinkage,
                                               llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvmContext), 0, true),
                                               name);
            alloc = go;
        }
    } else
        alloc = new llvm::AllocaInst(this->type->toLLvmType(), 32, name.c_str(), context.currentBlock());
    context.insert(name, alloc);
    std::cout << "Creating variable declaration successfully " << name << std::endl;
    return alloc;
}

llvm::Value *Program::codeGen(CodeGenContext &context) {
    llvm::Value *last = nullptr;

    /* const decl part */
    for (auto const_decl: this->constPart->children) {
        std::cout << "Generating code for " << typeid(const_decl).name() << std::endl;
        last = const_decl->codeGen(context);
    }
    // deal with variable declaration

    for (auto node: this->varPart->children) {
        std::cout << "Generating code for " << typeid(node).name() << std::endl;
        auto var_decl = (VarDecl *) node;
        var_decl->isGlobal = 1;
        for (auto temp: var_decl->names->children) {
            auto idd = (Identifier*)temp;
            last = var_decl->codeGen(idd->name, context);
        }
    }
    for (auto routine : this->routineList->children) {
        std::cout << "Generating code for " << typeid(routine).name() << std::endl;
        last = routine->codeGen(context);
    }

    for (auto stmt: routineBody->children)
        stmt->codeGen(context);
    std::cout << "Creating program" << endl;
    return last;
}

llvm::Value *Routine::codeGen(CodeGenContext &context) {
    std::vector<llvm::Type *> arg_types;
    for (auto it : this->args->children) {
        auto varDecl = (VarDecl *) it;
        for (auto temp: varDecl->names->children)
            arg_types.push_back(varDecl->type->toLLvmType());
    }
    auto f_type = llvm::FunctionType::get(
            this->isProcedure() ? llvm::Type::getVoidTy(llvmContext) : this->returnType->toLLvmType(),
            llvm::makeArrayRef(arg_types), false);
    std::cout << this->routineName->name << std::endl;
    auto function = llvm::Function::Create(f_type, llvm::GlobalValue::InternalLinkage, this->routineName->name,
                                           context.module);
    auto block = llvm::BasicBlock::Create(llvmContext, "entry", function, NULL);
    auto oldFunc = context.currentFunction;
    context.currentFunction = function;
    auto oldBlock = context.currentBlock();
    context.parent[function] = oldFunc;
    // push block and start routine
    context.pushBlock(block);

    // deal with arguments
    llvm::Value *arg_value;
    auto args_values = function->arg_begin();
    for (auto arg : this->args->children) {
        auto varDecl = (VarDecl *) arg;
        for (auto temp: varDecl->names->children) {
            auto name = (Identifier*)temp;
            varDecl->codeGen(name->name, context);
            arg_value = args_values++;
            arg_value->setName(name->name.c_str());
            auto inst = new llvm::StoreInst(arg_value, context.getValue(name->name), false, block);
        }
    }

    std::cout << "is func?" << !this->isProcedure() << " part suc!\n";
    // add function return variable
    if (!this->isProcedure()) {
        std::cout << "Creating function return value declaration" << this->returnType->repr() << std::endl;
        auto alloc = new llvm::AllocaInst(this->returnType->toLLvmType(), 32, this->routineName->name.c_str(),
                                          context.currentBlock());
        // context.insert(this->routine_name->name) = alloc;
    }
    std::cout << "func part suc!\n";
    // deal with variable declaration
    for (auto arg: this->varPart->children) {
        std::cout << "Generating code for decl " << typeid(arg).name() << std::endl;
        auto varDecl = (VarDecl *) arg;
        for (auto temp: varDecl->names->children) {
            auto name = (Identifier*)temp;
            varDecl->codeGen(name->name, context);
        }
    }
    for (auto routine : this->routineList->children) {
        std::cout << "Generating code for " << typeid(routine).name() << std::endl;
        routine->codeGen(context);
    }
    // deal with program statements
    std::cout << "var part suc!\n";
    routineBody->codeGen(context);
    // return value


    if (!this->isProcedure()) {
        std::cout << "Generating return value for function" << std::endl;
        auto load_ret = new llvm::LoadInst(context.getValue(this->routineName->name), "", false,
                                           context.currentBlock());
        llvm::ReturnInst::Create(llvmContext, load_ret, context.currentBlock());
    } else if (this->isProcedure()) {
        std::cout << "Generating return void for procedure" << std::endl;
        llvm::ReturnInst::Create(llvmContext, context.currentBlock());

    }

    // pop block and finsh
    while (context.currentBlock() != oldBlock)
        context.popBlock();
    context.currentFunction = oldFunc;
    std::cout << "Creating " << this->repr() << ":" << this->routineName->name << std::endl;
    return function;
}

llvm::Value *FuncCall::codeGen(CodeGenContext &context) {
    auto function = context.module->getFunction(this->identifier->name);
    if (function == nullptr)
        throw std::domain_error("Function not defined: " + this->identifier->name);
    std::vector<Value *> args;
    for (auto node: this->args->children) {
        auto varDecl = (VarDecl *) node;
        for (auto temp: varDecl->names->children) {
            auto name = (Identifier*)temp;
            args.push_back(varDecl->codeGen(name->name, context));
        }
    }
    auto call = llvm::CallInst::Create(function, llvm::makeArrayRef(args), "", context.currentBlock());
    std::cout << "Creating method call: " << this->identifier->name << std::endl;
    return call;
}

llvm::Value *ProcCall::codeGen(CodeGenContext &context) {
    auto function = context.module->getFunction(this->procName->name.c_str());
    if (function == nullptr)
        throw std::domain_error("procedure not defined: " + this->procName->name);
    std::vector<Value *> args;
    for (auto node: this->args->children) {
        auto varDecl = (VarDecl *) node;
        for (auto temp: varDecl->names->children) {
            auto name = (Identifier*)temp;
            args.push_back(varDecl->codeGen(name->name, context));
        }
    }
    auto call = llvm::CallInst::Create(function, llvm::makeArrayRef(args), "", context.currentBlock());
    std::cout << "Creating method(p) call: " << this->procName->name << std::endl;
    return call;
}

llvm::Value *IfStmt::codeGen(CodeGenContext &context) {
    Value *test = condition->codeGen(context);
    BasicBlock *btrue = BasicBlock::Create(llvmContext, "thenStmt", context.currentFunction);
    BasicBlock *bfalse = BasicBlock::Create(llvmContext, "elseStmt", context.currentFunction);
    BasicBlock *bmerge = BasicBlock::Create(llvmContext, "mergeStmt", context.currentFunction);
    auto ret = llvm::BranchInst::Create(btrue, bfalse, test, context.currentBlock());

    context.pushBlock(btrue);
    ifBody->codeGen(context);
    llvm::BranchInst::Create(bmerge, context.currentBlock());
    context.popBlock();
    context.pushBlock(bfalse);
    if (elseBody != nullptr)
        elseBody->codeGen(context);
    llvm::BranchInst::Create(bmerge, context.currentBlock());
    context.popBlock();
    context.pushBlock(bmerge);


    return ret;
}

llvm::Value *WhileStmt::codeGen(CodeGenContext &context) {
    BasicBlock *sloop = BasicBlock::Create(llvmContext, "startloop", context.currentFunction);
    BasicBlock *bloop = BasicBlock::Create(llvmContext, "loopStmt", context.currentFunction);
    BasicBlock *bexit = BasicBlock::Create(llvmContext, "eixtStmt", context.currentFunction);
    llvm::BranchInst::Create(sloop, context.currentBlock());
    context.pushBlock(sloop);
    Value *test = condition->codeGen(context);
    llvm::Instruction *ret = llvm::BranchInst::Create(bloop, bexit, test, context.currentBlock());
    context.popBlock();

    context.pushBlock(bloop);
    statement->codeGen(context);
    llvm::BranchInst::Create(sloop, context.currentBlock());
    context.popBlock();

    context.pushBlock(bexit);

    return ret;
}

llvm::Value *RepeatStmt::codeGen(CodeGenContext &context) {
    BasicBlock *bloop = BasicBlock::Create(llvmContext, "loopStmt", context.currentFunction);
    BasicBlock *bexit = BasicBlock::Create(llvmContext, "eixtStmt", context.currentFunction);
    llvm::BranchInst::Create(bloop, context.currentBlock());

    context.pushBlock(bloop);
    for (auto node: statementList->children) {
        auto stmt = (Statement *) node;
        stmt->codeGen(context);
    }
    Value *test = condition->codeGen(context);
    llvm::Instruction *ret = llvm::BranchInst::Create(bexit, bloop, test, context.currentBlock());
    context.popBlock();

    context.pushBlock(bexit);

    return ret;
}

llvm::Value *ForStmt::codeGen(CodeGenContext &context) {
    BasicBlock *sloop = BasicBlock::Create(llvmContext, "startloop", context.currentFunction);
    BasicBlock *bloop = BasicBlock::Create(llvmContext, "loopStmt", context.currentFunction);
    BasicBlock *bexit = BasicBlock::Create(llvmContext, "eixtStmt", context.currentFunction);

    AssignStmt *initial = new AssignStmt(variable, left);
    initial->codeGen(context);
    llvm::BranchInst::Create(sloop, context.currentBlock());

    context.pushBlock(sloop);
    MyBinaryOperator *compare = new MyBinaryOperator(OpType::eq, variable, right);
    Value *test = compare->codeGen(context);
    llvm::Instruction *ret = llvm::BranchInst::Create(bexit, bloop, test, context.currentBlock());
    context.popBlock();

    context.pushBlock(bloop);
    stmt->codeGen(context);
    MyBinaryOperator *update;

    auto int1 = new IntLiteral(1);
    if (direction == 1) {
        update = new MyBinaryOperator(OpType::plus, variable, int1);
    } else {
        update = new MyBinaryOperator(OpType::minus, variable, int1);
    }
    auto updateStmt = new AssignStmt(variable, update);
    updateStmt->codeGen(context);
    llvm::BranchInst::Create(sloop, context.currentBlock());
    context.popBlock();

    context.pushBlock(bexit);
    stmt->codeGen(context);
    delete initial;
    delete compare;
    delete int1;
    delete update;
    delete updateStmt;
    return ret;

}

llvm::Value *CaseStmt::codeGen(CodeGenContext &context) {

    cout << "in case" << expression->repr() << endl;
    auto ret = statement->codeGen(context);
    cout << "in case 2" << expression->repr() << endl;
    llvm::BranchInst::Create(bexit, context.currentBlock());
    cout << "in case 3" << expression->repr() << endl;
    return ret;
}

llvm::Value *SwitchStmt::codeGen(CodeGenContext &context) {
    BasicBlock *bexit = BasicBlock::Create(llvmContext, "exit", context.currentFunction);

    std::vector<BasicBlock *> bblocks;
    cout << ((CaseStmt *) switchBody->children[0])->repr() << endl;

    for (int i = 0; i < switchBody->children.size(); i++) {
        auto bblock = BasicBlock::Create(llvmContext, "caseStmt", context.currentFunction);
        bblocks.push_back(bblock);
    }

    for (int i = 0; i < bblocks.size() - 1; i++) {
        cout << "in bblocks\n";
        auto caseStmt = (CaseStmt *)switchBody->children[i];
        cout << caseStmt->expression->repr() << "\n";
        auto con = new MyBinaryOperator(OpType::eq, expression, caseStmt->expression);
        BasicBlock *bnext = BasicBlock::Create(llvmContext, "next", context.currentFunction);
        llvm::BranchInst::Create(bblocks[i], bnext, con->codeGen(context), context.currentBlock());
        context.pushBlock(bnext);
    }
    auto con = new MyBinaryOperator(OpType::eq, expression, ((CaseStmt *)switchBody->children[switchBody->children.size()-1])->expression);
    auto ret = llvm::BranchInst::Create(bblocks[bblocks.size() - 1], bexit, con->codeGen(context),
                                        context.currentBlock());
    for (int i = 0; i < bblocks.size(); i++) {
        context.pushBlock(bblocks[i]);
        auto cst = (CaseStmt *) switchBody->children[i];
        cst->bexit = bexit;
        cst->codeGen(context);
        cout << "gggg~~~\n";
        context.popBlock();
    }

    context.pushBlock(bexit);

    return ret;
}

llvm::Value* GotoStmt::codeGen(CodeGenContext &context){
    llvm::Value* test= (new BoolLiteral("true"))->codeGen(context);
    BasicBlock* bafter = BasicBlock::Create(llvmContext, "afterGoto", context.currentFunction);
    auto ret = llvm::BranchInst::Create(context.labelBlock[label],context.currentBlock());
    context.pushBlock(bafter);
    return ret;
}

llvm::Value* MyArrayRef::codeGen(CodeGenContext &context){
    return new llvm::LoadInst(this->getRef(context), "", false, context.currentBlock());
}

llvm::Value* MyArrayRef::getRef(CodeGenContext& context) {
    auto arr = context.getValue(this->arrayName->name);

    auto idx_list = std::vector<llvm::Value*>();
    idx_list.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvmContext), 0));
    idx_list.push_back(index->codeGen(context));
    return llvm::GetElementPtrInst::CreateInBounds(arr, llvm::ArrayRef<llvm::Value*>(idx_list), "tempname", context.currentBlock());
}
