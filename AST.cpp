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
    cout << "creating int" << value << endl;
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

Value *BinaryOperator::codeGen(CodeGenContext &context) {
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
        return nullptr;
    }
}

Value* AssignStmt::codeGen(CodeGenContext& context) {
    if (this->leftType == 2)
        std::cout << "Creating assignment for array: " << this->leftName << "[]" << std::endl;
    else
        std::cout << "Creating assignemnt for id " << this->leftName << std::endl;

    if (this->leftType == 2)
        return new llvm::StoreInst(right->codeGen(context), ((ArrayRef* )left)->getRef(context), false, context.currentBlock());

    return new llvm::StoreInst(right->codeGen(context), context.getValue(this->leftName), false, context.currentBlock());
}

Value* ConstDecl::codeGen(CodeGenContext& context) {
    std::cout << "Creating constant declaration " << this->name->name << std::endl;
    auto alloc = new llvm::AllocaInst(this->type->toLLVMType(), this->name->name.c_str(), context.currentBlock());
    context.insert(this->name->name, alloc);
    auto store = new llvm::StoreInst(this->value->codeGen(context), alloc, false, context.currentBlock());

    context.insertConst(this->name->name, this->value);
    return store;
}
