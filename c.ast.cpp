#include "c.ast.hpp"

#include <iostream>

#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

using namespace std;

ostream &operator<<(ostream &output, const TypeSpecifier &type) {
    const string stringreps[]{"void", "int", "char", "..."};
    cout << stringreps[static_cast<int>(type)];
    return output;
}

ostream &operator<<(ostream &output, const BinaryOperator &type) {
    const string stringreps[]{
        "Multiply", "Divide",   "Plus",       "Minus",      "Left",
        "Right",    "Less",     "Greater",    "LessEqual",  "GreaterEqual",
        "Equal",    "NotEqual", "BitwiseAnd", "BitwiseXor", "BitwiseOr",
        "And",      "Or"};
    cout << stringreps[static_cast<int>(type)];
    return output;
}

ostream &operator<<(ostream &output, const ReferentType &type) {
    const string stringreps[]{"function", "var"};
    cout << stringreps[static_cast<int>(type)];
    return output;
}

llvm::Type *generateType(TypeSpecifier type, llvm::LLVMContext &context) {
    switch (type) {
        case TypeSpecifier::Int:
            return llvm::Type::getInt32Ty(context);
            break;
        case TypeSpecifier::Char:
            return llvm::Type::getInt32Ty(context);
            break;
        case TypeSpecifier::Void:
            return llvm::Type::getVoidTy(context);
            break;
        default:
            break;
    }
}

void emitCode(CodeKit &kit) {
    llvm::verifyModule(kit.module, &llvm::outs());
    kit.module.print(llvm::outs(), nullptr);

    auto irFile = "output.bc";
    error_code ec;
    llvm::raw_fd_ostream irFileStream(irFile, ec, llvm::sys::fs::F_None);
    llvm::WriteBitcodeToFile(kit.module, irFileStream);
    irFileStream.flush();

    auto targetTriple = llvm::sys::getDefaultTargetTriple();
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    string error;
    auto target = llvm::TargetRegistry::lookupTarget(targetTriple, error);
    auto cpu = "generic";
    auto features = "";
    llvm::TargetOptions opt;
    auto rm = llvm::Optional<llvm::Reloc::Model>();
    auto targetMachine =
        target->createTargetMachine(targetTriple, cpu, features, opt, rm);

    kit.module.setDataLayout(targetMachine->createDataLayout());
    kit.module.setTargetTriple(targetTriple);

    auto objectFile = "output.o";
    llvm::raw_fd_ostream objectFileStream(objectFile, ec,
                                          llvm::sys::fs::OF_None);

    llvm::legacy::PassManager pass;
    auto fileType = llvm::CGFT_ObjectFile;
    targetMachine->addPassesToEmitFile(pass, objectFileStream, nullptr,
                                       fileType);
    pass.run(kit.module);
    objectFileStream.flush();
}

llvm::AllocaInst *createEntryBlockAlloca(llvm::Function *func, const string var,
                                         llvm::Type *type) {
    llvm::IRBuilder<> tempBuilder(&func->getEntryBlock(),
                                  func->getEntryBlock().begin());
    return tempBuilder.CreateAlloca(type, 0, var.c_str());
}

llvm::Value *logError(string error) {
    cout << "error: " << error << endl;
    return nullptr;
}

// AST

void AST::print(ostream &output, int indent) const {
    for (auto it : *items) {
        output << *it << endl;
    }
}

void AST::traverse(SymbolTable<Referent> &st) {
    st.enterScope();
    for (auto it : *items) {
        it->traverse(st);
        // cout << st << endl;
    }
    st.exitScope();
};

llvm::Value *AST::generateCode(CodeKit &kit) {
    kit.symbolTable.enterScope();
    for (auto it : *items) {
        auto *func = it->generateCode(kit);
    }
    kit.symbolTable.exitScope();
}

// Declaration

void Declaration::print(ostream &output, int indent) const {
    output << string(indent, ' ') << type;
    if (type != TypeSpecifier::Ellipsis) {
        if (constant) {
            output << " const";
        }
        output << " " << *sig;
    }
}

void Declaration::traverse(SymbolTable<Referent> &st) {
    auto *ref = new Referent(ReferentType::Var, type, sig->pointers, this);
    st.addSymbol(sig->name, ref);
};

llvm::Value *Declaration::generateCode(CodeKit &kit) {
    llvm::Function *func = kit.builder.GetInsertBlock()->getParent();
    llvm::Type *t = generateType(type, kit.context);
    auto *val = llvm::Constant::getNullValue(t);
    auto *alloca = createEntryBlockAlloca(func, sig->name, t);
    kit.builder.CreateStore(val, alloca);
    kit.symbolTable.addSymbol(sig->name, alloca);
    return val;
}

// Signature

void Signature::print(ostream &output, int indent) const {
    output << string(indent, ' ') << name << "(";
    if (arguments != nullptr) {
        for (auto it : *arguments) {
            output << *it;
            if (it != arguments->back()) {
                output << ", ";
            }
        }
    }
    output << ")";
};

// Literal

void IntLiteral::print(ostream &output, int indent) const {
    output << string(indent, ' ') << value;
}

llvm::Value *IntLiteral::generateCode(CodeKit &kit) {
    return llvm::ConstantInt::get(kit.context, llvm::APInt(32, value, true));
};

void StrLiteral::print(ostream &output, int indent) const {
    output << string(indent, ' ') << str;
}

void Identifier::print(ostream &output, int indent) const {
    output << string(indent, ' ') << name;
}

llvm::Value *Identifier::generateCode(CodeKit &kit) {
    llvm::Value *v = kit.symbolTable.findSymbol(name);
    if (v == nullptr) {
        return logError("variable undeclared");
    }
    return kit.builder.CreateLoad(v, name.c_str());
};

// CompoundStatement

void CompoundStatement::print(ostream &output, int indent) const {
    output << string(indent, ' ') << "{" << endl;
    for (auto it : *items) {
        it->print(output, indent + 4);
        output << endl;
    }
    output << string(indent, ' ') << "}";
}

void CompoundStatement::traverse(SymbolTable<Referent> &st) {
    for (auto it : *items) {
        it->traverse(st);
    }
}

llvm::Value *CompoundStatement::generateCode(CodeKit &kit) {
    for (auto it : *items) {
        it->generateCode(kit);
    }
};

// BinaryExpression

void BinaryExpression::print(ostream &output, int indent) const {
    output << string(indent, ' ') << op << "(" << *left << ", " << *right
           << ")";
}

llvm::Value *BinaryExpression::generateCode(CodeKit &kit) {
    auto *lv = left->generateCode(kit);
    auto *rv = right->generateCode(kit);
    switch (op) {
        case BinaryOperator::Multiply:
            return kit.builder.CreateMul(lv, rv, "Multiply");
            break;
        case BinaryOperator::Divide:
            return kit.builder.CreateSDiv(lv, rv, "Divide");
            break;
        case BinaryOperator::Plus:
            return kit.builder.CreateAdd(lv, rv, "Add");
            break;
        case BinaryOperator::Minus:
            return kit.builder.CreateSub(lv, rv, "Subtract");
            break;
        case BinaryOperator::Left:
            return kit.builder.CreateShl(lv, rv, "Left shift");
            break;
        case BinaryOperator::Right:
            return kit.builder.CreateLShr(lv, rv, "Right shift");
            break;
        case BinaryOperator::Less:
            return kit.builder.CreateICmpSLT(lv, rv, "Less");
            break;
        case BinaryOperator::Greater:
            return kit.builder.CreateICmpSGT(lv, rv, "Greater");
            break;
        case BinaryOperator::LessEqual:
            return kit.builder.CreateICmpSLE(lv, rv, "Less or Equal");
            break;
        case BinaryOperator::GreaterEqual:
            return kit.builder.CreateICmpSGE(lv, rv, "Greater or Equal");
            break;
        case BinaryOperator::Equal:
            return kit.builder.CreateICmpEQ(lv, rv, "Equal");
            break;
        case BinaryOperator::NotEqual:
            return kit.builder.CreateICmpNE(lv, rv, "Not Equal");
            break;
        case BinaryOperator::BitwiseAnd:
            return kit.builder.CreateAnd(lv, rv, "Bitwise And");
            break;
        case BinaryOperator::BitwiseXor:
            return kit.builder.CreateXor(lv, rv, "Bitwise Xor");
            break;
        case BinaryOperator::BitwiseOr:
            return kit.builder.CreateOr(lv, rv, "Bitwise Or");
            break;
        case BinaryOperator::And:
            return kit.builder.CreateAnd(lv, rv, "And");
            break;
        case BinaryOperator::Or:
            return kit.builder.CreateOr(lv, rv, "Or");
            break;
        default:
            break;
    }
};

// Assignment

void Assignment::print(ostream &output, int indent) const {
    output << string(indent, ' ');
    if (var != nullptr) {
        output << *var << " = ";
    }
    output << *expr;
}

llvm::Value *Assignment::generateCode(CodeKit &kit) {
    llvm::Value *val = expr->generateCode(kit);
    if (var == nullptr) {
        return val;
    }
    llvm::Value *variable = kit.symbolTable.findSymbol(var->name);
    if (variable == nullptr) {
        return logError("variable undeclared");
    }
    kit.builder.CreateStore(val, variable);
    return val;
}

// While

void While::print(ostream &output, int indent) const {
    output << string(indent, ' ') << "While (" << *cond << ")" << endl;
    stmt->print(output, indent + 4);
}

void While::traverse(SymbolTable<Referent> &st) {
    st.enterScope();
    this->stmt->traverse(st);
    st.exitScope();
}

llvm::Value *While::generateCode(CodeKit &kit) {
    llvm::Function *func = kit.builder.GetInsertBlock()->getParent();
    auto *checkBlock =
        llvm::BasicBlock::Create(kit.context, "whilecheck", func);
    auto *loopBlock = llvm::BasicBlock::Create(kit.context, "whileloop");
    auto *mergeBlock = llvm::BasicBlock::Create(kit.context, "whilemerge");

    kit.builder.CreateBr(checkBlock);
    kit.builder.SetInsertPoint(checkBlock);
    llvm::Value *condVal = cond->generateCode(kit);
    kit.builder.CreateCondBr(condVal, loopBlock, mergeBlock);

    func->getBasicBlockList().push_back(loopBlock);
    kit.builder.SetInsertPoint(loopBlock);
    kit.symbolTable.enterScope();
    stmt->generateCode(kit);
    kit.symbolTable.exitScope();
    loopBlock = kit.builder.GetInsertBlock();
    if (loopBlock->getTerminator() == nullptr) {
        kit.builder.CreateBr(checkBlock);
    }

    func->getBasicBlockList().push_back(mergeBlock);
    kit.builder.SetInsertPoint(mergeBlock);
    return nullptr;
}

// Return

void Return::print(ostream &output, int indent) const {
    output << string(indent, ' ') << "Return " << *expr;
}

llvm::Value *Return::generateCode(CodeKit &kit) {
    auto *val = expr->generateCode(kit);
    kit.builder.CreateRet(val);
}

// Conditional

void Conditional::print(ostream &output, int indent) const {
    output << string(indent, ' ') << "If (" << *condition << ")" << endl;
    ifstmt->print(output, indent + 4);
    if (elsestmt != nullptr) {
        output << endl << string(indent, ' ') << "Else" << endl;
        elsestmt->print(output, indent + 4);
    }
}

void Conditional::traverse(SymbolTable<Referent> &st) {
    if (this->ifstmt != nullptr) {
        st.enterScope();
        this->ifstmt->traverse(st);
        st.exitScope();
    }
    if (this->elsestmt != nullptr) {
        st.enterScope();
        this->elsestmt->traverse(st);
        st.exitScope();
    }
}

llvm::Value *Conditional::generateCode(CodeKit &kit) {
    auto *cond = condition->generateCode(kit);
    if (cond == nullptr) {
        return nullptr;
    }
    llvm::Function *func = kit.builder.GetInsertBlock()->getParent();
    auto *ifBlock = llvm::BasicBlock::Create(kit.context, "if", func);
    auto *elseBlock = llvm::BasicBlock::Create(kit.context, "else");
    auto *mergeBlock = llvm::BasicBlock::Create(kit.context, "ifelsemerge");

    kit.builder.CreateCondBr(cond, ifBlock, elseBlock);
    kit.builder.SetInsertPoint(ifBlock);
    if (ifstmt != nullptr) {
        kit.symbolTable.enterScope();
        ifstmt->generateCode(kit);
        kit.symbolTable.exitScope();
    }
    ifBlock = kit.builder.GetInsertBlock();
    if (ifBlock->getTerminator() == nullptr) {
        kit.builder.CreateBr(mergeBlock);
    }

    func->getBasicBlockList().push_back(elseBlock);
    kit.builder.SetInsertPoint(elseBlock);
    if (elsestmt != nullptr) {
        kit.symbolTable.enterScope();
        elsestmt->generateCode(kit);
        kit.symbolTable.exitScope();
    }
    elseBlock = kit.builder.GetInsertBlock();
    if (elseBlock->getTerminator() == nullptr) {
        kit.builder.CreateBr(mergeBlock);
    }

    func->getBasicBlockList().push_back(mergeBlock);
    kit.builder.SetInsertPoint(mergeBlock);
    return nullptr;
}

// FunctionDeclaration

void FunctionDeclaration::print(ostream &output, int indent) const {
    output << string(indent, ' ') << "function " << ret << " " << name << " (";
    if (arguments != nullptr) {
        for (auto it : *arguments) {
            output << *it << ", ";
        }
    }
    output << ")";
};

void FunctionDeclaration::traverse(SymbolTable<Referent> &st) {
    auto *ref = new Referent(ReferentType::Func, this->ret, 0, this);
    st.addSymbol(this->name, ref);
};

llvm::Function *FunctionDeclaration::generateCode(CodeKit &kit) {
    vector<llvm::Type *> argTypes;
    if (arguments != nullptr) {
        for (auto it : *arguments) {
            argTypes.push_back(generateType(it->type, kit.context));
        }
    }
    llvm::FunctionType *funcType = llvm::FunctionType::get(
        generateType(ret, kit.context), argTypes, false);
    llvm::Function *func = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, name, &kit.module);
    int index = 0;
    for (auto &it : func->args()) {
        it.setName(arguments->at(index++)->sig->name);
    };
    return func;
};

// FunctionDefinition

void FunctionDefinition::print(ostream &output, int indent) const {
    output << string(indent, ' ') << *decl << endl;
    content->print(output, indent + 4);
};

void FunctionDefinition::traverse(SymbolTable<Referent> &st) {
    decl->traverse(st);
    st.enterScope();
    if (decl->arguments != nullptr) {
        for (auto it : *decl->arguments) {
            it->traverse(st);
        }
    }
    content->traverse(st);
    cout << st << endl;
    st.exitScope();
}

llvm::Function *FunctionDefinition::generateCode(CodeKit &kit) {
    auto *func = kit.module.getFunction(decl->name);
    if (func == nullptr) {
        func = decl->generateCode(kit);
    }
    if (!func->empty()) {
        return (llvm::Function *)logError("redefinition of " + decl->name);
    }

    llvm::BasicBlock *entryBlock =
        llvm::BasicBlock::Create(kit.context, "entry", func);
    kit.builder.SetInsertPoint(entryBlock);
    kit.symbolTable.enterScope();

    for (auto &arg : func->args()) {
        auto *alloca =
            createEntryBlockAlloca(func, arg.getName(), arg.getType());
        kit.builder.CreateStore(&arg, alloca);
        kit.symbolTable.addSymbol(arg.getName(), alloca);
    }

    content->generateCode(kit);
    if (func->getReturnType()->isVoidTy()) {
        kit.builder.CreateRetVoid();
    }
    llvm::verifyFunction(*func, &llvm::outs());
    kit.symbolTable.exitScope();
    return func;
};

// FunctionCall

void FunctionCall::print(ostream &output, int indent) const {
    output << string(indent, ' ') << function << "(";
    for (auto it : *arguments) {
        output << *it;
        if (it != arguments->back()) {
            output << ", ";
        }
    }
    output << ")";
}

llvm::Value *FunctionCall::generateCode(CodeKit &kit) {
    llvm::Function *func = kit.module.getFunction(function);
    if (func == nullptr) {
        return logError("undefined reference to " + function);
    }
    vector<llvm::Value *> args;
    for (auto arg : *arguments) {
        args.push_back(arg->generateCode(kit));
    }
    return kit.builder.CreateCall(func, args, "call");
};