#include "c.ast.hpp"

#include <iostream>

#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/IR/LegacyPassManager.h"


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

unique_ptr<llvm::LLVMContext> theContext;
unique_ptr<llvm::IRBuilder<>> builder;
unique_ptr<llvm::Module> theModule;
map<string, llvm::AllocaInst *> namedValues;

llvm::Type *generateType(TypeSpecifier type) {
    switch (type) {
        case TypeSpecifier::Int:
            return llvm::Type::getInt32Ty(*theContext);
            break;
        case TypeSpecifier::Char:
            return llvm::Type::getInt32Ty(*theContext);
            break;
        case TypeSpecifier::Void:
            return llvm::Type::getVoidTy(*theContext);
            break;
        default:
            break;
    }
}

void initLlvm() {
    theContext = make_unique<llvm::LLVMContext>();
    theModule = make_unique<llvm::Module>("my cool jit", *theContext);
    builder = make_unique<llvm::IRBuilder<>>(*theContext);
}

void emitCode() {
    theModule->print(llvm::outs(), nullptr);

    auto irFile = "output.bc";
    error_code ec;
    llvm::raw_fd_ostream irFileStream(irFile, ec, llvm::sys::fs::F_None);
    llvm::WriteBitcodeToFile(*theModule, irFileStream);
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
    auto targetMachine = target->createTargetMachine(targetTriple, cpu, features, opt, rm);

    theModule->setDataLayout(targetMachine->createDataLayout());
    theModule->setTargetTriple(targetTriple);

    auto objectFile = "output.o";
    llvm::raw_fd_ostream objectFileStream(objectFile, ec, llvm::sys::fs::OF_None);

    llvm::legacy::PassManager pass;
    auto fileType = llvm::CGFT_ObjectFile;
    targetMachine->addPassesToEmitFile(pass, objectFileStream, nullptr, fileType);
    pass.run(*theModule);
    objectFileStream.flush();
}

llvm::AllocaInst *createEntryBlockAlloca(llvm::Function *func, const string var,
                                         llvm::Type *type) {
    llvm::IRBuilder<> tempBuilder(&func->getEntryBlock(),
                                  func->getEntryBlock().begin());
    return tempBuilder.CreateAlloca(type, 0, var.c_str());
}

llvm::Value *logError(string error) {
    cout << "error: " << error;
    return nullptr;
}

// AST

void AST::print(ostream &output, int indent) const {
    for (auto it : *items) {
        output << *it << endl;
    }
}

void AST::traverse(SymbolTable &st) {
    st.enterScope();
    for (auto it : *items) {
        it->traverse(st);
        // cout << st << endl;
    }
    st.exitScope();
};

llvm::Value *AST::generateCode() {
    for (auto it : *items) {
        auto *func = it->generateCode();
    }
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

void Declaration::traverse(SymbolTable &st) {
    auto *ref = new Referent(ReferentType::Var, type, sig->pointers, this);
    st.addSymbol(sig->name, ref);
};

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

llvm::Value *IntLiteral::generateCode() {
    return llvm::ConstantInt::get(*theContext, llvm::APInt(32, value, true));
};

void StrLiteral::print(ostream &output, int indent) const {
    output << string(indent, ' ') << str;
}

void Identifier::print(ostream &output, int indent) const {
    output << string(indent, ' ') << name;
}

llvm::Value *Identifier::generateCode() {
    llvm::Value *v = namedValues[name];
    if (v == nullptr) {
        logError("variable undeclared");
    }
    return builder->CreateLoad(v, name.c_str());
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

void CompoundStatement::traverse(SymbolTable &st) {
    for (auto it : *items) {
        it->traverse(st);
    }
}

llvm::Value *CompoundStatement::generateCode() {
    for (auto it : *items) {
        it->generateCode();
    }
};

// BinaryExpression

void BinaryExpression::print(ostream &output, int indent) const {
    output << string(indent, ' ') << op << "(" << *left << ", " << *right
           << ")";
}

llvm::Value *BinaryExpression::generateCode() {
    auto *lv = left->generateCode();
    auto *rv = right->generateCode();
    switch (op) {
        case BinaryOperator::Multiply:
            return builder->CreateMul(lv, rv, "Multiply");
            break;
        case BinaryOperator::Divide:
            return builder->CreateSDiv(lv, rv, "Divide");
            break;
        case BinaryOperator::Plus:
            return builder->CreateAdd(lv, rv, "Add");
            break;
        case BinaryOperator::Minus:
            return builder->CreateSub(lv, rv, "Subtract");
            break;
        case BinaryOperator::Left:
            return builder->CreateShl(lv, rv, "Left shift");
            break;
        case BinaryOperator::Right:
            return builder->CreateLShr(lv, rv, "Right shift");
            break;
        case BinaryOperator::Less:
            return builder->CreateICmpSLT(lv, rv, "Less");
            break;
        case BinaryOperator::Greater:
            return builder->CreateICmpSGT(lv, rv, "Greater");
            break;
        case BinaryOperator::LessEqual:
            return builder->CreateICmpSLE(lv, rv, "Less or Equal");
            break;
        case BinaryOperator::GreaterEqual:
            return builder->CreateICmpSGE(lv, rv, "Greater or Equal");
            break;
        case BinaryOperator::Equal:
            return builder->CreateICmpEQ(lv, rv, "Equal");
            break;
        case BinaryOperator::NotEqual:
            return builder->CreateICmpNE(lv, rv, "Not Equal");
            break;
        case BinaryOperator::BitwiseAnd:
            return builder->CreateAnd(lv, rv, "Bitwise And");
            break;
        case BinaryOperator::BitwiseXor:
            return builder->CreateXor(lv, rv, "Bitwise Xor");
            break;
        case BinaryOperator::BitwiseOr:
            return builder->CreateOr(lv, rv, "Bitwise Or");
            break;
        case BinaryOperator::And:
            return builder->CreateAnd(lv, rv, "And");
            break;
        case BinaryOperator::Or:
            return builder->CreateOr(lv, rv, "Or");
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

llvm::Value *Assignment::generateCode() {
    llvm::Value *val = expr->generateCode();
    if (var == nullptr) {
        return val;
    }
    llvm::Value *variable = namedValues[var->name];
    if (variable == nullptr) {
        return logError("variable undeclared");
    }
    builder->CreateStore(val, variable);
    return val;
}

// While

void While::print(ostream &output, int indent) const {
    output << string(indent, ' ') << "While (" << *cond << ")" << endl;
    stmt->print(output, indent + 4);
}

void While::traverse(SymbolTable &st) {
    st.enterScope();
    this->stmt->traverse(st);
    st.exitScope();
}

// Return

void Return::print(ostream &output, int indent) const {
    output << string(indent, ' ') << "Return " << *expr;
}

llvm::Value *Return::generateCode() {
    auto *val = expr->generateCode();
    builder->CreateRet(val);
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

void Conditional::traverse(SymbolTable &st) {
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

llvm::Value *Conditional::generateCode() {
    auto *cond = condition->generateCode();
    if (cond == nullptr) {
        return nullptr;
    }
    llvm::Function *func = builder->GetInsertBlock()->getParent();
    auto *ifBlock = llvm::BasicBlock::Create(*theContext, "if", func);
    auto *elseBlock = llvm::BasicBlock::Create(*theContext, "else");
    auto *mergeBlock = llvm::BasicBlock::Create(*theContext, "ifelsemerge");

    builder->CreateCondBr(cond, ifBlock, elseBlock);
    builder->SetInsertPoint(ifBlock);
    auto *ifVal = ifstmt->generateCode();
    if (ifVal == nullptr) {
        return nullptr;
    }
    builder->CreateBr(mergeBlock);
    ifBlock = builder->GetInsertBlock();

    func->getBasicBlockList().push_back(elseBlock);
    builder->SetInsertPoint(elseBlock);
    auto *elseVal = elsestmt->generateCode();
    if (elseVal == nullptr) {
        return nullptr;
    }
    builder->CreateBr(mergeBlock);
    elseBlock = builder->GetInsertBlock();

    func->getBasicBlockList().push_back(mergeBlock);
    builder->SetInsertPoint(mergeBlock);
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

void FunctionDeclaration::traverse(SymbolTable &st) {
    auto *ref = new Referent(ReferentType::Func, this->ret, 0, this);
    st.addSymbol(this->name, ref);
};

llvm::Function *FunctionDeclaration::generateCode() {
    vector<llvm::Type *> argTypes;
    if (arguments != nullptr) {
        for (auto it : *arguments) {
            argTypes.push_back(generateType(it->type));
        }
    }
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(generateType(ret), argTypes, false);
    llvm::Function *func = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, name, theModule.get());
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

void FunctionDefinition::traverse(SymbolTable &st) {
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

llvm::Function *FunctionDefinition::generateCode() {
    // theModule->dump();
    auto *func = theModule->getFunction("main");
    if (func == nullptr) {
        func = decl->generateCode();
    }
    if (!func->empty()) {
        return (llvm::Function *)logError("redefinition of " + decl->name);
    }
    // auto *func = decl->generateCode();

    llvm::BasicBlock *entryBlock =
        llvm::BasicBlock::Create(*theContext, "entry", func);
    builder->SetInsertPoint(entryBlock);
    namedValues.clear();

    for (auto &arg : func->args()) {
        auto *alloca =
            createEntryBlockAlloca(func, arg.getName(), arg.getType());
        builder->CreateStore(&arg, alloca);
        namedValues[arg.getName()] = alloca;
    }

    content->generateCode();
    if (func->getReturnType()->isVoidTy()) {
        builder->CreateRetVoid();
    }
    llvm::verifyFunction(*func);
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

llvm::Value *FunctionCall::generateCode() {
    llvm::Function *func = theModule->getFunction(function);
    if (func == nullptr) {
        return logError("undefined reference to " + function);
    }
    vector<llvm::Value *> args;
    for (auto arg : *arguments) {
        args.push_back(arg->generateCode());
    }
    return builder->CreateCall(func, args, "call");
};