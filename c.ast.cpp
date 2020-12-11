#include "c.ast.hpp"

#include <iostream>
#include <typeinfo>

#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

using namespace std;

ostream &operator<<(ostream &output, const TypeSpecifier &type) {
    const string stringreps[]{"void", "char",  "short", "int",
                              "long", "float", "double"};
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

// source https://stackoverflow.com/a/5612287/5837426
string unescape(const string &s) {
    string res;
    string::const_iterator it = s.begin();
    while (it != s.end()) {
        char c = *it++;
        if (c == '\\' && it != s.end()) {
            switch (*it++) {
                case '\\':
                    c = '\\';
                    break;
                case 'n':
                    c = '\n';
                    break;
                case 't':
                    c = '\t';
                    break;
                // all other escapes
                default:
                    // invalid escape sequence - skip it. alternatively you can
                    // copy it as is, throw an exception...
                    continue;
            }
        }
        res += c;
    }
    return res;
}

llvm::Type *generateType(TypeSpecifier type, llvm::LLVMContext &context,
                         int pointers) {
    llvm::Type *ty;
    switch (type) {
        case TypeSpecifier::Void:
            ty = llvm::Type::getVoidTy(context);
            break;
        case TypeSpecifier::Char:
            ty = llvm::Type::getInt8Ty(context);
            break;
        case TypeSpecifier::Short:
            ty = llvm::Type::getInt16Ty(context);
            break;
        case TypeSpecifier::Int:
            ty = llvm::Type::getInt32Ty(context);
            break;
        case TypeSpecifier::Long:
            ty = llvm::Type::getInt64Ty(context);
            break;
        case TypeSpecifier::Float:
            ty = llvm::Type::getFloatTy(context);
            break;
        case TypeSpecifier::Double:
            ty = llvm::Type::getDoubleTy(context);
            break;
        default:
            break;
    }
    for (unsigned i = 0; i < pointers; ++i) {
        ty = ty->getPointerTo(0);
    }
    return ty;
}

void emitCode(CodeKit &kit) {
    llvm::verifyModule(kit.module, &llvm::outs());
    kit.module.print(llvm::outs(), nullptr);
    auto tirFile = "output.ll";
    error_code ec;
    llvm::raw_fd_ostream tirFileStream(tirFile, ec, llvm::sys::fs::F_None);
    kit.module.print(tirFileStream, nullptr);
    tirFileStream.flush();

    auto irFile = "output.bc";
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
    auto rm = llvm::Reloc::PIC_;
    // auto rm = llvm::Optional<llvm::Reloc::Model>();
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

void AST::reproduce(ostream &output, int indent) const {
    for (auto it : *items) {
        output << *it << endl;
    }
}

void AST::print(ostream &output, const string prefix, bool isFirst) const {
    output << prefix;
    output << (isFirst ? "├──" : "└──");
    output << "AST" << endl;
    for (auto it : *items) {
        if (it == items->front()) {
            it->print(output, prefix + (isFirst ? "│   " : "    "), true);
        } else {
            it->print(output, prefix + (isFirst ? "│   " : "    "), false);
        }
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

void Declaration::reproduce(ostream &output, int indent) const {
    output << string(indent, ' ') << type;
    if (!ellipsis) {
        if (constant) {
            output << " const";
        }
        output << " " << *sig;
    }
}

void Declaration::print(ostream &output, const string prefix,
                        bool isFirst) const {
    output << prefix;
    output << (isFirst ? "├──" : "└──");
    output << "Declaration(" << (constant ? "constant " : "") << type
           << (ellipsis ? "..." : "") << ")" << endl;
    sig->print(output, prefix + (isFirst ? "│   " : "    "), true);
}

void Declaration::traverse(SymbolTable<Referent> &st) {
    auto *ref = new Referent(ReferentType::Var, type, sig->pointers, this);
    st.addSymbol(sig->name, ref);
};

llvm::Value *Declaration::generateCode(CodeKit &kit) {
    llvm::Function *func = kit.builder.GetInsertBlock()->getParent();
    llvm::Type *t = generateType(type, kit.context, sig->pointers);
    auto *val = llvm::Constant::getNullValue(t);
    auto *alloca = createEntryBlockAlloca(func, sig->name, t);
    kit.builder.CreateStore(val, alloca);
    kit.symbolTable.addSymbol(sig->name, alloca);
    return val;
}

// Signature

void Signature::reproduce(ostream &output, int indent) const {
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

void Signature::print(ostream &output, const string prefix,
                      bool isFirst) const {
    output << prefix;
    output << (isFirst ? "├──" : "└──");
    output << "Signature(" << string("*", pointers) << name << ")" << endl;
    if (arguments != nullptr) {
        for (auto it : *arguments) {
            if (it == arguments->front()) {
                it->print(output, prefix + (isFirst ? "│   " : "    "), true);
            } else {
                it->print(output, prefix + (isFirst ? "│   " : "    "), false);
            }
        }
    }
}

// Literal

void IntLiteral::reproduce(ostream &output, int indent) const {
    output << string(indent, ' ') << value;
}

void IntLiteral::print(ostream &output, const string prefix,
                       bool isFirst) const {
    output << prefix;
    output << (isFirst ? "├──" : "└──");
    output << "IntLiteral(" << value << ")" << endl;
}

llvm::Value *IntLiteral::generateCode(CodeKit &kit) {
    return llvm::ConstantInt::get(kit.context, llvm::APInt(32, value, true));
};

void FloatLiteral::reproduce(ostream &output, int indent) const {
    output << string(indent, ' ') << value;
}

void FloatLiteral::print(ostream &output, const string prefix,
                         bool isFirst) const {
    output << prefix;
    output << (isFirst ? "├──" : "└──");
    output << "FloatLiteral(" << value << ")" << endl;
}

llvm::Value *FloatLiteral::generateCode(CodeKit &kit) {
    return llvm::ConstantFP::get(kit.context, llvm::APFloat(value));
};

void StrLiteral::reproduce(ostream &output, int indent) const {
    output << string(indent, ' ') << str;
}

void StrLiteral::print(ostream &output, const string prefix,
                       bool isFirst) const {
    output << prefix;
    output << (isFirst ? "├──" : "└──");
    output << "IntLiteral(" << str << ")" << endl;
}

// source https://stackoverflow.com/a/51811344/5837426
llvm::Value *StrLiteral::generateCode(CodeKit &kit) {
    string str = unescape(this->str);
    auto charType = generateType(TypeSpecifier::Char, kit.context);
    vector<llvm::Constant *> chars(str.length());
    for (unsigned i = 0; i < str.size(); ++i) {
        chars[i] = llvm::ConstantInt::get(charType, str[i]);
    }
    chars.push_back(llvm::ConstantInt::get(charType, 0));
    auto stringType = llvm::ArrayType::get(charType, chars.size());

    auto globalDecl = (llvm::GlobalVariable *)kit.module.getOrInsertGlobal(
        ".str", stringType);
    globalDecl->setInitializer(llvm::ConstantArray::get(stringType, chars));
    globalDecl->setConstant(true);
    globalDecl->setLinkage(llvm::GlobalValue::LinkageTypes::PrivateLinkage);
    globalDecl->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

    return llvm::ConstantExpr::getBitCast(globalDecl, charType->getPointerTo());
}

void Identifier::reproduce(ostream &output, int indent) const {
    output << string(indent, ' ') << name;
}

void Identifier::print(ostream &output, const string prefix,
                       bool isFirst) const {
    output << prefix;
    output << (isFirst ? "├──" : "└──");
    output << "Identifier('" << name << "')" << endl;
}

llvm::Value *Identifier::generateCode(CodeKit &kit) {
    llvm::Value *v = kit.symbolTable.findSymbol(name);
    if (v == nullptr) {
        return logError("variable undeclared");
    }
    return kit.builder.CreateLoad(v, name.c_str());
};

// CompoundStatement

void CompoundStatement::reproduce(ostream &output, int indent) const {
    output << string(indent, ' ') << "{" << endl;
    for (auto it : *items) {
        it->reproduce(output, indent + 4);
        output << endl;
    }
    output << string(indent, ' ') << "}";
}

void CompoundStatement::print(ostream &output, const string prefix,
                              bool isFirst) const {
    output << prefix;
    output << (isFirst ? "├──" : "└──");
    output << "CompoundStatement" << endl;
    for (auto it : *items) {
        if (it == items->front()) {
            it->print(output, prefix + (isFirst ? "│   " : "    "), true);
        } else {
            it->print(output, prefix + (isFirst ? "│   " : "    "), false);
        }
    }
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

// Binary Expression

void BinaryExpression::reproduce(ostream &output, int indent) const {
    output << string(indent, ' ') << op << "(" << *left << ", " << *right
           << ")";
}

void BinaryExpression::print(ostream &output, const string prefix,
                             bool isFirst) const {
    output << prefix;
    output << (isFirst ? "├──" : "└──");
    output << "BinaryExpression(" << op << ")" << endl;
    left->print(output, prefix + (isFirst ? "│   " : "    "), true);
    right->print(output, prefix + (isFirst ? "│   " : "    "), false);
}

llvm::Value *BinaryExpression::generateCode(CodeKit &kit) {
    auto *lv = left->generateCode(kit);
    auto *rv = right->generateCode(kit);
    switch (op) {
        case BinaryOperator::Multiply:
            return kit.builder.CreateMul(lv, rv, "mul");
        case BinaryOperator::Divide:
            return kit.builder.CreateSDiv(lv, rv, "div");
            break;
        case BinaryOperator::Plus:
            if (lv->getType()->isFloatingPointTy()) {
                return kit.builder.CreateFAdd(lv, rv, "add");
            } else {
                return kit.builder.CreateAdd(lv, rv, "add");
            }
            break;
        case BinaryOperator::Minus:
            return kit.builder.CreateSub(lv, rv, "sub");
            break;
        case BinaryOperator::Left:
            return kit.builder.CreateShl(lv, rv, "lshift");
            break;
        case BinaryOperator::Right:
            return kit.builder.CreateLShr(lv, rv, "rshift");
            break;
        case BinaryOperator::Less:
            return kit.builder.CreateICmpSLT(lv, rv, "less");
            break;
        case BinaryOperator::Greater:
            return kit.builder.CreateICmpSGT(lv, rv, "greater");
            break;
        case BinaryOperator::LessEqual:
            return kit.builder.CreateICmpSLE(lv, rv, "lesseq");
            break;
        case BinaryOperator::GreaterEqual:
            return kit.builder.CreateICmpSGE(lv, rv, "greatereq");
            break;
        case BinaryOperator::Equal:
            return kit.builder.CreateICmpEQ(lv, rv, "eq");
            break;
        case BinaryOperator::NotEqual:
            return kit.builder.CreateICmpNE(lv, rv, "noteq");
            break;
        case BinaryOperator::BitwiseAnd:
            return kit.builder.CreateAnd(lv, rv, "bitwiseand");
            break;
        case BinaryOperator::BitwiseXor:
            return kit.builder.CreateXor(lv, rv, "bitwisexor");
            break;
        case BinaryOperator::BitwiseOr:
            return kit.builder.CreateOr(lv, rv, "bitwiseor");
            break;
        case BinaryOperator::And:
            return kit.builder.CreateAnd(lv, rv, "and");
            break;
        case BinaryOperator::Or:
            return kit.builder.CreateOr(lv, rv, "or");
            break;
        default:
            break;
    }
};

// Cast Expression

void CastExpression::reproduce(ostream &output, int indent) const {
    output << string(indent, ' ') << "(" << type << ")";
    expr->reproduce(output);
}

void CastExpression::print(ostream &output, const string prefix,
                           bool isFirst) const {
    output << prefix;
    output << (isFirst ? "├──" : "└──");
    output << "CastExpression(" << type << ")" << endl;
    expr->print(output, prefix + (isFirst ? "│   " : "    "), true);
}

llvm::Value *CastExpression::generateCode(CodeKit &kit) {
    auto *val = expr->generateCode(kit);
    llvm::Instruction::CastOps ops;
    switch (type) {
        case TypeSpecifier::Double:
            if (val->getType()->isFloatingPointTy()) {
                ops = llvm::Instruction::CastOps::FPExt;
            } else if(val->getType()->isIntegerTy()) {
                ops = llvm::Instruction::CastOps::SIToFP;
            } else {
                return nullptr;
            }
            break;
        case TypeSpecifier::Float:
            if (val->getType()->isFloatingPointTy()) {
                ops = llvm::Instruction::CastOps::FPTrunc;
            } else if(val->getType()->isIntegerTy()) {
                ops = llvm::Instruction::CastOps::SIToFP;
            } else {
                return nullptr;
            }
            break;
        case TypeSpecifier::Int:
            if (val->getType()->isFloatingPointTy()) {
                ops = llvm::Instruction::CastOps::FPToSI;
            } else {
                return val;
            }
        default:
            return nullptr;
            break;
    }
    return kit.builder.CreateCast(ops, val, generateType(type, kit.context));
};

// Assignment

void Assignment::reproduce(ostream &output, int indent) const {
    output << string(indent, ' ');
    if (var != nullptr) {
        output << *var << " = ";
    }
    output << *expr;
}

void Assignment::print(ostream &output, const string prefix,
                       bool isFirst) const {
    output << prefix;
    output << (isFirst ? "├──" : "└──");
    output << "Assignment" << endl;
    if (var != nullptr) {
        var->print(output, prefix + (isFirst ? "│   " : "    "), true);
    }
    expr->print(output, prefix + (isFirst ? "│   " : "    "), false);
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

void While::reproduce(ostream &output, int indent) const {
    output << string(indent, ' ') << "While (" << *cond << ")" << endl;
    stmt->reproduce(output, indent + 4);
}

void While::print(ostream &output, const string prefix, bool isFirst) const {
    output << prefix;
    output << (isFirst ? "├──" : "└──");
    output << "While" << endl;
    cond->print(output, prefix + (isFirst ? "│   " : "    "), true);
    stmt->print(output, prefix + (isFirst ? "│   " : "    "), false);
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

void Return::reproduce(ostream &output, int indent) const {
    output << string(indent, ' ') << "Return " << *expr;
}

void Return::print(ostream &output, const string prefix, bool isFirst) const {
    output << prefix;
    output << (isFirst ? "├──" : "└──");
    output << "Return" << endl;
    expr->print(output, prefix + (isFirst ? "│   " : "    "), true);
}

llvm::Value *Return::generateCode(CodeKit &kit) {
    auto *val = expr->generateCode(kit);
    kit.builder.CreateRet(val);
}

// Conditional

void Conditional::reproduce(ostream &output, int indent) const {
    output << string(indent, ' ') << "If (" << *condition << ")" << endl;
    ifstmt->reproduce(output, indent + 4);
    if (elsestmt != nullptr) {
        output << endl << string(indent, ' ') << "Else" << endl;
        elsestmt->reproduce(output, indent + 4);
    }
}

void Conditional::print(ostream &output, const string prefix,
                        bool isFirst) const {
    output << prefix;
    output << (isFirst ? "├──" : "└──");
    output << "Conditional" << endl;
    ifstmt->print(output, prefix + (isFirst ? "│   " : "    "), true);
    if (elsestmt != nullptr) {
        ifstmt->print(output, prefix + (isFirst ? "│   " : "    "), false);
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
    bool mergeUsed = false;
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
        mergeUsed = true;
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
        mergeUsed = true;
    }

    if (mergeUsed) {
        func->getBasicBlockList().push_back(mergeBlock);
        kit.builder.SetInsertPoint(mergeBlock);
    }

    return nullptr;
}

// FunctionDeclaration

void FunctionDeclaration::reproduce(ostream &output, int indent) const {
    output << string(indent, ' ') << "function " << ret << " " << name << " (";
    if (arguments != nullptr) {
        for (auto it : *arguments) {
            output << *it << ", ";
        }
    }
    output << ")";
};

void FunctionDeclaration::print(ostream &output, const string prefix,
                                bool isFirst) const {
    output << prefix;
    output << (isFirst ? "├──" : "└──");
    output << "FunctionDeclaration(" << ret << " " << name
           << (varargs ? " varargs" : "") << ")" << endl;
    if (arguments != nullptr) {
        for (auto it : *arguments) {
            if (it == arguments->front()) {
                it->print(output, prefix + (isFirst ? "│   " : "    "), true);
            } else {
                it->print(output, prefix + (isFirst ? "│   " : "    "), false);
            }
        }
    }
}

void FunctionDeclaration::traverse(SymbolTable<Referent> &st) {
    auto *ref = new Referent(ReferentType::Func, this->ret, 0, this);
    st.addSymbol(this->name, ref);
};

llvm::Function *FunctionDeclaration::generateCode(CodeKit &kit) {
    vector<llvm::Type *> argTypes;
    if (arguments != nullptr) {
        for (auto it : *arguments) {
            argTypes.push_back(
                generateType(it->type, kit.context, it->sig->pointers));
        }
    }
    llvm::FunctionType *funcType = llvm::FunctionType::get(
        generateType(ret, kit.context), argTypes, varargs);
    llvm::Function *func = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, name, &kit.module);
    int index = 0;
    for (auto &it : func->args()) {
        it.setName(arguments->at(index++)->sig->name);
    };
    return func;
};

// FunctionDefinition

void FunctionDefinition::reproduce(ostream &output, int indent) const {
    output << string(indent, ' ') << *decl << endl;
    content->reproduce(output, indent + 4);
};

void FunctionDefinition::print(ostream &output, const string prefix,
                               bool isFirst) const {
    output << prefix;
    output << (isFirst ? "├──" : "└──");
    output << "FunctionDefinition" << endl;
    decl->print(output, prefix + (isFirst ? "│   " : "    "), true);
    content->print(output, prefix + (isFirst ? "│   " : "    "), false);
}

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

void FunctionCall::reproduce(ostream &output, int indent) const {
    output << string(indent, ' ') << function << "(";
    for (auto it : *arguments) {
        output << *it;
        if (it != arguments->back()) {
            output << ", ";
        }
    }
    output << ")";
}

void FunctionCall::print(ostream &output, const string prefix,
                         bool isFirst) const {
    output << prefix;
    output << (isFirst ? "├──" : "└──");
    output << "FunctionCall(" << function << ")" << endl;
    if (arguments != nullptr) {
        for (auto it : *arguments) {
            if (it == arguments->front()) {
                it->print(output, prefix + (isFirst ? "│   " : "    "), true);
            } else {
                it->print(output, prefix + (isFirst ? "│   " : "    "), false);
            }
        }
    }
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