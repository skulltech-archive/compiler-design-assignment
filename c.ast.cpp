#include "c.ast.hpp"

#include <iostream>

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

void AST::print(ostream &output, int indent) const {
    for (auto it : *items) {
        output << *it << endl;
    }
}

void AST::traverse(SymbolTable &st) {
    st.enterScope();
    for (auto it : *items) {
        it->traverse(st);
        cout << st << endl;
    }
    st.exitScope();
};

void Declaration::print(ostream &output, int indent) const {
    output << string(indent, ' ') << type;
    if (type != TypeSpecifier::Ellipsis) {
        if (constant) {
            output << " const";
        }
        output << " " << *sig;
    }
}

ostream &operator<<(ostream &output, const Declaration &decl) {
    decl.print(output);
    return output;
}

void Declaration::traverse(SymbolTable &st) {
    Referent *ref;
    if (sig->arguments == nullptr) {
        ref = new Referent(ReferentType::Var, type, sig->pointers, this);
    } else {
        ref = new Referent(ReferentType::Func, type, sig->pointers, this);
    }
    st.addSymbol(sig->name, ref);
};

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

void IntLiteral::print(ostream &output, int indent) const {
    output << string(indent, ' ') << value;
}

llvm::Value *IntLiteral::generateCode() {
    return llvm::ConstantInt::get(TheContext, llvm::APInt(32, value, true));
};

void StrLiteral::print(ostream &output, int indent) const {
    output << string(indent, ' ') << str;
}

void Identifier::print(ostream &output, int indent) const {
    output << string(indent, ' ') << name;
}

llvm::Value *Identifier::generateCode() {
    llvm::Value *v = NamedValues[name];
    if (v == nullptr) {
        cout << "Error, variable " << name << " not found!";
    }
    return v;
};

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

void BinaryExpression::print(ostream &output, int indent) const {
    output << string(indent, ' ') << op << "(" << *left << ", " << *right
           << ")";
}

llvm::Value *BinaryExpression::generateCode() {
    auto *lv = left->generateCode();
    auto *rv = right->generateCode();
    switch (op) {
        case BinaryOperator::Multiply:
            return Builder.CreateMul(lv, rv, "Multiply");
            break;
        case BinaryOperator::Divide:
            return Builder.CreateSDiv(lv, rv, "Divide");
            break;
        case BinaryOperator::Plus:
            return Builder.CreateAdd(lv, rv, "Add");
            break;
        case BinaryOperator::Minus:
            return Builder.CreateSub(lv, rv, "Subtract");
            break;
        case BinaryOperator::Left:
            return Builder.CreateShl(lv, rv, "Left shift");
            break;
        case BinaryOperator::Right:
            return Builder.CreateLShr(lv, rv, "Right shift");
            break;
        case BinaryOperator::Less:
            return Builder.CreateICmpSLT(lv, rv, "Less");
            break;
        case BinaryOperator::Greater:
            return Builder.CreateICmpSGT(lv, rv, "Greater");
            break;
        case BinaryOperator::LessEqual:
            return Builder.CreateICmpSLE(lv, rv, "Less or Equal");
            break;
        case BinaryOperator::GreaterEqual:
            return Builder.CreateICmpSGE(lv, rv, "Greater or Equal");
            break;
        case BinaryOperator::Equal:
            return Builder.CreateICmpEQ(lv, rv, "Equal");
            break;
        case BinaryOperator::NotEqual:
            return Builder.CreateICmpNE(lv, rv, "Not Equal");
            break;
        case BinaryOperator::BitwiseAnd:
            return Builder.CreateAnd(lv, rv, "Bitwise And");
            break;
        case BinaryOperator::BitwiseXor:
            return Builder.CreateXor(lv, rv, "Bitwise Xor");
            break;
        case BinaryOperator::BitwiseOr:
            return Builder.CreateOr(lv, rv, "Bitwise Or");
            break;
        case BinaryOperator::And:
            return Builder.CreateAnd(lv, rv, "And");
            break;
        case BinaryOperator::Or:
            return Builder.CreateOr(lv, rv, "Or");
            break;
        default:
            break;
    }
};

void Assignment::print(ostream &output, int indent) const {
    output << string(indent, ' ');
    if (var != nullptr) {
        output << *var << " = ";
    }
    output << *expr;
}

void While::print(ostream &output, int indent) const {
    output << string(indent, ' ') << "While (" << *cond << ")" << endl;
    stmt->print(output, indent + 4);
}

void While::traverse(SymbolTable &st) {
    st.enterScope();
    this->stmt->traverse(st);
    st.exitScope();
}

void Return::print(ostream &output, int indent) const {
    output << string(indent, ' ') << "Return " << *expr;
}

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

void FunctionDefinition::print(ostream &output, int indent) const {
    output << string(indent, ' ') << "function " << ret << " " << name << " (";
    if (arguments != nullptr) {
        for (auto it : *arguments) {
            output << *it << ", ";
        }
    }
    output << ")" << endl;
    content->print(output, indent + 4);
};

void FunctionDefinition::traverse(SymbolTable &st) {
    auto *ref = new Referent(ReferentType::Func, this->ret, 0, this);
    st.addSymbol(this->name, ref);
    st.enterScope();
    if (arguments != nullptr) {
        for (auto it : *arguments) {
            it->traverse(st);
        }
    }
    content->traverse(st);
    cout << st << endl;
    st.exitScope();
}

llvm::Type *generateType(TypeSpecifier type) {
    switch (type) {
        case TypeSpecifier::Int:
            return llvm::Type::getInt32Ty(TheContext);
            break;
        case TypeSpecifier::Char:
            return llvm::Type::getInt32Ty(TheContext);
            break;
        case TypeSpecifier::Void:
            return llvm::Type::getVoidTy(TheContext);
            break;
        default:
            break;
    }
}

llvm::Function *FunctionDefinition::generateCode() {
    std::vector<llvm::Type *> argTypes{};
    for (auto it : *arguments) {
        argTypes.push_back(generateType(it->type));
    };
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(generateType(ret), argTypes, false);
    llvm::Function *func = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, name, TheModule.get());
    int index = 0;
    for (auto &it : func->args()) {
        it.setName(arguments->at(index++)->sig->name);
    };
    return func;
};

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