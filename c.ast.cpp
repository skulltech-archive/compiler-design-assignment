#include "c.ast.hpp"

#include <iostream>

using namespace std;

ostream &operator<<(ostream &output, const TypeSpecifier &type) {
    const string stringreps[]{"void", "int", "char", "..."};
    cout << stringreps[static_cast<int>(type)];
    return output;
}

ostream &operator<<(ostream &output, const ExpressionType &type) {
    const string stringreps[]{"literal", "unary"};
    cout << stringreps[static_cast<int>(type)];
    return output;
}

ostream &operator<<(ostream &output, const UnaryOperator &type) {
    const string stringreps[]{
        "Multiply", "Divide",   "Plus",       "Minus",      "Left",
        "Right",    "Less",     "Greater",    "LessEqual",  "GreaterEqual",
        "Equal",    "NotEqual", "BitwiseAnd", "BitwiseXor", "BitwiseOr",
        "And",      "Or"};
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
    switch (type) {
        case TypeSpecifier::Int:
            ref = new Referent(ReferentType::Int, this);
            break;
        case TypeSpecifier::Char:
            ref = new Referent(ReferentType::Char, this);
            break;
        default:
            break;
    }
    if (ref != NULL) {
        st.addSymbol(sig->name, ref);
    }
};

void Signature::print(ostream &output, int indent) const {
    output << string(indent, ' ') << name << "(";
    if (arguments != NULL) {
        for (auto it : *arguments) {
            output << *it;
            if (it != arguments->back()) {
                output << ", ";
            }
        }
    }
    output << ")";
};

void CompoundStatement::print(ostream &output, int indent) const {
    output << string(indent, ' ') << "{" << endl;
    for (auto it : *items) {
        it->print(output, indent + 4);
        output << endl;
    }
    output << string(indent, ' ') << "}";
}

void UnaryExpression::print(ostream &output, int indent) const {
    output << string(indent, ' ') << op << "(" << *left << ", " << *right
           << ")";
}

void Assignment::print(ostream &output, int indent) const {
    output << string(indent, ' ');
    if (var != NULL) {
        output << *var << " = ";
    }
    output << *expr;
}

void While::print(ostream &output, int indent) const {
    output << string(indent, ' ') << "While (" << *cond << ")" << endl;
    stmt->print(output, indent + 4);
}
void Return::print(ostream &output, int indent) const {
    output << string(indent, ' ') << "Return " << *expr;
}

void Conditional::print(ostream &output, int indent) const {
    output << string(indent, ' ') << "If (" << *condition << ")" << endl;
    ifstmt->print(output, indent + 4);
    if (elsestmt != NULL) {
        output << endl << string(indent, ' ') << "Else" << endl;
        elsestmt->print(output, indent + 4);
    }
}

void FunctionDefinition::print(ostream &output, int indent) const {
    output << string(indent, ' ') << "function " << ret << " " << name << " (";
    if (arguments != NULL) {
        for (auto it : *arguments) {
            output << *it << ", ";
        }
    }
    output << ")" << endl;
    content->print(output, indent + 4);
};
void FunctionDefinition::traverse(SymbolTable &st) {
    auto *ref = new Referent(ReferentType::Func, this);
    st.addSymbol(this->name, ref);
    st.enterScope();
    for (auto it : *arguments) {
        it->traverse(st);
    }
    content->traverse(st);
    st.exitScope();
}

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