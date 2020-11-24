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

ostream &operator<<(ostream &output, const AST &ast) {
    for (const auto &it : ast) {
        output << *it << endl;
    }
    return output;
}