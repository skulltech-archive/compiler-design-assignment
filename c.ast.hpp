#include <string>
#include <vector>

using namespace std;

class FunctionDefinition;
class Declaration;

enum class TypeSpecifier {Str, Int, Void};

class FunctionDefinition {
    public:
    TypeSpecifier ret;
    string name;
    vector<Declaration> arguments;
    // Statement content;
    
    friend ostream &operator<<(ostream &output, const FunctionDefinition &fn) {
        vector<string> typestrs {"void", "int", "str"};
        output << "function: " << typestrs.at(static_cast<int>(fn.ret)) << " " << fn.name; 
        return output;
    }
};

class Declaration {
    public:
    TypeSpecifier type;
    bool constant;
    string name;

    friend ostream &operator<<(ostream &output, const Declaration &decl) {
        vector<string> typestrs {"void", "int", "str"};
        cout << "declaration: " << typestrs.at(static_cast<int>(decl.type)) << " " << decl.name;
        return output;
    }
};

class Signature {
    public:
    string name;
    // vector<string> arguments;
    friend ostream &operator<<(ostream &output, const Signature &sig) {
        cout << "signature: " << sig.name;
        return output;
    }
};

class Expression;
class Statement;
class IntLiteral;
class StrLiteral;

// enum class DeclaratorType {Name, FuncSig};
// struct Declarator {
//     DeclaratorType type;
//     union {
//         string name;
//         FunctionSignature funcsig;
//     };
//     Declarator() {}
//     Declarator(const Declarator&decl) {}
//     ~Declarator() {}
// };

class BlockOfFunctions {
    public:
    vector<FunctionDefinition> block;
    
    friend ostream &operator<<(ostream &output, const BlockOfFunctions &block) {
        for (auto& it : block.block) {
            output << it; 
        }
        return output;
    }
};




class LiteralType {
    public:
    enum Node {Int, Str, Ident};
};

class Literal {
    public:
    LiteralType type;
};

class IntLiteral: Literal {
    public:
    int value;
};

class StrLiteral: Literal {
    public:
    string value;
};

class Variable: Literal {
    public:
    string name;
};

enum class UnaryOperator {BitwiseAnd, Star, Plus, Minus, Tilde, Exclaim, Or, And, Greater, Less, GreaterEqual, LessEqual};
class UnaryExpression {
    public:
    UnaryOperator op;
    Expression *left;
    Expression *right;
};

enum class ExpressionType {Lit, Unary};
class Expression {
    ExpressionType type;
    Literal *lit;
    UnaryExpression *unary;
};

enum class StatementType {Decl, Block, Func, Expr};
class Statement {
    public:
    StatementType type;
    Declaration *decl;
    vector<Statement> *block;
    FunctionDefinition *func;
};
