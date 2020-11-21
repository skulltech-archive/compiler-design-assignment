#include <string>
#include <vector>

using namespace std;

class FunctionDefinition;
class Expression;
class Statement;
class IntLiteral;
class StrLiteral;
class Declaration;

enum class TypeSpecifier {Void, Int, Str};

class FunctionDefinition {
    public:
    TypeSpecifier ret;
    string name;
    // vector<Declaration> *arguments;
    // Statement *content;
    
    friend ostream &operator<<(ostream &output, const FunctionDefinition &fn) {
        output << "function: " << static_cast<int>(fn.ret) << " " << fn.name << endl; 
        return output;
    }
};

class FunctionSignature {
    public:
    string name;
    vector<Declaration> *arguments;
};

class BlockOfFunctions {
    public:
    vector<FunctionDefinition> block;
    
    friend ostream &operator<<(ostream &output, const BlockOfFunctions &block) {
        for (auto& it : block.block) {
            output << it << endl; 
        }
        return output;
    }
};


class Declaration {
    public:
    TypeSpecifier type;
    bool constant;
    string name;
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

