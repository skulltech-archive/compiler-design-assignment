#pragma once
#include <iostream>
#include <string>
#include <vector>

using namespace std;

class FunctionDefinition;
class Declaration;
class IntLiteral;
class Expression;
class Statement;
class IntLiteral;
class Variable;
class BlockItem;

enum class TypeSpecifier { Void, Int, Char };
ostream &operator<<(ostream &output, const TypeSpecifier &type);

class Declaration {
   public:
    TypeSpecifier type;
    bool constant;
    string name;

    friend ostream &operator<<(ostream &output, const Declaration &decl) {
        vector<string> typestrs{"void", "int", "char"};
        cout << decl.type << " " << decl.name;
        return output;
    }
};

enum class BlockItemType { Stmt, Decl };
class BlockItem {
   public:
    BlockItemType btype;
    virtual void print(ostream &output) const {};
    friend ostream &operator<<(ostream &output, const BlockItem &block) {
        block.print(output);
        return output;
    }
};

class FunctionDefinition {
   public:
    TypeSpecifier ret;
    string name;
    vector<Declaration> arguments;
    vector<BlockItem*> content;

    friend ostream &operator<<(ostream &output, const FunctionDefinition &fn) {
        vector<string> typestrs{"void", "int", "str"};
        output << "function " << typestrs.at(static_cast<int>(fn.ret)) << " "
               << fn.name << " (";
        for (auto &it : fn.arguments) {
            output << it << ", ";
        }
        cout << ")";
        for (auto it: fn.content) {
            cout << endl << "    " << *it;
        }
        return output;
    }
};

class Signature {
   public:
    string name;
    vector<Declaration> arguments;
    friend ostream &operator<<(ostream &output, const Signature &sig) {
        cout << sig.name << "(";
        for (auto &it : sig.arguments) {
            output << it << ", ";
        }
        cout << ")";
        return output;
    }
};

class BlockOfFunctions {
   public:
    vector<FunctionDefinition> block;

    friend ostream &operator<<(ostream &output, const BlockOfFunctions &block) {
        for (auto &it : block.block) {
            output << it << endl;
        }
        return output;
    }
};


enum class StatementType { Expr };
class Statement : public BlockItem {
   public:
    StatementType stype;
};

enum class ExpressionType { Lit, Unary, Assign };
ostream &operator<<(ostream &output, const ExpressionType &type);

class Expression : public Statement {
   public:
    ExpressionType etype;
    Expression(ExpressionType type) : etype(type) {}
};

enum class LiteralType { Int, Var };
ostream &operator<<(ostream &output, const LiteralType &type);

class Literal : public Expression {
   public:
    LiteralType ltype;
    Literal(LiteralType type) : Expression(ExpressionType::Lit), ltype(type){};
};

class IntLiteral : public Literal {
   public:
    int value;
    IntLiteral(int i) : Literal(LiteralType::Int), value(i) {}
    virtual void print(ostream &output) const { output << value; }
};

class Variable : public Literal {
   public:
    string name;
    Variable(string s) : Literal(LiteralType::Var), name(s) {}
    virtual void print(ostream &output) const { output << name; }
};

enum class UnaryOperator {
    Multiply,
    Divide,
    Plus,
    Minus,
    Left,
    Right,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
    NotEqual,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    And,
    Or
};
ostream &operator<<(ostream &output, const UnaryOperator &type);

class UnaryExpression : public Expression {
   public:
    UnaryOperator op;
    Expression *left;
    Expression *right;
    UnaryExpression(UnaryOperator o, Expression *l, Expression *r)
        : Expression(ExpressionType::Unary), op(o), left(l), right(r) {}
    virtual void print(ostream &output) const {
        output << op << "(" << *left << ", " << *right << ")";
    }
};

class Assignment : public Expression {
   public:
    Variable *var;
    Expression *expr;
    Assignment(Variable *v, Expression *e)
        : Expression(ExpressionType::Assign), var(v), expr(e) {}
    Assignment(Expression *e) : Expression(ExpressionType::Assign), expr(e) {}
    virtual void print(ostream &output) const {
        if (var != NULL) {
            output << *var << " = ";
        }
        output << *expr;
    }
};
