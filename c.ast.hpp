#pragma once
#include <iostream>
#include <string>
#include <tuple>
#include <vector>

using namespace std;

class FunctionDefinition;
class Declaration;
class IntLiteral;
class Expression;
class Statement;
class IntLiteral;
class Identifier;
class BlockItem;

enum class TypeSpecifier { Void, Int, Char, Ellipsis };
ostream &operator<<(ostream &output, const TypeSpecifier &type);

using DeclSpecifier = pair<TypeSpecifier, bool>;

enum class BlockItemType { Stmt, Decl };
class BlockItem {
   public:
    BlockItemType btype;
    virtual void print(ostream &output, int indent = 0) const {};
    friend ostream &operator<<(ostream &output, const BlockItem &block) {
        block.print(output);
        return output;
    }
};

class Declaration : public BlockItem {
   public:
    TypeSpecifier type;
    bool constant;
    string name;
    Declaration(TypeSpecifier t, bool c = false, string n = "")
        : type(t), constant(c), name(n) {}
    virtual void print(ostream &output, int indent = 0) const {
        output << string(indent, ' ') << type;
        if (type != TypeSpecifier::Ellipsis) {
            if (constant) {
                output << " const";
            }
            output << " " << name;
        }
    }
};

class Ellipsis : public Declaration {
   public:
    Ellipsis() : Declaration(TypeSpecifier::Ellipsis) {}
    virtual void print(ostream &output, int indent = 0) const {
        output << string(indent, ' ') << "...";
    }
};

class Signature {
   public:
    string name;
    vector<Declaration *> *arguments;
    friend ostream &operator<<(ostream &output, const Signature &sig) {
        sig.print(output);
        return output;
    }
    virtual void print(ostream &output, int indent = 0) const {
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
};

enum class StatementType { Expr };
class Statement : public BlockItem {
   public:
    StatementType stype;
};

class CompoundStatement : public Statement {
   public:
    vector<BlockItem *> *items;
    CompoundStatement() { items = new vector<BlockItem *>(); }
    CompoundStatement(vector<BlockItem *> *i) : items(i) {}
    virtual void print(ostream &output, int indent = 0) const {
        for (auto it : *items) {
            it->print(output, indent + 4);
            if (it != items->back()) {
                output << endl;
            }
        }
    }
};

enum class ExpressionType { Lit, Unary, Assign, FnCall };
ostream &operator<<(ostream &output, const ExpressionType &type);

class Expression : public Statement {
   public:
    ExpressionType etype;
    Expression(ExpressionType type) : etype(type) {}
};

enum class LiteralType { Int, Var, Str };
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
    virtual void print(ostream &output, int indent = 0) const {
        output << string(indent, ' ') << value;
    }
};

class StrLiteral : public Literal {
   public:
    string str;
    StrLiteral(string s) : Literal(LiteralType::Str), str(s) {}
    virtual void print(ostream &output, int indent = 0) const {
        output << string(indent, ' ') << str;
    }
};

class Identifier : public Literal {
   public:
    string name;
    Identifier(string s) : Literal(LiteralType::Var), name(s) {}
    virtual void print(ostream &output, int indent = 0) const {
        output << string(indent, ' ') << name;
    }
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
    virtual void print(ostream &output, int indent = 0) const {
        output << string(indent, ' ') << op << "(" << *left << ", " << *right
               << ")";
    }
};

class Assignment : public Expression {
   public:
    Identifier *var;
    Expression *expr;
    Assignment(Identifier *v, Expression *e)
        : Expression(ExpressionType::Assign), var(v), expr(e) {}
    Assignment(Expression *e) : Expression(ExpressionType::Assign), expr(e) {}
    virtual void print(ostream &output, int indent = 0) const {
        output << string(indent, ' ');
        if (var != NULL) {
            output << *var << " = ";
        }
        output << *expr;
    }
};

class Conditional : public Statement {
   public:
    Expression *condition;
    Statement *ifstmt;
    Statement *elsestmt;
    Conditional(Expression *c, Statement *i, Statement *e)
        : condition(c), ifstmt(i), elsestmt(e) {}
    Conditional(Expression *c, Statement *i) : condition(c), ifstmt(i) {}
    virtual void print(ostream &output, int indent = 0) const {
        output << string(indent, ' ') << "If (" << *condition << ")" << endl;
        ifstmt->print(output, indent + 4);
        if (elsestmt != NULL) {
            output << endl << string(indent, ' ') << "Else" << endl;
            elsestmt->print(output, indent + 4);
        }
    }
};

class While : public Statement {
   public:
    Expression *cond;
    Statement *stmt;
    While(Expression *c, Statement *s) : cond(c), stmt(s) {}
    virtual void print(ostream &output, int indent = 0) const {
        output << string(indent, ' ') << "While (" << *cond << ")" << endl;
        stmt->print(output, indent + 4);
    }
};

class Return : public Statement {
   public:
    Expression *expr;
    Return(Expression *e) : expr(e) {}
    virtual void print(ostream &output, int indent = 0) const {
        output << string(indent, ' ') << "Return " << *expr;
    }
};

class FunctionDefinition {
   public:
    TypeSpecifier ret;
    string name;
    vector<Declaration *> *arguments;
    CompoundStatement *content;
    FunctionDefinition(TypeSpecifier t, string n, vector<Declaration *> *a,
                       CompoundStatement *c)
        : ret(t), name(n), arguments(a), content(c) {}
    friend ostream &operator<<(ostream &output, const FunctionDefinition &fn) {
        fn.print(output);
        return output;
    }
    virtual void print(ostream &output, int indent = 0) const {
        output << string(indent, ' ') << "function " << ret << " " << name
               << " (";
        if (arguments != NULL) {
            for (auto it : *arguments) {
                output << *it << ", ";
            }
        }
        output << ")";
        if (content != NULL) {
            output << endl;
            content->print(output, indent + 4);
        }
    };
};

class FunctionCall : public Expression {
   public:
    string function;
    vector<Expression *> *arguments;
    FunctionCall(string f, vector<Expression *> *a)
        : Expression(ExpressionType::FnCall), function(f), arguments(a) {}
    virtual void print(ostream &output, int indent = 0) const {
        output << string(indent, ' ') << function << "(";
        for (auto it : *arguments) {
            output << *it;
            if (it != arguments->back()) {
                output << ", ";
            }
        }
        output << ")";
    }
};
