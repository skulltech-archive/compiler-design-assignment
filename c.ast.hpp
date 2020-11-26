#pragma once
#include <iostream>
#include <string>
#include <tuple>
#include <vector>

#include "c.sym.hpp"

using namespace std;

class Signature;

enum class TypeSpecifier { Void, Int, Char, Ellipsis };
ostream &operator<<(ostream &output, const TypeSpecifier &type);

using DeclSpecifier = pair<TypeSpecifier, bool>;

class Node {
   public:
    virtual void print(ostream &output, int indent = 0) const {};
    friend ostream &operator<<(ostream &output, const Node &node) {
        node.print(output);
        return output;
    }
    virtual void traverse(SymbolTable &st){};
};

class External : virtual public Node {};

class AST : public Node {
   public:
    vector<External *> *items;
    AST() { items = new vector<External *>; }
    virtual void print(ostream &output, int indent = 0) const;
    virtual void traverse(SymbolTable &st);
};

class BlockItem : virtual public Node {};

class Declaration : public BlockItem, public External {
   public:
    TypeSpecifier type;
    bool constant;
    Signature *sig;
    Declaration(TypeSpecifier t) : type(t) {}
    Declaration(TypeSpecifier t, bool c, Signature *s)
        : type(t), constant(c), sig(s) {}
    virtual void print(ostream &output, int indent = 0) const;
    friend ostream &operator<<(ostream &output, const Declaration &decl);
    virtual void traverse(SymbolTable &st);
};

class Signature : public Node {
   public:
    int pointers;
    string name;
    vector<Declaration *> *arguments;
    Signature(string n) : pointers(0), name(n) {}
    virtual void print(ostream &output, int indent = 0) const;
};

class Ellipsis : public Declaration {
   public:
    Ellipsis() : Declaration(TypeSpecifier::Ellipsis) {}
    virtual void print(ostream &output, int indent = 0) const {
        output << string(indent, ' ') << "...";
    }
};

class Statement : public BlockItem {};

class CompoundStatement : public Statement {
   public:
    vector<BlockItem *> *items;
    CompoundStatement() { items = new vector<BlockItem *>(); }
    CompoundStatement(vector<BlockItem *> *i) : items(i) {}
    virtual void print(ostream &output, int indent = 0) const;
    virtual void traverse(SymbolTable &st);
};

class Expression : public Statement {};

class Literal : public Expression {};

class IntLiteral : public Literal {
   public:
    int value;
    IntLiteral(int i) : value(i) {}
    virtual void print(ostream &output, int indent = 0) const {
        output << string(indent, ' ') << value;
    }
};

class StrLiteral : public Literal {
   public:
    string str;
    StrLiteral(string s) : str(s) {}
    virtual void print(ostream &output, int indent = 0) const {
        output << string(indent, ' ') << str;
    }
};

class Identifier : public Literal {
   public:
    string name;
    Identifier(string s) : name(s) {}
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
        : op(o), left(l), right(r) {}
    virtual void print(ostream &output, int indent = 0) const;
};

class Assignment : public Expression {
   public:
    Identifier *var;
    Expression *expr;
    Assignment(Identifier *v, Expression *e) : var(v), expr(e) {}
    Assignment(Expression *e) : expr(e) {}
    virtual void print(ostream &output, int indent = 0) const;
};

class Conditional : public Statement {
   public:
    Expression *condition;
    Statement *ifstmt;
    Statement *elsestmt;
    Conditional(Expression *c, Statement *i, Statement *e)
        : condition(c), ifstmt(i), elsestmt(e) {}
    Conditional(Expression *c, Statement *i) : condition(c), ifstmt(i) {}
    virtual void print(ostream &output, int indent = 0) const;
    virtual void traverse(SymbolTable &st);
};

class While : public Statement {
   public:
    Expression *cond;
    Statement *stmt;
    While(Expression *c, Statement *s) : cond(c), stmt(s) {}
    virtual void print(ostream &output, int indent = 0) const;
    virtual void traverse(SymbolTable &st);
};

class Return : public Statement {
   public:
    Expression *expr;
    Return(Expression *e) : expr(e) {}
    virtual void print(ostream &output, int indent = 0) const;
};

class FunctionDefinition : public External {
   public:
    TypeSpecifier ret;
    string name;
    vector<Declaration *> *arguments;
    CompoundStatement *content;
    FunctionDefinition(TypeSpecifier t, string n, vector<Declaration *> *a,
                       CompoundStatement *c)
        : ret(t), name(n), arguments(a), content(c) {}
    virtual void print(ostream &output, int indent = 0) const;
    virtual void traverse(SymbolTable &st);
};

class FunctionCall : public Expression {
   public:
    string function;
    vector<Expression *> *arguments;
    FunctionCall(string f, vector<Expression *> *a)
        : function(f), arguments(a) {}
    virtual void print(ostream &output, int indent = 0) const;
};
