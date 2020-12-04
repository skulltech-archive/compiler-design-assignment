#pragma once
#include <iostream>
#include <map>
#include <stack>
#include <string>
#include <tuple>
#include <vector>

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

using namespace std;

enum class TypeSpecifier { Void, Int, Char, Ellipsis };
ostream &operator<<(ostream &output, const TypeSpecifier &type);

enum ReferentType { Func, Var };
ostream &operator<<(ostream &output, const ReferentType &type);

using DeclSpecifier = pair<TypeSpecifier, bool>;

class Signature;
class Referent;

template <typename T>
class SymbolTable;

struct CodeKit;

class Node {
   public:
    virtual void print(ostream &output, int indent = 0) const {};
    friend ostream &operator<<(ostream &output, const Node &node) {
        node.print(output);
        return output;
    }
    virtual void traverse(SymbolTable<Referent> &st){};
    virtual llvm::Value *generateCode(CodeKit &kit){};
};

class External : public Node {
   public:
    virtual llvm::Function *generateCode(CodeKit &kit){};
};

class AST : public Node {
   public:
    vector<External *> *items;
    AST() { items = new vector<External *>; }
    virtual void print(ostream &output, int indent = 0) const;
    virtual void traverse(SymbolTable<Referent> &st);
    virtual llvm::Value *generateCode(CodeKit &kit);
};

class BlockItem : public Node {
   public:
    virtual llvm::Value *generateCode(CodeKit &kit){};
};

class Declaration : public BlockItem {
   public:
    TypeSpecifier type;
    bool constant;
    Signature *sig;
    Declaration(TypeSpecifier t) : type(t) {}
    Declaration(TypeSpecifier t, bool c, Signature *s)
        : type(t), constant(c), sig(s) {}
    virtual void print(ostream &output, int indent = 0) const;
    virtual void traverse(SymbolTable<Referent> &st);
    virtual llvm::Value *generateCode(CodeKit &kit);
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

class Statement : public BlockItem {
   public:
    virtual llvm::Value *generateCode(CodeKit &kit){};
};

class CompoundStatement : public Statement {
   public:
    vector<BlockItem *> *items;
    CompoundStatement() { items = new vector<BlockItem *>(); }
    CompoundStatement(vector<BlockItem *> *i) : items(i) {}
    virtual void print(ostream &output, int indent = 0) const;
    virtual void traverse(SymbolTable<Referent> &st);
    virtual llvm::Value *generateCode(CodeKit &kit);
};

class Expression : public Statement {
   public:
    virtual llvm::Value *generateCode(CodeKit &kit){};
};

class Literal : public Expression {};

class IntLiteral : public Literal {
   public:
    int value;
    IntLiteral(int i) : value(i) {}
    virtual void print(ostream &output, int indent = 0) const;
    virtual llvm::Value *generateCode(CodeKit &kit);
};

class StrLiteral : public Literal {
   public:
    string str;
    StrLiteral(string s) : str(s) {}
    virtual void print(ostream &output, int indent = 0) const;
};

class Identifier : public Literal {
   public:
    string name;
    Identifier(string s) : name(s) {}
    virtual void print(ostream &output, int indent = 0) const;
    virtual llvm::Value *generateCode(CodeKit &kit);
};

enum class BinaryOperator {
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
ostream &operator<<(ostream &output, const BinaryOperator &type);

class BinaryExpression : public Expression {
   public:
    BinaryOperator op;
    Expression *left;
    Expression *right;
    BinaryExpression(BinaryOperator o, Expression *l, Expression *r)
        : op(o), left(l), right(r) {}
    virtual void print(ostream &output, int indent = 0) const;
    virtual llvm::Value *generateCode(CodeKit &kit);
};

class Assignment : public Expression {
   public:
    Identifier *var;
    Expression *expr;
    Assignment(Identifier *v, Expression *e) : var(v), expr(e) {}
    Assignment(Expression *e) : expr(e) {}
    virtual void print(ostream &output, int indent = 0) const;
    virtual llvm::Value *generateCode(CodeKit &kit);
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
    virtual void traverse(SymbolTable<Referent> &st);
    virtual llvm::Value *generateCode(CodeKit &kit);
};

class While : public Statement {
   public:
    Expression *cond;
    Statement *stmt;
    While(Expression *c, Statement *s) : cond(c), stmt(s) {}
    virtual void print(ostream &output, int indent = 0) const;
    virtual void traverse(SymbolTable<Referent> &st);
    virtual llvm::Value *generateCode(CodeKit &kit);
};

class Return : public Statement {
   public:
    Expression *expr;
    Return(Expression *e) : expr(e) {}
    virtual void print(ostream &output, int indent = 0) const;
    virtual llvm::Value *generateCode(CodeKit &kit);
};

class FunctionDeclaration : public External {
   public:
    TypeSpecifier ret;
    string name;
    vector<Declaration *> *arguments;
    FunctionDeclaration(TypeSpecifier t, string n, vector<Declaration *> *a)
        : ret(t), name(n), arguments(a) {}
    virtual void print(ostream &output, int indent = 0) const;
    virtual void traverse(SymbolTable<Referent> &st);
    virtual llvm::Function *generateCode(CodeKit &kit);
};

class FunctionDefinition : public External {
   public:
    FunctionDeclaration *decl;
    CompoundStatement *content;
    bool hasReturn;
    FunctionDefinition(FunctionDeclaration *d, CompoundStatement *c)
        : decl(d), content(c) {}
    virtual void print(ostream &output, int indent = 0) const;
    virtual void traverse(SymbolTable<Referent> &st);
    virtual llvm::Function *generateCode(CodeKit &kit);
};

class FunctionCall : public Expression {
   public:
    string function;
    vector<Expression *> *arguments;
    FunctionCall(string f, vector<Expression *> *a)
        : function(f), arguments(a) {}
    virtual void print(ostream &output, int indent = 0) const;
    virtual llvm::Value *generateCode(CodeKit &kit);
};

struct Referent {
    ReferentType rtype;
    TypeSpecifier type;
    int pointers;
    Node *node;
    llvm::AllocaInst *alloc;
    Referent(ReferentType rt, TypeSpecifier t, int p, Node *n)
        : rtype(rt), type(t), pointers(p), node(n) {}
    Referent(ReferentType rt, TypeSpecifier t, int p, Node *n,
             llvm::AllocaInst *a)
        : rtype(rt), type(t), pointers(p), node(n), alloc(a) {}
};

// source https://stackoverflow.com/a/13428630/5837426
template <typename T>
class StackOfScopes : public stack<map<string, T *>> {
   public:
    using stack<map<string, T *>>::c;  // expose the container
};

template <typename T>
class SymbolTable {
   public:
    StackOfScopes<T> table;
    void enterScope() {
        auto *m = new map<string, T *>();
        table.push(*m);
    }
    T *findSymbol(string sym) {
        for (int i = 0; i < table.size(); ++i) {
            if (table.c[i].count(sym) != 0) {
                return table.c[i][sym];
            }
        }
        return nullptr;
    }
    void addSymbol(string sym, T *ref) { table.top().insert({sym, ref}); }
    bool checkScope(string sym) { return table.top().count(sym) != 0; }
    void exitScope() { table.pop(); }
    friend ostream &operator<<(ostream &output, const SymbolTable &st) {
        output << string(20, '-') << endl;
        for (int i = 0; i < st.table.size(); ++i) {
            for (const auto &it : st.table.c[i]) {
                output << it.first << " : " << it.second->type
                       << string(it.second->pointers, '*') << " "
                       << it.second->rtype << endl;
            }
            output << string(5, '.') << endl;
        }
        output << string(20, '-') << endl;
        return output;
    }
};

struct CodeKit {
    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    llvm::Module module;
    SymbolTable<llvm::AllocaInst> symbolTable;
    map<string, llvm::AllocaInst *> namedValues;
    CodeKit(string name) : module(name, context), builder(context) {}
};

llvm::Value *logError(string error);
void emitCode(CodeKit &kit);
llvm::AllocaInst *createEntryBlockAlloca(llvm::Function *func, const string var,
                                         llvm::Type *type);
llvm::Type *generateType(TypeSpecifier type, llvm::LLVMContext &context);
