#pragma once
#include <map>
#include <stack>

using namespace std;

class Node;
ostream& operator<<(ostream& output, const Node& node);

enum ReferentType { Func, Var };
ostream& operator<<(ostream& output, const ReferentType& type);

enum class TypeSpecifier;
ostream& operator<<(ostream& output, const TypeSpecifier& type);

struct Referent {
    ReferentType rtype;
    TypeSpecifier type;
    int pointers;
    Node* node;
    Referent(ReferentType rt, TypeSpecifier t, int p, Node* n)
        : rtype(rt), type(t), pointers(p), node(n) {}
};

// source https://stackoverflow.com/a/13428630/5837426
class StackOfScopes : public stack<map<string, Referent*>> {
   public:
    using stack<map<string, Referent*>>::c;  // expose the container
};

class SymbolTable {
   public:
    StackOfScopes table;
    void enterScope() {
        auto* m = new map<string, Referent*>();
        table.push(*m);
    }
    Referent* findSymbol(string sym) {
        for (int i = 0; i < table.size(); ++i) {
            if (table.c[i].count(sym) != 0) {
                return table.c[i][sym];
            }
        }
        return NULL;
    }
    void addSymbol(string sym, Referent* ref) {
        table.top().insert({sym, ref});
    }
    bool checkScope(string sym) { return table.top().count(sym) != 0; }
    void exitScope() { table.pop(); }
    friend ostream& operator<<(ostream& output, const SymbolTable& st) {
        output << string(20, '-') << endl;
        for (int i = 0; i < st.table.size(); ++i) {
            for (const auto& it : st.table.c[i]) {
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
