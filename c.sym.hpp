#pragma once
#include <map>
#include <stack>

using namespace std;

class Node;
ostream& operator<<(ostream& output, const Node& node);

enum ReferentType { Func, Int, Char };

struct Referent {
    ReferentType type;
    Node* node;
    Referent(ReferentType t, Node* n) : type(t), node(n) {}
    Referent(ReferentType t) : type(t) {}
    friend ostream& operator<<(ostream& output, const Referent& ref) {
        const string reftypes[]{"function", "int", "char"};
        cout << reftypes[static_cast<int>(ref.type)] << " : " << *ref.node;
        return output;
    }
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
        for (int i = 0; i < st.table.size(); ++i) {
            output << string(10, '-') << endl;
            for (const auto& entry : st.table.c[i]) {
                output << "Symbol: " << entry.first
                       << ", Type: " << entry.second->type << endl;
            }
        }
        return output;
    }
};
