#include <map>
#include <stack>

#include "c.ast.hpp"
using namespace std;

// source https://stackoverflow.com/a/13428630/5837426
class StackOfScopes : public stack<map<string, Node*>> {
   public:
    using stack<map<string, Node*>>::c;  // expose the container
};

class SymbolTable {
   public:
    StackOfScopes table;
    SymbolTable() {}
    void enterScope() {
        auto* m = new map<string, Node*>();
        table.push(*m);
    }
    Node* findSymbol(string sym) {
        for (int i = 0; i < table.size(); ++i) {
            try {
                return table.c[i][sym];
            } catch (out_of_range e) {
            }
        }
        return NULL;
    }
    void addSymbol(string sym, Node* node) { table.top().insert({sym, node}); }
    bool checkScope(string sym) { return table.top().count(sym) != 0; }
    void exitScope() { table.pop(); }
};

void traverse(Node& node, SymbolTable& symtbl) {
    symtbl.enterScope();
    Node* n = symtbl.findSymbol("yes");
    if (n == NULL) {
        cout << "NULL" << endl;
    }
};

void validateScope(AST& ast) {
    SymbolTable symtbl;
    traverse(ast, symtbl);
};
