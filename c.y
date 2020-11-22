%{
#include <cstdio>
#include <iostream>
#include <cstring>
#include <stdio.h>
#include "c.ast.hpp"
#include <typeinfo>
#define YYDEBUG 1

using namespace std;

// stuff from flex that bison needs to know about:
extern "C" int yylex();
int yyparse(BlockOfFunctions *ast);
extern "C" FILE *yyin;
 
void yyerror(BlockOfFunctions *ast, const char *s);

#define TRACE printf("reduce at line %d\n", __LINE__);


%}
%token  IDENTIFIER I_CONSTANT F_CONSTANT STRING_LITERAL FUNC_NAME SIZEOF
%token  PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token  AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token  SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token  XOR_ASSIGN OR_ASSIGN
%token  TYPEDEF_NAME ENUMERATION_CONSTANT

%token  TYPEDEF EXTERN STATIC AUTO REGISTER INLINE
%token  CONST RESTRICT VOLATILE
%token  BOOL CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE VOID
%token  COMPLEX IMAGINARY 
%token  STRUCT UNION ENUM ELLIPSIS

%token  CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%token  ALIGNAS ALIGNOF ATOMIC GENERIC NORETURN STATIC_ASSERT THREAD_LOCAL

%start translation_unit
%parse-param {BlockOfFunctions *ast}

%union {
    string *str;
    TypeSpecifier typespec;
    FunctionDefinition *func;
    BlockOfFunctions *blockfunc;
    Declaration *decl;
    vector<Declaration> *decls;
    Signature *sig;
}

%type<typespec> type_specifier declaration_specifiers
%type<str> IDENTIFIER
%type<func> external_declaration function_definition
%type<blockfunc> translation_unit
%type<decl> parameter_declaration
%type<decls> parameter_list parameter_type_list
%type<sig> declarator direct_declarator
%%

declaration_specifiers
    : type_specifier { TRACE $$ = $1; }
    ;

type_specifier
    : VOID {
        cout << "creating void" << endl;
        $$ = TypeSpecifier::Void; }
    | INT { cout << "creating int" << endl; $$ = TypeSpecifier::Int; }
    ;

declarator
    : direct_declarator { $$ = $1; }
    ;

direct_declarator
    : IDENTIFIER {
        Signature sig;
        string name = *$1;
        sig.name = name;
        $$ = &sig;
        cout << "creating identifier " << sig.name << endl;
    }
    | direct_declarator '(' parameter_type_list ')' {
        cout << "with argument" << endl;
        
        cout << "got declarator " << *$1 << endl;
        cout << "creating declaration " << $3->at(0) << endl;
        $$ = $1;
    }
    | direct_declarator '(' ')' {
        $$ = $1;
        cout << "argument less function" << endl; 
    }
    ;

parameter_type_list
    : parameter_list {
        $$ = $1;
        cout << "creating parameter type list " << $$->at(0) << endl; 
    }
    ;

parameter_list
    : parameter_declaration {
        vector<Declaration> params;
        cout << "pushing back " << *$1 << endl;
        params.push_back(*$1);
        $$ = &params;
        cout << "creating parameter declaration " << $$->at(0) << endl;
    }
    ;

parameter_declaration
    : declaration_specifiers declarator {
        cout << "creating param declaration" << endl;
        Declaration decl;
        string name = $2->name;
        decl.type = $1;
        decl.name = name;
        $$ = &decl;
    }
    ;

translation_unit
    : external_declaration { ast->block.push_back(*$1); }
    | translation_unit external_declaration { ast->block.push_back(*$2); }
    ;

external_declaration
    : function_definition  { TRACE $$ = $1; }
    ;

function_definition
    : declaration_specifiers declarator '{' '}' {
        string name = $2->name;
        FunctionDefinition fn;
        fn.ret = $1;
        fn.name = name;
        $$ = &fn;
    }
    ;

%%
#include <stdio.h>

void yyerror(BlockOfFunctions *ast, const char *s)
{
    fflush(stdout);
    fprintf(stderr, "*** %s\n", s);
}