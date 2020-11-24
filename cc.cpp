#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <iostream>
#include "c.ast.hpp"
#include "c.tab.hpp"

extern "C" int yylex();
void yyerror(AST *ast, const char *s);
extern "C" FILE *yyin;

static void usage() {
  printf("Usage: cc <prog.c>\n");
}

int main(int argc, char **argv) {
  if (argc != 2) {
    usage();
    exit(1);
  }
  char const *filename = argv[1];
  yyin = fopen(filename, "r");
  assert(yyin);
  AST ast;
  int ret = yyparse(&ast);
  cout << "retv = " << ret << endl;
  for (const auto &it: ast) {
    cout << *it << endl;
  }
  exit(0);
}
