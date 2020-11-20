#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <iostream>
#include "c.ast.hpp"
#include "c.tab.hpp"

extern "C" int yylex();
int yyparse(BlockOfFunctions *ast);
extern "C" FILE *yyin;

static void usage() {
  printf("Usage: cc <prog.c>\n");
}

int main(int argc, char **argv) {
  int yydebug = 1;
  if (argc != 2) {
    usage();
    exit(1);
  }
  char const *filename = argv[1];
  yyin = fopen(filename, "r");
  assert(yyin);
  BlockOfFunctions ast;
  int ret = yyparse(&ast);
  cout << "retv = " << ret << endl;
  cout << ast << endl;
  exit(0);
}
