cc: cc.cpp c.tab.cpp c.lex.cpp
	g++ c.tab.cpp c.lex.cpp c.ast.cpp cc.cpp -lm -ll -lfl -Wall -o $@

c.tab.cpp c.tab.hpp: c.y
	bison -t -v -o c.tab.cpp -d c.y

c.lex.cpp: c.l c.tab.hpp
	flex -o c.lex.cpp -l c.l

clean::
	rm -f c.tab.cpp c.tab.hpp c.lex.cpp cc c.output

run: clean cc
	./cc examples/test.c
