cc: cc.cpp c.tab.cpp c.lex.cpp
	g++ -g c.tab.cpp c.lex.cpp c.ast.cpp cc.cpp `llvm-config --cxxflags --ldflags --libs` -lm -ll -lfl -Wall -o $@

c.tab.cpp c.tab.hpp: c.y
	bison -o c.tab.cpp -d c.y

c.lex.cpp: c.l c.tab.hpp
	flex -o c.lex.cpp -l c.l

clean:
	rm -f c.tab.cpp c.tab.hpp c.lex.cpp cc output.bc output.o main

run: cc
	./cc examples/test.c

clean-run: clean cc run

main: cc
	g++ output.o main.cpp -o main
	./main
