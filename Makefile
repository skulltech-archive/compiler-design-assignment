cc: cc.cpp c.tab.cpp c.lex.cpp
	g++ -g c.tab.cpp c.lex.cpp c.ast.cpp cc.cpp `llvm-config --cxxflags --ldflags --libs` -lm -ll -lfl -Wall -o $@

c.tab.cpp c.tab.hpp: c.y
	bison -o c.tab.cpp -d c.y

c.lex.cpp: c.l c.tab.hpp
	flex -o c.lex.cpp -l c.l

cleancc:
	rm -f c.tab.cpp c.tab.hpp c.lex.cpp cc

output.ir output.bc output.o runcc: cc
	./cc examples/test.c

clean-runcc: cleancc cc runcc

output: output.ir output.bc output.o
	g++ output.o -o output

cleanoutput:
	rm -f output.ir output.bc output.o output

runoutput: output
	./output

clean-runoutput: cleanoutput output runoutput

opt.so: c.opt.cpp
	g++ -g c.opt.cpp `llvm-config --cxxflags --ldflags --libs` -Wall -shared -fPIC -o $@

cleanopt:
	rm -f opt.so output.opt.ir output.opt.o output.opt 

runopt: opt.so
	opt -S -load ./opt.so -deadcondbr -deadbr < output.ir | tee output.opt.ir
	llc -filetype=obj --relocation-model=pic opt.ir -o output.opt.o
	gcc output.opt.o -o output.opt
	./output.opt

clean-runopt: cleanopt runopt

cleanall: cleancc cleanoutput cleanopt
runall: runcc runoutput runopt
cleanall-runall: cleanall runall
