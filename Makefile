c.tab.cpp c.tab.hpp: c.y
	bison -o c.tab.cpp -d c.y

c.lex.cpp: c.l c.tab.hpp
	flex -o c.lex.cpp -l c.l

cc: cc.cpp c.tab.cpp c.lex.cpp
	g++ -g c.tab.cpp c.lex.cpp c.ast.cpp cc.cpp `llvm-config --cxxflags --ldflags --libs` -lm -ll -lfl -Wall -o $@

cleancc:
	rm -f c.tab.cpp c.tab.hpp c.lex.cpp cc

output.ll output.bc output.o runcc: cc
	./cc examples/test.c

clean-runcc: cleancc runcc

output: output.ll output.bc output.o
	g++ output.o -o output

cleanoutput:
	rm -f output.ll output.bc output.o output

runoutput: output
	./output

clean-runoutput: cleanoutput runoutput

opt.so: c.opt.cpp
	g++ -g c.opt.cpp `llvm-config --cxxflags --ldflags --libs` -Wall -shared -fPIC -o $@

output.opt output.opt.ll output.opt.o opt: opt.so output.ll
	opt -S -load ./opt.so -deadcondbr -deadbr -foldconst < output.ll | tee output.opt.ll
	llc -filetype=obj --relocation-model=pic output.opt.ll -o output.opt.o
	gcc output.opt.o -o output.opt

cleanopt:
	rm -f opt.so output.opt.ll output.opt.o output.opt 

runopt: output.opt
	./output.opt

clean-runopt: cleanopt runopt

cleanall: cleancc cleanoutput cleanopt
runall: runcc runoutput runopt
cleanall-runall: cleanall runall
