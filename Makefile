all: pascal

pascal.cpp: pascal.y
	bison -v -d -o $@ $^

pascal.hpp: pascal.cpp

tokens.cpp: pascal.l pascal.hpp
	flex -o $@ $^

pascal: pascal.cpp tokens.cpp
	g++ -std=c++11 -o $@ pascal.cpp tokens.cpp

clean:
	rm pascal.cpp pascal.hpp tokens.cpp pascal pascal.output