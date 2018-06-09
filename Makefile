LLVM_CONFIG = /usr/local/opt/llvm/bin/llvm-config

NO_WARNING =  -Wno-return-type \
	-Wno-c++11-compat-deprecated-writable-strings \
	-Wno-deprecated-register \
	-Wno-switch \

CXXFLAGS = `$(LLVM_CONFIG) --cppflags` -std=c++11 $(NO_WARNING)
LDFLAGS = `$(LLVM_CONFIG) --ldflags` -lpthread -ldl -lffi -lz -lncurses -rdynamic
LIBS = `$(LLVM_CONFIG) --libs --system-libs`

all: pascal

pascal.cpp: pascal.y
	bison -d -o $@ $^

pascal.hpp: pascal.cpp

tokens.cpp: pascal.l pascal.hpp
	flex -o $@ $^

%.o: %.cpp
	g++ -c $(CXXFLAGS) -g -o $@ $<

pascal: pascal.o tokens.o AST.o codegen.o main.o
	g++ -std=c++11 -o $@ $(LIBS) $(LDFLAGS) *.o

clean:
	rm pascal.cpp pascal.hpp tokens.cpp pascal

