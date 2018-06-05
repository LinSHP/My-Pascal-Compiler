cmake_minimum_required(VERSION 3.9)
project(my_pascal)

set(CMAKE_CXX_STANDARD 11)

include_directories(
        "/usr/local/opt/llvm/include"
)

add_executable(my_pascal main.cpp AST.h codegen.h codegen.cpp AST.cpp)