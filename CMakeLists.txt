cmake_minimum_required(VERSION 3.9.6)

project(ParsingExprGrammar)

find_package(GTest REQUIRED)

add_library(Peg Peg.h Peg.cpp)

add_executable(PegTest PegTest.cpp)

target_link_libraries(PegTest PRIVATE Peg)

add_test(NAME PegTest COMMAND $<TARGET_FILE:PegTest>)

enable_testing()
