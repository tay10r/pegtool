cmake_minimum_required(VERSION 3.9.6)

project(pegtool)

option(PEGTOOL_TESTS "Whether or not to build the unit tests.")
option(PEGTOOL_VALGRIND "Whether or not to use valgrind for testing (if found.)" ON)

if(NOT MSVC)
  set(cxxflags -Wall -Wextra -Werror -Wfatal-errors -std=c++11)
endif(NOT MSVC)

add_library(peg pegtool.h pegtool.cpp)

target_compile_options(peg PRIVATE ${cxxflags})

target_include_directories(peg PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")

if(PEGTOOL_TESTS)
  add_subdirectory(tests)
  enable_testing()
endif(PEGTOOL_TESTS)
