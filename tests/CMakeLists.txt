cmake_minimum_required(VERSION 3.9.6)

find_program(VALGRIND_EXECUTABLE valgrind)

add_executable(pegtool_test_driver
  test_driver/main.cpp
  test_driver/run_test.h
  test_driver/run_test.cpp)

target_compile_options(pegtool_test_driver PRIVATE ${cxxflags})

target_link_libraries(pegtool_test_driver PRIVATE peg)

set_target_properties(pegtool_test_driver
  PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}"
    OUTPUT_NAME run_test)

file(GLOB dir_entries "${CMAKE_CURRENT_SOURCE_DIR}/*")

set(tests)

foreach (dir_entry ${dir_entries})

  if (IS_DIRECTORY ${dir_entry} AND EXISTS "${dir_entry}/grammar.peg")
    list(APPEND tests ${dir_entry})
  endif (IS_DIRECTORY ${dir_entry} AND EXISTS "${dir_entry}/grammar.peg")

endforeach (dir_entry ${dit_entries})

set(test_list_content)

foreach (test ${tests})

  string(APPEND test_list_content ${test} "\n")

endforeach (test ${tests})

set(test_list_path "${PROJECT_BINARY_DIR}/pegtool_tests.txt")

file(WRITE ${test_list_path} ${test_list_content})

set(VALGRIND_CMD)

if (VALGRIND_EXECUTABLE AND PEGTOOL_VALGRIND)
  set(VALGRIND_CMD ${VALGRIND_EXECUTABLE} --error-exitcode=1)
endif (VALGRIND_EXECUTABLE AND PEGTOOL_VALGRIND)

add_test(NAME pegtool_tests
  COMMAND ${VALGRIND_CMD} $<TARGET_FILE:pegtool_test_driver> --test-list ${test_list_path})

enable_testing()
