cmake_minimum_required(VERSION 3.9.6)

project(pegtool)

option(PEGTOOL_TESTS "Whether or not to build the unit tests.")

if(NOT MSVC)
  set(cxxflags -Wall -Wextra -Werror -Wfatal-errors)
endif(NOT MSVC)

add_library(peg pegtool.h pegtool.cpp)

target_compile_options(peg PRIVATE ${cxxflags})

target_include_directories(peg PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")

target_compile_features(peg PRIVATE cxx_std_11)

if(PEGTOOL_TESTS)

  add_executable(peg_test pegtool_test.cpp)

  target_compile_options(peg_test PRIVATE ${cxxflags})

  target_link_libraries(peg_test PRIVATE peg)

  set(tests
    invalid_def_name
    missing_right_arrow
    missing_expr_after_right_arrow
    literal_expr
    literal_expr_2
    literal_expr_3
    literal_expr_lf
    literal_expr_cr
    literal_expr_tab
    literal_expr_inner_squote
    literal_expr_inner_dquote
    literal_expr_missing_right_squote
    literal_expr_missing_right_dquote
    literal_expr_octal
    literal_expr_octal_2
    literal_expr_octal_3
    literal_expr_octal_4
    literal_expr_backslash
    literal_expr_backslash_fail
    literal_expr_hex
    literal_expr_hex_fail
    literal_expr_hex_uppercase
    literal_expr_hex_uppercase_fail
    literal_expr_hex_single_digit
    literal_expr_hex_single_digit_fail
    literal_expr_unicode
    literal_expr_unicode_fail
    literal_expr_unicode_2
    literal_expr_unicode_2_fail
    literal_expr_unicode_3
    literal_expr_unicode_3_fail
    literal_expr_unicode_4
    literal_expr_unicode_4_fail
    literal_expr_unicode_diag
    literal_expr_unicode_diag_2
    literal_expr_unicode_diag_3
    sequence_expr
    sequence_expr_2
    reference_expr
    reference_expr_unresolved
    duplicate_definition
    unused_definition
    unused_definition_2
    dot_expr
    dot_expr_2
    dot_expr_3
    dot_expr_4
    dot_expr_5)

  foreach (test ${tests})

    add_test(NAME PegTest:${test}
      COMMAND $<TARGET_FILE:peg_test>
      WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/tests/${test}")

  endforeach (test ${tests})

  enable_testing()

endif(PEGTOOL_TESTS)
