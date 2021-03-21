Test Cases
==========

This directory contains a series of sub-directories each containing a test case.
A test case is a set of files used by the test driver to verify a certain
behavior of the library. The files present in each test case affect what data
the test driver will inspect for correctness. Here is a list of all relevant
files for test cases:

| Name                     | Usage                                                                                  |
|--------------------------|----------------------------------------------------------------------------------------|
| grammar.peg              | Contains the specification for the gramamr being tested.                               |
| input.txt                | Contains the input data to be sent to the parser.                                      |
| expected_diagnostics.txt | Contains printed diagnostics expected to be generated after parsing the `grammar.peg`. |
| expected_output.txt      | Contains the output expected to be printed of the parse tree.                          |
| should_fail.txt          | If present, indicates that `grammar.peg` should fail to load.                          |

The only file that is required to be present in each directory is `grammar.peg`.
