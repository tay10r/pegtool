#include <gtest/gtest.h>

#include <pegtool.h>

#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>

namespace {

bool
fileExists(const char* path);

std::string
readFile(const char* path);

std::string
diagnosticsToString(const peg::Grammar&);

std::vector<std::string>
splitLines(const std::string& in);

void
printStringComparison(const std::string& a, const std::string& b);

bool
verifyDiagnosticOutput(const peg::Grammar& grammar, const char* expectedErrPath)
{
  auto success = true;

  auto actualErr = diagnosticsToString(grammar);

  auto expectedErr = readFile(expectedErrPath);

  if (expectedErr != actualErr) {
    std::cerr << "Diagnostics did not meet expectations.";
    std::cerr << std::endl;
    std::cerr << "Expected vs. Actual:" << std::endl;
    printStringComparison(expectedErr, actualErr);
    success = false;
  }

  return success;
}

bool
verifyParserOutput(const peg::Grammar& grammar, const char* inputPath)
{
  auto input = readFile(inputPath);

  auto root = grammar.parse(input.c_str(), input.size());

  std::ostringstream printStream;

  if (root)
    root->print(printStream);

  std::string expectedOut = readFile("expected_output.txt");

  auto actualOut = printStream.str();

  if (expectedOut != actualOut) {
    std::cerr << "Parser did not print expected parse tree" << std::endl;
    std::cerr << std::endl;
    std::cerr << "Expected vs. Actual:" << std::endl;
    printStringComparison(expectedOut, actualOut);
    return false;
  }

  return true;
}

} // namespace

int
main()
{
  const char* grammarPath = "grammar.peg";

  std::ifstream grammarFile(grammarPath);

  if (!grammarFile.good()) {
    std::cerr << "Failed to open '" << grammarPath << "'" << std::endl;
    return EXIT_FAILURE;
  }

  std::ostringstream grammarFileStream;

  grammarFileStream << grammarFile.rdbuf();

  auto grammarStr = grammarFileStream.str();

  peg::Grammar grammar;

  grammar.load(grammarStr.c_str(), grammarStr.size(), grammarPath);

  if (fileExists("should_fail.txt")) {
    if (!grammar.hasErrors()) {
      std::cerr << "Test passed when it was expected to fail." << std::endl;
      return EXIT_FAILURE;
    }
  } else {
    if (grammar.hasErrors()) {
      std::cerr << "Test failed when it was expected to pass." << std::endl;
      return EXIT_FAILURE;
    }
  }

  if (fileExists("expected_err.txt")) {
    if (!verifyDiagnosticOutput(grammar, "expected_err.txt"))
      return EXIT_FAILURE;
  }

  if (fileExists("input.txt")) {
    if (!verifyParserOutput(grammar, "input.txt"))
      return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

namespace {

bool
fileExists(const char* path)
{
  std::ifstream file(path);
  return file.good();
}

std::string
readFile(const char* path)
{
  std::ifstream file(path);

  if (!file.good())
    return "<failed-to-open>";

  std::ostringstream fileStream;

  fileStream << file.rdbuf();

  return fileStream.str();
}

std::vector<std::string>
splitLines(const std::string& in)
{
  std::istringstream stream(in);

  std::vector<std::string> lines;

  std::string line;

  while (std::getline(stream, line))
    lines.emplace_back(line);

  return lines;
}

std::string
diagnosticsToString(const peg::Grammar& grammar)
{
  std::ostringstream errStream;

  grammar.printDiagnostics(errStream);

  return errStream.str();
}

std::size_t
getMaxColumnCount(const std::vector<std::string>& lineVec)
{
  size_t maxColCount = 0;

  for (const auto& s : lineVec)
    maxColCount = std::max(maxColCount, s.size());

  return maxColCount;
}

void
printStringComparison(const std::string& a, const std::string& b)
{
  auto aLines = splitLines(a);
  auto bLines = splitLines(b);

  auto maxLineCount = std::max(aLines.size(), bLines.size());

  auto maxColCount = getMaxColumnCount(aLines);

  for (size_t i = 0; i < maxLineCount; i++) {

    std::cout << "  ";

    if (i < aLines.size()) {
      std::cout << aLines[i];

      size_t remainingCols = maxColCount - aLines[i].size();

      for (size_t i = 0; i < remainingCols; i++)
        std::cout << ' ';

    } else {

      for (size_t i = 0; i < maxColCount; i++)
        std::cout << ' ';
    }

    if (i < bLines.size())
      std::cout << " | " << bLines[i];
    else
      std::cout << " |";

    std::cout << std::endl;
  }
}

} // namespace
