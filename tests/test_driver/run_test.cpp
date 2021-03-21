#include "run_test.h"

#include <pegtool.h>

#include <algorithm>
#include <cstdlib>
#include <fstream>
#include <sstream>
#include <vector>

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
printStringComparison(const std::string& a,
                      const std::string& b,
                      std::ostream& errLog);

bool
verifyDiagnosticOutput(const peg::Grammar& grammar,
                       const char* expectedErrPath,
                       std::ostream& errLog)
{
  auto success = true;

  auto actualErr = diagnosticsToString(grammar);

  auto expectedErr = readFile(expectedErrPath);

  if (expectedErr != actualErr) {
    errLog << "Diagnostics did not meet expectations.";
    errLog << std::endl;
    errLog << "Expected vs. Actual:" << std::endl;
    printStringComparison(expectedErr, actualErr, errLog);
    success = false;
  }

  return success;
}

bool
verifyParserOutput(const peg::Grammar& grammar,
                   const char* inputPath,
                   const char* expectedOutputPath,
                   std::ostream& errLog)
{
  auto input = readFile(inputPath);

  auto root = grammar.parse(input.c_str(), input.size());

  std::ostringstream printStream;

  if (root)
    root->print(printStream);

  std::string expectedOut = readFile(expectedOutputPath);

  auto actualOut = printStream.str();

  if (expectedOut != actualOut) {
    errLog << "Parser did not print expected parse tree" << std::endl;
    errLog << std::endl;
    errLog << "Expected vs. Actual:" << std::endl;
    printStringComparison(expectedOut, actualOut, errLog);
    return false;
  }

  return true;
}

std::string
makePath(const char* testBasePath, const char* filename)
{
  std::string path(testBasePath);

  if ((path.size() > 0) && ((path.back() != '/') && (path.back() != '\\')))
    path.push_back('/');

  path += filename;

  return path;
}

TestResults
fail()
{
  TestResults results;
  results.failed = true;
  return results;
}

TestResults
pass()
{
  TestResults results;
  results.failed = false;
  return results;
}

} // namespace

TestResults
runTest(const char* testPath, std::ostream&, std::ostream& errLog)
{
  auto grammarPath = makePath(testPath, "grammar.peg");

  std::ifstream grammarFile(grammarPath.c_str());

  if (!grammarFile.good()) {
    errLog << "Failed to open '" << grammarPath << "'" << std::endl;
    return fail();
  }

  std::ostringstream grammarFileStream;

  grammarFileStream << grammarFile.rdbuf();

  auto grammarStr = grammarFileStream.str();

  peg::Grammar grammar;

  grammar.load(grammarStr.c_str(), grammarStr.size(), "grammar.peg");

  auto shouldFailPath = makePath(testPath, "should_fail.txt");

  if (fileExists(shouldFailPath.c_str())) {
    if (!grammar.hasErrors()) {
      errLog << "Test passed when it was expected to fail." << std::endl;
      errLog << std::endl;
      errLog << "Contains the following diagnostics:" << std::endl;
      errLog << std::endl;
      grammar.printDiagnostics(errLog);
      return fail();
    }
  } else {
    if (grammar.hasErrors()) {
      errLog << "Test failed when it was expected to pass." << std::endl;
      errLog << std::endl;
      grammar.printDiagnostics(errLog);
      return fail();
    }
  }

  auto diagnosticsPath = makePath(testPath, "expected_diagnostics.txt");

  if (fileExists(diagnosticsPath.c_str())) {
    if (!verifyDiagnosticOutput(grammar, diagnosticsPath.c_str(), errLog))
      return fail();
  } else {
    if (grammar.hasDiagnostics()) {
      errLog << "Found unexpected diagnostics:" << std::endl;
      grammar.printDiagnostics(errLog);
      return fail();
    }
  }

  auto inPath = makePath(testPath, "input.txt");

  auto outPath = makePath(testPath, "expected_output.txt");

  if (fileExists(inPath.c_str())) {
    if (!verifyParserOutput(grammar, inPath.c_str(), outPath.c_str(), errLog))
      return fail();
  }

  return pass();
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

size_t
getColumnCount(const std::string& str)
{
  size_t count = 0;

  for (const auto& c : str) {
    if ((c & 0xc0) == 0x80)
      continue;
    count++;
  }

  return count;
}

std::size_t
getMaxColumnCount(const std::vector<std::string>& lineVec)
{
  size_t maxColCount = 0;

  for (const auto& s : lineVec)
    maxColCount = std::max(maxColCount, getColumnCount(s));

  return maxColCount;
}

void
printStringComparison(const std::string& a,
                      const std::string& b,
                      std::ostream& errLog)
{
  auto aLines = splitLines(a);
  auto bLines = splitLines(b);

  auto maxLineCount = std::max(aLines.size(), bLines.size());

  auto maxColCount = getMaxColumnCount(aLines);

  for (size_t i = 0; i < maxLineCount; i++) {

    errLog << "  ";

    if (i < aLines.size()) {
      errLog << aLines[i];

      size_t remainingCols = maxColCount - getColumnCount(aLines[i]);

      for (size_t i = 0; i < remainingCols; i++)
        errLog << ' ';

    } else {

      for (size_t i = 0; i < maxColCount; i++)
        errLog << ' ';
    }

    if (i < bLines.size())
      errLog << " | " << bLines[i];
    else
      errLog << " |";

    errLog << std::endl;
  }
}

} // namespace
