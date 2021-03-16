#include "Peg.h"

#include <fstream>
#include <iostream>
#include <sstream>

#include <cstdlib>

namespace {

bool
handleErrors(const char *grammarPath, const peg::Grammar &grammar)
{
  for (const auto &diag : grammar.getDiagnostics()) {

    std::cerr << grammarPath << ':';

    diag.print(std::cerr);
  };

  return true;
}

} // namespace

int
main()
{
  const char *grammarPath = "grammar.peg";

  std::ifstream grammarFile(grammarPath);

  if (!grammarFile.good()) {
    std::cerr << "Failed to open '" << grammarPath << "'" << std::endl;
    return EXIT_FAILURE;
  }

  std::ostringstream grammarFileStream;

  grammarFileStream << grammarFile.rdbuf();

  auto grammarStr = grammarFileStream.str();

  auto grammar = peg::parseGrammar(grammarStr.c_str(), grammarStr.size());

  if (grammar.hasErrors())
    return handleErrors(grammarPath, grammar) ? EXIT_SUCCESS : EXIT_FAILURE;

  return EXIT_SUCCESS;
}
