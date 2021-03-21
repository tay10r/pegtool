#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

#include <cstdlib>
#include <cstring>

#include "run_test.h"

namespace {

bool
registerTestList(std::vector<std::string>& testPaths, const char* testListPath)
{
  std::ifstream file(testListPath);

  if (!file.good()) {
    std::cerr << "Failed to open '" << testListPath << "'" << std::endl;
    return false;
  }

  while (!file.eof()) {

    std::string path;

    std::getline(file, path);

    if (path.empty())
      break;

    testPaths.emplace_back(std::move(path));
  }

  return true;
}

bool
fileExists(const char* path)
{
  std::ifstream file(path);

  return file.good();
}

void
writeTestOutput(std::ostream& dstStream, const std::string& output, bool err)
{
  if (output.empty())
    return;

  const char* prefix = err ? "err -> | " : "out -> | ";

  dstStream << prefix;

  prefix = "       | ";

  for (size_t i = 0; i < output.size(); i++) {

    auto c = output[i];

    dstStream << c;

    if ((c == '\n') && ((i + 1) < output.size()))
      dstStream << prefix;
  }
}

} // namespace

int
main(int argc, char** argv)
{
  std::vector<std::string> testPaths;

  for (int i = 1; i < argc; i++) {

    if (std::strcmp(argv[i], "--test-list") == 0) {

      if ((i + 1) >= argc) {
        std::cerr << "Test list path not specified." << std::endl;
        return EXIT_FAILURE;
      }

      if (!registerTestList(testPaths, argv[++i]))
        return EXIT_FAILURE;
    }

    if (argv[i][0] == '-') {
      std::cerr << "Unknown option '" << argv[i] << "'" << std::endl;
      return EXIT_FAILURE;
    }

    testPaths.emplace_back(argv[i]);
  }

  if (testPaths.empty() && fileExists("pegtool_tests.txt")) {
    if (!registerTestList(testPaths, "pegtool_tests.txt"))
      return EXIT_FAILURE;
  }

  if (testPaths.empty())
    testPaths.emplace_back(".");

  size_t failureCount = 0;

  for (const auto& testPath : testPaths) {

    std::ostringstream outLog;

    std::ostringstream errLog;

    TestResults results = runTest(testPath.c_str(), outLog, errLog);

    if (results.failed) {

      std::cerr << "FAILURE at " << testPath << std::endl;

      writeTestOutput(std::cout, outLog.str(), false /* is err */);
      writeTestOutput(std::cerr, errLog.str(), true);

      failureCount++;
    }
  }

  std::cout << (testPaths.size() - failureCount) << " out of "
            << testPaths.size() << " tests passed." << std::endl;

  return failureCount ? EXIT_FAILURE : EXIT_SUCCESS;
}
