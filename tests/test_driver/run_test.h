#pragma once

#include <iosfwd>

struct TestResults final
{
  bool failed = false;
};

TestResults
runTest(const char* testPath, std::ostream& outLog, std::ostream& errLog);
