#include "Peg.h"

#include <algorithm>
#include <sstream>

namespace peg {

// {{{ Char Cursor
//================

namespace {

class CharCursor final
{
public:
  CharCursor(const char *s, size_t len)
    : source(s)
    , charMax(len)
  {}

  bool atEnd() const noexcept;

  const char *getOffsetPtr() const noexcept {
    return this->source + this->charIdx;
  }

  size_t getLine() const noexcept { return this->ln; }

  size_t getColumn() const noexcept { return this->col; }

  bool outOfBounds(size_t relOffset) const noexcept;

  char peek(size_t offset) const noexcept;

  void next(size_t count) noexcept;

  void skipUnused();

private:
  bool skipWS();

  bool skipComment();

  size_t toAbsIndex(size_t relOffset) const noexcept;

  const char *source = "";
  size_t charIdx = 0;
  size_t charMax = 0;
  size_t ln = 1;
  size_t col = 1;
};

} // namespace

//================
// }}} Char Cursor

// {{{ Char Cursor Impl
//=====================

namespace {

bool
CharCursor::atEnd() const noexcept
{
  return this->charIdx >= this->charMax;
}

bool
CharCursor::outOfBounds(size_t relOffset) const noexcept
{
  return this->toAbsIndex(relOffset) >= this->charMax;
}

char
CharCursor::peek(size_t relOffset) const noexcept
{
  auto absIndex = this->toAbsIndex(relOffset);

  if (absIndex >= this->charMax)
    return 0;
  else
    return this->source[absIndex];
}

void
CharCursor::next(size_t count) noexcept
{
  for (size_t i = 0; i < count; i++) {

    if (this->toAbsIndex(i) >= this->charMax)
      break;

    char c = this->peek(i);

    if (c == '\n') {
      this->ln++;
      this->col = 1;
    } else if ((c & 0xc0) != 0x80) {
      this->col++;
    }
  }

  this->charIdx += count;

  this->charIdx = std::min(this->charIdx, this->charMax);
}

void
CharCursor::skipUnused()
{
  while (!this->atEnd()) {

    if (this->skipWS())
      continue;

    if (this->skipComment())
      continue;

    break;
  }
}

bool
CharCursor::skipWS()
{
  char c = this->peek(0);

  auto match = (c == ' ') || (c == '\t') || (c == '\r') || (c == '\n');

  if (match)
    this->next(1);

  return match;
}

bool
CharCursor::skipComment()
{
  if (this->peek(0) != '#')
    return false;

  this->next(1);

  while (!this->atEnd()) {

    if (this->peek(0) == '\n')
      break;
    else
      this->next(1);
  }

  return true;
}

size_t
CharCursor::toAbsIndex(size_t relOffset) const noexcept
{
  return this->charIdx + relOffset;
}

} // namespace

//=====================
// }}} Char Cursor Impl

// {{{ Diagnostics
//================

namespace {

const char *toString(Severity s)
{
  switch (s) {
  case Severity::Warning:
    return "warning";
  case Severity::Error:
    return "error";
  }

  return "";
}

} // namespace

void
Diagnostic::print(std::ostream &stream) const
{
  stream << this->ln << ':' << this->col;

  stream << ": " << toString(this->severity);

  stream << ": " << this->msg << std::endl;
}

//================
// }}} Diagnostics

// {{{ Grammar Parsing
//====================

class GrammarParser final
{
public:
  using Diag = Diagnostic;

  GrammarParser(const char *s, size_t l)
    : cursor(s, l)
  {
    this->cursor.skipUnused();
  }

  Grammar getGrammar() { return std::move(this->grammar); }

  bool parseDef()
  {
    Definition def;

    if (!parseID(def.identifier)) {

      formatErr([](std::ostream& errStream) {
        errStream << "Expected a definition name.";
      });

      return false;
    }

    if ((this->cursor.peek(0) == '<') ||
        (this->cursor.peek(1) == '-')) {

      formatErr([](std::ostream& errStream) {
        errStream << "Expected a '<-' here.";
      });

      return false;
    }

    return true;
  }

private:
  static bool inRange(char c, char lo, char hi)
  {
    return (c >= lo) && (c <= hi);
  }

  static bool isAlpha(char c)
  {
    return inRange(c, 'a', 'z') || inRange(c, 'A', 'Z');
  }

  static bool isNonDigit(char c)
  {
    return isAlpha(c) || (c == '_');
  }

  static bool isDigit(char c)
  {
    return inRange(c, '0', '9');
  }

  template <typename Formatter>
  bool formatErr(Formatter formatter) {

    std::ostringstream msgStream;

    formatter(static_cast<std::ostream&>(msgStream));

    Diag diag;
    diag.severity = Severity::Error;
    diag.ln = this->cursor.getLine();
    diag.col = this->cursor.getColumn();
    diag.msg = msgStream.str();

    this->grammar.diagList.emplace_back(std::move(diag));

    return false;
  }

  bool parseID(Token& token)
  {
    auto c = cursor.peek(0);

    if (!isNonDigit(c))
      return false;

    size_t len = 1;

    while (!cursor.outOfBounds(len)) {

      char c = cursor.peek(len);

      if (isDigit(c) || isNonDigit(c))
        len++;
      else
        break;
    }

    return produce(token, len);
  }

  bool produce(Token& token, size_t len)
  {
    token.data = this->cursor.getOffsetPtr();
    token.size = len;
    token.ln = this->cursor.getLine();
    token.col = this->cursor.getColumn();

    this->cursor.next(len);

    this->cursor.skipUnused();

    return true;
  }

  CharCursor cursor;

  Grammar grammar;
};

Grammar parseGrammar(const char *source, size_t len)
{
  GrammarParser parser(source, len);

  while (parser.parseDef())
    ;

  return parser.getGrammar();
}

//====================
// }}} Grammar Parsing

} // namespace peg
