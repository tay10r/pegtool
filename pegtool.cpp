#include <pegtool.h>

#include <algorithm>
#include <iomanip>
#include <map>
#include <memory>
#include <new>
#include <sstream>
#include <string>
#include <vector>

#include <cassert>
#include <cstring>

// Note to readers:
//
// This file is maintained using Vim's fold markers. To view this file with a
// more organization / easier navigation, ensure that Vim is using fold markers
// by running the command:
//
// :set foldmethod=marker

namespace peg {

namespace {

class GrammarParser;

} // namespace

// {{{ UTF-8
//==========

namespace {

using UChar = unsigned char;

/// @return The number of bytes that successfully match a UTF-8 sequence.
size_t
utf8Length(UChar prefixByte)
{
  // branchless utf-8

  // Taken from https://nullprogram.com/blog/2017/10/06

  static const UChar lengths[]{
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 3, 3, 4, 0
  };

  UChar len = lengths[prefixByte >> 3];

  return len + !len;
}

size_t
getUtf8ValidLength(const char* basePtr, size_t offset, size_t baseLen)
{
  if (offset >= baseLen)
    return 0;

  auto len = utf8Length(UChar(basePtr[offset]));

  for (size_t i = 1; (i < len) && ((offset + i) < baseLen); i++) {
    if ((basePtr[offset + i] & 0xc0) != 0x80)
      return i;
  }

  return len;
}

/// @return False if the UTF-32 character is not valid Unicode.
bool
convertUtf32To8(char32_t in, std::string& out)
{
  // See section 3 of RFC 3629 on "UTF-8 definition"

  if (in <= 0x7f) {
    out.push_back(static_cast<char>(in));
  } else if ((in >= 0x80) && (in <= 0x7ff)) {
    out.push_back(static_cast<char>((in >> 6) & 0x1f) | 0xc0);
    out.push_back(static_cast<char>((in >> 0) & 0x3f) | 0x80);
  } else if ((in >= 0x800) && (in <= 0xffff)) {
    out.push_back(static_cast<char>(((in >> 0x0c) & 0x0f) | 0xe0));
    out.push_back(static_cast<char>(((in >> 0x06) & 0x3f) | 0x80));
    out.push_back(static_cast<char>(((in >> 0x00) & 0x3f) | 0x80));
  } else if ((in >= 0x10000) && (in <= 0x10ffff)) {
    out.push_back(static_cast<char>(((in >> 0x12) & 0x03) | 0xf0));
    out.push_back(static_cast<char>(((in >> 0x0c) & 0x3f) | 0x80));
    out.push_back(static_cast<char>(((in >> 0x06) & 0x3f) | 0x80));
    out.push_back(static_cast<char>(((in >> 0x00) & 0x3f) | 0x80));
  } else {
    return false;
  }

  return true;
}

} // namespace

//==========
// }}} UTF-8

// {{{ Position
//=============

namespace {

/// Used to indicate a position in the grammar file. The line and column
/// information is stored for diagnostic purposes and the character index is
/// stored for library purposes.
struct Position final
{
  /// The line number of the position, starting at 1.
  size_t ln = 0;
  /// The column number of the position, starting at 1.
  size_t col = 0;
  /// The character index of the position.
  size_t idx = 0;
};

} // namespace

//=============
// }}} Position

// {{{ Token
//==========

namespace {

/// This structure contains information regarding an identifier or symbol given
/// in the grammar file. It appears in the grammar structures in case a
/// diagnostic needs to be issued about one of them.
class Token final
{
public:
  static Token makeUnion(const Token& a, const Token& b)
  {
    Token out;

    if (a.data < b.data) {
      out.data = a.data;
      out.size = (b.data - a.data) + b.size;
      out.start = a.start;
    } else {
      out.data = b.data;
      out.size = (a.data - b.data) + a.size;
      out.start = b.start;
    }

    return out;
  }

  Token() = default;

  Token(const char* d, size_t s, const Position& p)
    : data(d)
    , size(s)
    , start(p)
  {}

  Position getPosition() const noexcept { return this->start; }

  size_t getLength() const noexcept { return this->size; }

  std::string toString() const;

  bool operator==(const char* s) const noexcept;

  char operator[](size_t i) const noexcept;

private:
  /// A pointer to the start of the token character data.
  const char* data = "";
  /// The number of bytes in the token character data.
  size_t size = 0;
  /// The starting position of the token.
  Position start;
};

std::string
Token::toString() const
{
  return std::string(this->data, this->size);
}

bool
Token::operator==(const char* s) const noexcept
{
  auto l = std::strlen(s);

  if (l != this->size)
    return false;

  return std::memcmp(s, this->data, this->size) == 0;
}

} // namespace

//==========
// }}} Token

// {{{ Char Cursor
//================

namespace {

class CharCursor final
{
public:
  static const char* getLinePtr(const char* src,
                                size_t srcLen,
                                size_t idx) noexcept
  {
    if (idx >= srcLen)
      return src + srcLen;

    for (size_t i = idx; i > 0; i--) {
      if (src[i - 1] == '\n')
        return src + i;
    }

    return src;
  }

  static size_t getStartingIndexOfLine(const char* src,
                                       size_t srcLen,
                                       size_t idx) noexcept
  {
    if (idx >= srcLen)
      return srcLen;

    for (size_t i = idx; i > 0; i--) {
      if (src[i - 1] == '\n')
        return i;
    }

    return 0;
  }

  static size_t getLineSize(const char* src,
                            size_t srcLen,
                            size_t lineOffset) noexcept
  {
    for (size_t i = lineOffset; i < srcLen; i++) {
      if (src[i] == '\n')
        return i - lineOffset;
    }

    return srcLen - lineOffset;
  }

  CharCursor(const char* s, size_t len)
    : source(s)
    , charMax(len)
  {}

  bool atEnd() const noexcept;

  const char* getStartingPtr() const noexcept { return this->source; }

  size_t getSourceLength() const noexcept { return this->charMax; }

  const char* getOffsetPtr() const noexcept
  {
    return this->source + this->charIdx;
  }

  size_t getLine() const noexcept { return this->ln; }

  size_t getColumn() const noexcept { return this->col; }

  size_t getOffset() const noexcept { return this->charIdx; }

  bool outOfBounds(size_t relOffset) const noexcept;

  char peek(size_t offset) const noexcept;

  void next(size_t count) noexcept;

  void skipUnused();

  struct State final
  {
    size_t idx = 0;
    size_t ln = 0;
    size_t col = 0;
  };

  State getState() const noexcept
  {
    State state;
    state.idx = this->charIdx;
    state.ln = this->ln;
    state.col = this->col;
    return state;
  }

  void restoreState(const State& state) noexcept
  {
    this->charIdx = state.idx;
    this->ln = state.ln;
    this->col = state.col;
  }

private:
  bool skipWS();

  bool skipComment();

  size_t toAbsIndex(size_t relOffset) const noexcept;

  const char* source = "";
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

/// This enumerates the level of severity of a diagnostic. Only diagnostics with
/// a severity of @ref Severity::Error should halt further processing of the
/// grammar.
enum class Severity
{
  Note,
  Warning,
  Error
};

/// Contains information regarding an error or a warning issued by the grammar
/// parser. It contains extra data for showing the line and range of characters
/// that caused the diagnostic.
class Diagnostic final
{
public:
  template<typename Formatter>
  static Diagnostic make(const Token& tok,
                         const char* src,
                         size_t srcLen,
                         Formatter formatter,
                         Severity severity = Severity::Error)
  {
    std::ostringstream msgStream;

    formatter(static_cast<std::ostream&>(msgStream));

    auto pos = tok.getPosition();

    Diagnostic diag;
    diag.severity = severity;
    diag.start = pos;
    diag.msg = msgStream.str();
    diag.lnPtr = CharCursor::getLinePtr(src, srcLen, pos.idx);
    diag.lnOffset = CharCursor::getStartingIndexOfLine(src, srcLen, pos.idx);
    diag.lnSize = CharCursor::getLineSize(src, srcLen, diag.lnOffset);
    diag.len = tok.getLength();

    return diag;
  }

  void print(std::ostream&) const;

  Position getStartPosition() const noexcept { return this->start; }

  size_t getLength() const noexcept { return this->len; }

  std::string getMessage() const { return this->msg; }

  Severity getSeverity() const noexcept { return this->severity; }

private:
  friend GrammarParser;

  Position start;

  size_t lnOffset = 0;

  size_t lnSize = 0;

  size_t len = 0;

  std::string msg;

  const char* lnPtr = "";

  Severity severity = Severity::Error;
};

const char*
toString(Severity s)
{
  switch (s) {
    case Severity::Note:
      return "note";
    case Severity::Warning:
      return "warning";
    case Severity::Error:
      return "error";
  }

  return "";
}

std::string
convertNumberToSpaces(size_t ln)
{
  std::ostringstream stream;

  stream << ln;

  auto s = stream.str();

  std::fill(s.begin(), s.end(), ' ');

  return s;
}

class DiagnosticFactory final
{
public:
  DiagnosticFactory(const char* s, size_t l)
    : source(s)
    , length(l)
  {}

  template<typename Formatter>
  Diagnostic make(const Token& tok,
                  Formatter formatter,
                  Severity severity = Severity::Error) const
  {
    return Diagnostic::make(tok, source, length, formatter, severity);
  }

private:
  const char* source = nullptr;

  size_t length = 0;
};

} // namespace

void
Diagnostic::print(std::ostream& stream) const
{
  stream << this->start.ln << ':' << this->start.col;

  stream << ": " << toString(this->severity);

  stream << ": " << this->msg << std::endl;

  stream << " " << this->start.ln << " | ";

  for (size_t i = 0; i < this->lnSize; i++) {

    auto symLength = utf8Length(this->lnPtr[i]);

    auto validLength = getUtf8ValidLength(this->lnPtr, i, this->lnSize);

    if (!validLength)
      break;

    if (validLength != symLength) {
      stream << "\xef\xbf\xbd";
    } else {
      for (size_t j = 0; j < symLength; j++) {
        stream << this->lnPtr[i + j];
      }
    }

    i += validLength - 1; // -1 because we're in a for-loop
  }

  stream << std::endl;

  stream << " " << convertNumberToSpaces(this->start.ln) << " | ";

  for (size_t i = this->lnOffset; i < this->start.idx; i++)
    stream << ((this->lnPtr[i - this->lnOffset] == '\t') ? '\t' : ' ');

  for (size_t i = 0; i < len; i++) {

    size_t symLnOffset = (this->start.idx + i) - this->lnOffset;

    auto symLength = getUtf8ValidLength(this->lnPtr, symLnOffset, this->lnSize);

    if (!symLength)
      break;

    stream << '~';

    i += symLength - 1;
  }

  stream << std::endl;
}

//================
// }}} Diagnostics

// {{{ Character Literals
//=======================

namespace {

class CharLiteral final
{
public:
  CharLiteral(const Token& t, std::string&& d)
    : token(t)
    , data(d)
  {}

  CharLiteral() = default;

  const Token& getToken() const noexcept { return this->token; }

  const std::string& getData() const noexcept { return this->data; }

private:
  Token token;
  /// The character literal data may not be the same as what's in the token
  /// because of escape sequences. Short string optimization should prevent
  /// anything from being allocated here.
  std::string data;
};

template<char lo, char hi>
bool
inRange(char c) noexcept
{
  return (c >= lo) && (c <= hi);
}

bool
isDigit(char c) noexcept
{
  return inRange<'0', '9'>(c);
}

char
toLower(char c) noexcept
{
  return inRange<'A', 'Z'>(c) ? (c + 32) : c;
}

bool
isHexDigit(char c) noexcept
{
  return isDigit(c) || inRange<'a', 'f'>(toLower(c));
}

size_t
getHexSequenceLength(const CharCursor& cursor, size_t startingOffset)
{
  size_t len = 0;

  for (size_t i = startingOffset; !cursor.outOfBounds(i); i++) {
    if (isHexDigit(cursor.peek(i)))
      len++;
    else
      break;
  }

  return len;
}

enum class CharErr
{
  None,
  InvalidCodePoint,
  InvalidEscapeSequence,
  InvalidUtfSequence,
  IncompleteCodePoint16,
  IncompleteCodePoint32
};

void
fillReplacementChar(std::string& str)
{
  str = "\xef\xbf\xbd";
}

size_t
getUtf8Char(const CharCursor& cursor,
            std::string& str,
            size_t offset,
            CharErr& err)
{
  err = CharErr::None;

  if (cursor.outOfBounds(offset))
    return 0;

  auto prefixByte = cursor.peek(offset);

  str.push_back(prefixByte);

  auto len = utf8Length(prefixByte);

  for (size_t i = 1; i < len; i++) {

    auto continuationByte = cursor.peek(offset + i);

    if ((continuationByte & 0xc0) != 0x80) {
      err = CharErr::InvalidUtfSequence;
      fillReplacementChar(str);
      return i;
    }

    str.push_back(continuationByte);
  }

  return len;
}

/// This function gets a character literal value from the cursor at a given
/// offset, putting the character at the end of the string container passed
/// in the parameter list.
///
/// @return The number of characters used, starting at @p offset.
size_t
getChar(const CharCursor& cursor, std::string& str, size_t offset, CharErr& err)
{
  err = CharErr::None;

  if (cursor.outOfBounds(offset))
    return 0;

  if (cursor.peek(offset) != '\\')
    return getUtf8Char(cursor, str, offset, err);

  if (cursor.outOfBounds(offset + 1)) {
    // TODO
    return 0;
  }

  auto secondChar = cursor.peek(offset + 1);

  switch (secondChar) {
    case 'n':
      str.push_back('\n');
      return 2;
    case 'r':
      str.push_back('\r');
      return 2;
    case 't':
      str.push_back('\t');
      return 2;
    case '\\':
    case '\'':
    case '"':
      str.push_back(secondChar);
      return 2;
  }

  if (inRange<'0', '2'>(secondChar) &&
      inRange<'0', '7'>(cursor.peek(offset + 2)) &&
      inRange<'0', '7'>(cursor.peek(offset + 3))) {
    UChar value = (secondChar - '0') * 64;
    value += (cursor.peek(offset + 2) - '0') * 8;
    value += (cursor.peek(offset + 3) - '0');
    str.push_back(char(value));
    return 4;
  }

  if (inRange<'0', '7'>(secondChar) &&
      inRange<'0', '7'>(cursor.peek(offset + 2))) {
    UChar value = (secondChar - '0') * 8;
    value += (cursor.peek(offset + 2) - '0');
    str.push_back(char(value));
    return 3;
  }

  if (inRange<'0', '7'>(secondChar)) {
    str.push_back(secondChar - '0');
    return 2;
  }

  auto getHexValue = [](char c) {
    if (inRange<'0', '9'>(c))
      return c - '0';
    else
      return (toLower(c) - 'a') + 10;
  };

  if ((toLower(secondChar) == 'x') && isHexDigit(cursor.peek(offset + 2)) &&
      isHexDigit(cursor.peek(offset + 3))) {
    UChar value = 0;
    value += getHexValue(cursor.peek(offset + 2)) * 16;
    value += getHexValue(cursor.peek(offset + 3));
    str.push_back(char(value));
    return 4;
  }

  if ((toLower(secondChar) == 'x') && isHexDigit(cursor.peek(offset + 2))) {
    str.push_back(getHexValue(cursor.peek(offset + 2)));
    return 3;
  }

  if (secondChar == 'u') {

    auto len = getHexSequenceLength(cursor, offset + 2);

    if (len != 4) {
      err = CharErr::IncompleteCodePoint16;
      return len + 2; // +2 for '\\' and 'u'
    }

    char32_t value = 0;

    value |= char32_t(getHexValue(cursor.peek(offset + 2))) << 12;
    value |= char32_t(getHexValue(cursor.peek(offset + 3))) << 8;
    value |= char32_t(getHexValue(cursor.peek(offset + 4))) << 4;
    value |= char32_t(getHexValue(cursor.peek(offset + 5)));

    if (!convertUtf32To8(value, str))
      err = CharErr::InvalidCodePoint;

    return len + 2;
  }

  if (secondChar == 'U') {

    auto len = getHexSequenceLength(cursor, offset + 2);

    if (len != 8) {
      err = CharErr::IncompleteCodePoint32;
      return len + 2; // +2 for '\\' and 'U'
    }

    char32_t value = 0;

    value |= char32_t(getHexValue(cursor.peek(offset + 2))) << 28;
    value |= char32_t(getHexValue(cursor.peek(offset + 3))) << 24;
    value |= char32_t(getHexValue(cursor.peek(offset + 4))) << 20;
    value |= char32_t(getHexValue(cursor.peek(offset + 5))) << 16;
    value |= char32_t(getHexValue(cursor.peek(offset + 6))) << 12;
    value |= char32_t(getHexValue(cursor.peek(offset + 7))) << 8;
    value |= char32_t(getHexValue(cursor.peek(offset + 8))) << 4;
    value |= char32_t(getHexValue(cursor.peek(offset + 9)));

    if (!convertUtf32To8(value, str))
      err = CharErr::InvalidCodePoint;

    return len + 2;
  }

  err = CharErr::InvalidEscapeSequence;

  return 2;
}

} // namespace

//=======================
// }}} Character Literals

// {{{ Expression Tree
//====================

namespace {

class ExprVisitor;
class ExprMutator;

class Expr
{
public:
  virtual ~Expr() = default;

  virtual bool accept(ExprVisitor&) const = 0;

  virtual bool acceptMutator(const ExprMutator&) = 0;
};

using UniqueExprPtr = std::unique_ptr<Expr>;

class GroupExpr;
class LiteralExpr;
class ClassExpr;
class DotExpr;
class PrefixExpr;
class ReferenceExpr;
class SlashExpr;
class SuffixExpr;
class Sequence;

class ExprVisitor
{
public:
  virtual ~ExprVisitor() = default;
  virtual bool visit(const GroupExpr&) = 0;
  virtual bool visit(const LiteralExpr&) = 0;
  virtual bool visit(const ClassExpr&) = 0;
  virtual bool visit(const DotExpr&) = 0;
  virtual bool visit(const ReferenceExpr&) = 0;
  virtual bool visit(const SlashExpr&) = 0;
  virtual bool visit(const SuffixExpr&) = 0;
  virtual bool visit(const PrefixExpr&) = 0;
  virtual bool visit(const Sequence&) = 0;
};

class ExprMutator
{
public:
  virtual ~ExprMutator() = default;
  virtual bool mutate(GroupExpr&) const = 0;
  virtual bool mutate(LiteralExpr&) const = 0;
  virtual bool mutate(ClassExpr&) const = 0;
  virtual bool mutate(DotExpr&) const = 0;
  virtual bool mutate(ReferenceExpr&) const = 0;
  virtual bool mutate(SlashExpr&) const = 0;
  virtual bool mutate(SuffixExpr&) const = 0;
  virtual bool mutate(PrefixExpr&) const = 0;
  virtual bool mutate(Sequence&) const = 0;
};

class LiteralExpr final : public Expr
{
public:
  LiteralExpr(std::string&& d)
    : data(std::move(d))
  {}

  bool accept(ExprVisitor& v) const override { return v.visit(*this); }

  bool acceptMutator(const ExprMutator& m) override { return m.mutate(*this); }

  const std::string& getData() const noexcept { return this->data; }

private:
  friend GrammarParser;

  LiteralExpr() = default;

  std::string data;

  Token token;
};

class DotExpr final : public Expr
{
public:
  DotExpr(const Token& t)
    : dotToken(t)
  {}

  bool accept(ExprVisitor& v) const override { return v.visit(*this); }

  bool acceptMutator(const ExprMutator& m) override { return m.mutate(*this); }

private:
  Token dotToken;
};

class ReferenceExpr final : public Expr
{
public:
  ReferenceExpr(const Token& t)
    : token(t)
  {}

  bool accept(ExprVisitor& v) const override { return v.visit(*this); }

  bool acceptMutator(const ExprMutator& m) override { return m.mutate(*this); }

  const Token& getToken() const noexcept { return this->token; }

  size_t getRuleIndex() const noexcept { return this->ruleIndex; }

  void setRuleIndex(size_t index) noexcept { this->ruleIndex = index; }

  bool isResolved() const noexcept
  {
    return this->ruleIndex != std::numeric_limits<size_t>::max();
  }

  std::string toString() const { return this->token.toString(); }

private:
  Token token;

  size_t ruleIndex = std::numeric_limits<size_t>::max();
};

class ClassExpr final : public Expr
{
public:
  class EqualitySubExpr;
  class IntervalSubExpr;

  class SubExprVisitor
  {
  public:
    virtual ~SubExprVisitor() = default;

    virtual bool visit(const EqualitySubExpr&) = 0;

    virtual bool visit(const IntervalSubExpr&) = 0;
  };

  class SubExpr
  {
  public:
    virtual ~SubExpr() = default;

    virtual bool accept(SubExprVisitor&) const = 0;
  };

  class EqualitySubExpr final : public SubExpr
  {
    CharLiteral expected;

  public:
    EqualitySubExpr(CharLiteral&& e)
      : expected(e)
    {}

    bool accept(SubExprVisitor& v) const override { return v.visit(*this); }

    const CharLiteral& getExpected() const noexcept { return this->expected; }
  };

  class IntervalSubExpr final : public SubExpr
  {
    CharLiteral lo;
    CharLiteral hi;

  public:
    IntervalSubExpr(CharLiteral&& l, CharLiteral&& h)
      : lo(std::move(l))
      , hi(std::move(h))
    {}

    bool accept(SubExprVisitor& v) const override { return v.visit(*this); }

    const CharLiteral& getLowerBound() const noexcept { return this->lo; }

    const CharLiteral& getUpperBound() const noexcept { return this->hi; }
  };

  bool accept(ExprVisitor& v) const override { return v.visit(*this); }

  bool acceptMutator(const ExprMutator& m) override { return m.mutate(*this); }

  /// @return True if visiting any one of the sub expressions returns true.
  bool acceptSubExprVisitor(SubExprVisitor& v) const noexcept
  {
    bool success = false;

    for (const auto& subExpr : this->subExprs)
      success |= subExpr->accept(v);

    return success;
  }

  void appendSubExpr(SubExpr* subExpr) { this->subExprs.emplace_back(subExpr); }

  size_t getSubExprCount() const noexcept { return this->subExprs.size(); }

private:
  std::vector<std::unique_ptr<SubExpr>> subExprs;
};

class SuffixExpr final : public Expr
{
public:
  enum class Kind
  {
    None,
    Question,
    Star,
    Plus
  };

  using TokenKindPair = std::pair<Token, Kind>;

  bool accept(ExprVisitor& v) const override { return v.visit(*this); }

  bool acceptMutator(const ExprMutator& m) override { return m.mutate(*this); }

  bool acceptPrimaryExprMutator(const ExprMutator& m)
  {
    if (this->primaryExpr)
      return this->primaryExpr->acceptMutator(m);
    else
      return false;
  }

  bool acceptPrimaryExprVisitor(ExprVisitor& v) const
  {
    if (this->primaryExpr)
      return this->primaryExpr->accept(v);
    else
      return false;
  }

  Kind getKind() const noexcept { return this->tokenKindPair.second; }

private:
  friend GrammarParser;

  TokenKindPair tokenKindPair;

  UniqueExprPtr primaryExpr;
};

class PrefixExpr final : public Expr
{
public:
  enum class Kind
  {
    None,
    And,
    Not
  };

  bool accept(ExprVisitor& v) const override { return v.visit(*this); }

  bool acceptMutator(const ExprMutator& m) override { return m.mutate(*this); }

  bool acceptSuffixExprMutator(const ExprMutator& m)
  {
    return m.mutate(this->suffixExpr);
  }

  bool acceptSuffixExprVisitor(ExprVisitor& v) const
  {
    return v.visit(this->suffixExpr);
  }

private:
  friend GrammarParser;

  SuffixExpr suffixExpr;
};

class Sequence final : public Expr
{
public:
  size_t getPrefixExprCount() const noexcept;

  bool accept(ExprVisitor& v) const override { return v.visit(*this); }

  bool acceptMutator(const ExprMutator& m) override { return m.mutate(*this); }

  bool acceptPrefixExprMutator(const ExprMutator& m)
  {
    auto success = true;

    for (auto& prefixExpr : this->prefixExprs)
      success &= m.mutate(prefixExpr);

    return success;
  }

  bool acceptPrefixExprVisitor(ExprVisitor& v) const
  {
    auto success = true;

    for (const auto& prefixExpr : this->prefixExprs)
      success &= v.visit(prefixExpr);

    return success;
  }

private:
  friend GrammarParser;

  std::vector<PrefixExpr> prefixExprs;
};

class SlashExpr final : public Expr
{
public:
  using SlashSeqPair = std::pair<Token, Sequence>;

  bool accept(ExprVisitor& v) const override { return v.visit(*this); }

  bool acceptMutator(const ExprMutator& m) override { return m.mutate(*this); }

  size_t getSequenceCount() const noexcept
  {
    return 1 + otherSequenceExprs.size();
  }

  bool acceptSequenceMutator(size_t seqIndex, const ExprMutator& m)
  {
    if (seqIndex == 0)
      return m.mutate(firstSequence);

    if ((seqIndex - 1) < this->otherSequenceExprs.size())
      return m.mutate(this->otherSequenceExprs[seqIndex - 1].second);

    return false;
  }

  bool acceptSequenceVisitor(size_t seqIndex, ExprVisitor& v) const
  {
    if (seqIndex == 0)
      return v.visit(firstSequence);

    if ((seqIndex - 1) < this->otherSequenceExprs.size())
      return v.visit(this->otherSequenceExprs[seqIndex - 1].second);

    return false;
  }

private:
  friend GrammarParser;

  Sequence firstSequence;

  std::vector<SlashSeqPair> otherSequenceExprs;
};

class RecursiveExprVisitor : public ExprVisitor
{
public:
  virtual ~RecursiveExprVisitor() = default;

  virtual bool visit(const GroupExpr&) override { return true; }

  virtual bool visit(const LiteralExpr&) override { return true; }

  virtual bool visit(const ClassExpr&) override { return true; }

  virtual bool visit(const DotExpr&) override { return true; }

  virtual bool visit(const ReferenceExpr&) override { return true; }

  virtual bool visit(const SlashExpr& slashExpr) override
  {
    size_t seqCount = slashExpr.getSequenceCount();

    auto success = true;

    for (size_t i = 0; i < seqCount; i++)
      success &= slashExpr.acceptSequenceVisitor(i, *this);

    return true;
  }

  virtual bool visit(const SuffixExpr& suffixExpr) override
  {
    return suffixExpr.acceptPrimaryExprVisitor(*this);
  }

  virtual bool visit(const PrefixExpr& prefixExpr) override
  {
    return prefixExpr.acceptSuffixExprVisitor(*this);
  }

  virtual bool visit(const Sequence& sequenceExpr) override
  {
    return sequenceExpr.acceptPrefixExprVisitor(*this);
  }
};

} // namespace

//====================
// }}} Expression Tree

// {{{ Definition
//===============

namespace {

class Definition final
{
public:
  std::string getName() const { return this->identifier.toString(); }

  const Token& getIdentifierToken() const noexcept { return this->identifier; }

  bool hasName(const char* name) const { return identifier == name; }

  bool acceptExprVisitor(ExprVisitor& v) const { return v.visit(this->expr); }

  bool acceptExprMutator(const ExprMutator& m) { return m.mutate(this->expr); }

private:
  friend GrammarParser;

  Definition() = default;

  Token identifier;

  Token arrow;

  SlashExpr expr;
};

} // namespace

//===============
// }}} Definition

// {{{ Parse Tree Printer
//=======================

namespace {

class ParseTreePrinter final
{
public:
  ParseTreePrinter(std::ostream& s)
    : stream(s)
  {}

  void print(const Leaf& leaf)
  {
    this->indent() << '\'';

    const char* data = leaf.getData();

    for (size_t i = 0; i < leaf.getLength(); i++) {

      char c = data[i];

      switch (c) {
        case '\t':
          this->stream << "\\t";
          continue;
        case '\r':
          this->stream << "\\r";
          continue;
        case '\n':
          this->stream << "\\n";
          continue;
        case '\'':
          this->stream << "\\'";
          continue;
        case '\\':
          this->stream << "\\\\";
          continue;
      }

      if (static_cast<UChar>(c) < static_cast<UChar>(' ')) {
        this->stream << "\\x";
        this->stream << static_cast<char>((c >> 4) + '0');
        this->stream << static_cast<char>((c & 15) + '0');
        continue;
      }

      this->stream << data[i];
    }

    this->stream << '\'' << std::endl;
  }

  void print(const Node& node)
  {
    this->indent() << node.getName() << ':' << std::endl;

    this->indentLevel++;

    if ((node.getChildCount() == 0) && (node.getLeafCount() == 0))
      this->indent() << "(empty)" << std::endl;

    for (size_t i = 0; i < node.getLeafCount(); i++)
      this->print(node.getLeaf(i));

    for (size_t i = 0; i < node.getChildCount(); i++)
      this->print(node.getChild(i));

    this->indentLevel--;
  }

private:
  auto indent() -> std::ostream&
  {
    for (size_t i = 0; i < this->indentLevel; i++)
      this->stream << "  ";

    return this->stream;
  }

  std::ostream& stream;

  size_t indentLevel = 0;
};

} // namespace

//=====================
// }}} ParseTreePrinter

// {{{ Symbols
//============

namespace {

class ErrorImpl final : public Error
{
public:
  static const ErrorImpl& null()
  {
    static ErrorImpl err;
    return err;
  }

  ErrorImpl(std::string&& m, const char* d, size_t l)
    : message(std::move(m))
    , data(d)
    , length(l)
  {}

  const char* getMessage() const noexcept override { return message.c_str(); }

  size_t getMessageLength() const noexcept override { return message.size(); }

  const char* getData() const noexcept override { return data; }

  size_t getLength() const noexcept override { return length; }

private:
  ErrorImpl() = default;

  std::string message;

  const char* data = nullptr;

  size_t length = 0;
};

const Leaf&
nullLeaf()
{
  static Leaf nullLeaf("", 0);
  return nullLeaf;
}

class NodeImpl final : public Node
{
public:
  static const NodeImpl& null()
  {
    static NodeImpl branch;
    return branch;
  }

  NodeImpl(std::string&& n)
    : name(std::move(n))
  {}

  void appendLeaf(Leaf&& leafNode)
  {
    this->leafs.emplace_back(std::move(leafNode));
  }

  void appendChild(std::unique_ptr<NodeImpl>&& child)
  {
    this->children.emplace_back(std::move(child));
  }

  const Error& getError(size_t index) const noexcept override
  {
    if (index >= this->errors.size())
      return ErrorImpl::null();
    else
      return this->errors[index];
  }

  size_t getErrorCount() const noexcept override { return this->errors.size(); }

  const Leaf& getLeaf(size_t index) const noexcept override
  {
    if (index >= this->leafs.size())
      return nullLeaf();
    else
      return this->leafs[index];
  }

  size_t getLeafCount() const noexcept override { return this->leafs.size(); }

  const char* getName() const noexcept override { return this->name.c_str(); }

  bool hasName(const char* n) const noexcept override
  {
    return this->name == n;
  }

  size_t getChildCount() const noexcept override
  {
    return this->children.size();
  }

  const Node& getChild(size_t index) const noexcept override
  {
    if (index >= this->children.size())
      return null();
    else
      return *this->children[index];
  }

private:
  NodeImpl() = default;

  std::string name;
  std::vector<Leaf> leafs;
  std::vector<Error> errors;
  std::vector<std::unique_ptr<NodeImpl>> children;
};

} // namespace

void
Node::print(std::ostream& stream) const
{
  ParseTreePrinter printer(stream);

  printer.print(*this);
}

//============
// }}} Symbols

// {{{ Grammar Parsing
//====================

namespace {

class GrammarParser final
{
public:
  using Diag = Diagnostic;

  GrammarParser(const char* s, size_t l)
    : cursor(s, l)
  {
    this->cursor.skipUnused();
  }

  auto getDiagnostics() -> std::vector<Diagnostic>
  {
    return std::move(this->diagnostics);
  }

  auto getDefinitions() -> std::vector<Definition>
  {
    return std::move(this->definitions);
  }

  bool parseDef()
  {
    if (this->cursor.atEnd())
      return false;

    Definition def;

    if (!parseID(def.identifier)) {

      Token token;

      produce(token, 1);

      formatErr(token, [](std::ostream& errStream) {
        errStream << "Expected a definition name here.";
      });

      return false;
    }

    if (!parseExactly(def.arrow, "<-")) {
      formatErr(def.identifier, [](std::ostream& errStream) {
        errStream << "Expected a '<-' after this.";
      });
      return false;
    }

    bool errFlag = false;

    if (!parseExpr(def.expr, errFlag)) {
      if (!errFlag) {
        formatErr(def.arrow, [](std::ostream& errStream) {
          errStream << "Expected an expression after this.";
        });
      }
      return false;
    }

    auto defName = def.getName();

    const auto* existingDefTok = this->findExistingDefinition(defName.c_str());

    if (existingDefTok) {

      formatErr(def.identifier, [&defName](std::ostream& errStream) {
        errStream << "'" << defName << "' is already defined.";
      });

      formatNote(*existingDefTok, [](std::ostream& errStream) {
        errStream << "Previously defined here.";
      });

      return false;
    }

    this->definitions.emplace_back(std::move(def));

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

  static bool isNonDigit(char c) { return isAlpha(c) || (c == '_'); }

  static bool isDigit(char c) { return inRange(c, '0', '9'); }

  const Token* findExistingDefinition(const char* name) const
  {
    for (const auto& def : this->definitions) {
      if (def.hasName(name))
        return &def.getIdentifierToken();
    }

    return nullptr;
  }

  bool formatCharErr(const Token& tok, CharErr charErr)
  {
    this->formatErr(tok, [charErr](std::ostream& errStream) {
      switch (charErr) {
        case CharErr::None:
          break;
        case CharErr::IncompleteCodePoint16:
          errStream << "Universal character name requires 4 hex digits.";
          break;
        case CharErr::IncompleteCodePoint32:
          errStream << "Universal character name requires 8 hex digits.";
          break;
        case CharErr::InvalidCodePoint:
          errStream << "This is not a valid code point.";
          break;
        case CharErr::InvalidEscapeSequence:
          errStream << "This is not a recognized escape sequence.";
          break;
        case CharErr::InvalidUtfSequence:
          errStream << "This UTF sequence is invalid.";
          break;
      }
    });

    return false;
  }

  template<typename Formatter>
  bool formatErr(const Token& tok, Formatter formatter)
  {
    auto diag = Diagnostic::make(tok,
                                 this->cursor.getStartingPtr(),
                                 this->cursor.getSourceLength(),
                                 formatter);

    this->diagnostics.emplace_back(std::move(diag));

    return false;
  }

  template<typename Formatter>
  bool formatNote(const Token& tok, Formatter formatter)
  {
    auto diag = Diagnostic::make(tok,
                                 this->cursor.getStartingPtr(),
                                 this->cursor.getSourceLength(),
                                 formatter,
                                 Severity::Note);

    this->diagnostics.emplace_back(std::move(diag));

    return false;
  }

  UniqueExprPtr parseLiteralExpr(bool& errFlag)
  {
    char first = this->cursor.peek(0);

    if ((first != '\'') && (first != '"'))
      return nullptr;

    size_t len = 1;

    std::string data;

    while (!this->cursor.outOfBounds(len)) {

      auto last = this->cursor.peek(len);

      if (last == first) {

        using UPtr = std::unique_ptr<LiteralExpr>;

        auto literalExpr = UPtr(new LiteralExpr(std::move(data)));

        produce(literalExpr->token, len + 1);

        return UniqueExprPtr(literalExpr.release());
      }

      CharErr charErr = CharErr::None;

      size_t delta = getChar(this->cursor, data, len, charErr);

      if (charErr != CharErr::None) {

        this->cursor.next(len);

        Token invalidCharToken;

        this->produce(invalidCharToken, delta);

        formatCharErr(invalidCharToken, charErr);

        errFlag = true;

        return nullptr;
      }

      if (!delta)
        break;

      len += delta;
    }

    Token leftQuoteChar;

    produce(leftQuoteChar, 1);

    formatErr(leftQuoteChar, [first](std::ostream& errStream) {
      errStream << "Missing '";

      if (first == '\'')
        errStream << "\\'";
      else
        errStream << '"';

      errStream << "'.";
    });

    errFlag = true;

    return nullptr;
  }

  UniqueExprPtr parseDotExpr()
  {
    Token dotToken;

    if (!parseExactly(dotToken, "."))
      return nullptr;

    return UniqueExprPtr(new DotExpr(dotToken));
  }

  UniqueExprPtr parseReferenceExpr()
  {
    auto cursorState = this->cursor.getState();

    Token token;

    if (!this->parseID(token))
      return nullptr;

    Token arrow;

    if (this->parseExactly(arrow, "<-")) {
      this->cursor.restoreState(cursorState);
      return nullptr;
    }

    return UniqueExprPtr(new ReferenceExpr(token));
  }

  void checkEmptyClassExpr(bool& errFlag)
  {
    if ((this->cursor.peek(0) != '[') || (this->cursor.peek(1) != ']'))
      return;

    errFlag = true;

    Token token;

    this->produce(token, 2);

    this->formatErr(token, [](std::ostream& stream) {
      stream << "Character classes cannot be empty.";
    });
  }

  void checkClassInterval(const CharLiteral& a,
                          const CharLiteral& b,
                          bool& errFlag)
  {
    const auto& aTok = a.getToken();
    const auto& bTok = b.getToken();

    const auto& aData = a.getData();
    const auto& bData = b.getData();

    if (aData == bData) {
      // TODO
    }

    if (aData > bData) {

      this->formatErr(Token::makeUnion(aTok, bTok), [](std::ostream& stream) {
        stream << "Character range is out of order.";
      });

      errFlag = true;

      return;
    }
  }

  UniqueExprPtr parseClassExpr(bool& errFlag)
  {
    checkEmptyClassExpr(errFlag);

    if (errFlag)
      return nullptr;

    Token leftBracket;

    bool skipUnused = false;

    if (!this->parseExactly(leftBracket, "[", skipUnused))
      return nullptr;

    if (errFlag)
      return nullptr;

    using EqualityExpr = ClassExpr::EqualitySubExpr;
    using IntervalExpr = ClassExpr::IntervalSubExpr;

    std::unique_ptr<ClassExpr> classExpr(new ClassExpr());

    size_t bracketBalance = 0;

    while (!this->cursor.atEnd()) {

      if (this->cursor.peek(0) == '[') {

        bracketBalance++;

      } else if (this->cursor.peek(0) == ']') {

        if (!bracketBalance) {

          Token rightToken;

          this->produce(rightToken, 1);

          return UniqueExprPtr(classExpr.release());
        }

        bracketBalance--;
      }

      CharErr charErr = CharErr::None;

      CharLiteral first;

      if (!this->parseCharLiteral(first, charErr)) {

        if (charErr != CharErr::None) {
          errFlag = true;
          return nullptr;
        }

        break;
      }

      if ((this->cursor.peek(0) != '-') ||
          ((this->cursor.peek(1) == ']') && !bracketBalance)) {
        classExpr->appendSubExpr(new EqualityExpr(std::move(first)));
        continue;
      }

      auto cursorState = this->cursor.getState();

      this->cursor.next(1);

      CharLiteral second;

      if (this->parseCharLiteral(second, charErr)) {

        checkClassInterval(first, second, errFlag);

        if (errFlag)
          return nullptr;

        using I = IntervalExpr;

        classExpr->appendSubExpr(new I(std::move(first), std::move(second)));

        continue;
      }

      if (charErr != CharErr::None) {
        errFlag = true;
        return nullptr;
      }

      classExpr->appendSubExpr(new EqualityExpr(std::move(first)));

      this->cursor.restoreState(cursorState);
    }

    this->formatErr(leftBracket, [](std::ostream& errStream) {
      errStream << "Missing ']'.";
    });

    errFlag = true;

    return classExpr;
  }

  UniqueExprPtr parsePrimaryExpr(bool& errFlag)
  {
    auto literalExpr = parseLiteralExpr(errFlag);
    if (literalExpr)
      return literalExpr;

    if (errFlag)
      return nullptr;

    auto referenceExpr = parseReferenceExpr();
    if (referenceExpr)
      return referenceExpr;

    auto dotExpr = parseDotExpr();
    if (dotExpr)
      return dotExpr;

    auto classExpr = parseClassExpr(errFlag);
    if (classExpr)
      return classExpr;

    return nullptr;
  }

  bool parseSuffixExpr(SuffixExpr& suffixExpr, bool& errFlag)
  {
    suffixExpr.primaryExpr = parsePrimaryExpr(errFlag);

    Token suffixToken;

    if (this->parseExactly(suffixToken, "?")) {
      suffixExpr.tokenKindPair.second = SuffixExpr::Kind::Question;
      suffixExpr.tokenKindPair.first = suffixToken;
    } else if (this->parseExactly(suffixToken, "*")) {
      suffixExpr.tokenKindPair.second = SuffixExpr::Kind::Star;
      suffixExpr.tokenKindPair.first = suffixToken;
    } else if (this->parseExactly(suffixToken, "+")) {
      suffixExpr.tokenKindPair.second = SuffixExpr::Kind::Plus;
      suffixExpr.tokenKindPair.first = suffixToken;
    }

    return !!suffixExpr.primaryExpr;
  }

  bool parsePrefixExpr(PrefixExpr& prefixExpr, bool& errFlag)
  {
    return parseSuffixExpr(prefixExpr.suffixExpr, errFlag);
  }

  bool parseSequence(Sequence& sequence, bool& errFlag)
  {
    while (!this->cursor.atEnd()) {

      PrefixExpr prefixExpr;

      if (parsePrefixExpr(prefixExpr, errFlag))
        sequence.prefixExprs.emplace_back(std::move(prefixExpr));
      else
        break;
    }

    return sequence.prefixExprs.size() > 0;
  }

  bool parseExpr(SlashExpr& expr, bool& errFlag)
  {
    if (!parseSequence(expr.firstSequence, errFlag))
      return false;

    return true;
  }

  bool parseExactly(Token& token, const char* expected, bool skipUnused = true)
  {
    size_t len = 0;

    for (;;) {

      auto expectedChar = expected[len];

      if (!expectedChar)
        return produce(token, len, skipUnused);

      if (this->cursor.peek(len) != expectedChar)
        break;

      len++;
    }

    return false;
  }

  bool parseCharLiteral(CharLiteral& literal,
                        CharErr& err,
                        bool skipUnused = false)
  {
    err = CharErr::None;

    std::string str;

    size_t len = getChar(this->cursor, str, 0, err);

    Token token;

    this->produce(token, len, skipUnused);

    if (err != CharErr::None)
      return formatCharErr(token, err);

    literal = CharLiteral(token, std::move(str));

    return true;
  }

  bool parseID(Token& token)
  {
    auto c = this->cursor.peek(0);

    if (!isNonDigit(c))
      return false;

    size_t len = 1;

    while (!this->cursor.outOfBounds(len)) {

      char c = this->cursor.peek(len);

      if (isDigit(c) || isNonDigit(c))
        len++;
      else
        break;
    }

    return this->produce(token, len);
  }

  bool produce(Token& token, size_t len, bool skipUnused = true)
  {
    token = Token(this->cursor.getOffsetPtr(), len, this->getPosition());

    this->cursor.next(len);

    if (skipUnused)
      this->cursor.skipUnused();

    return true;
  }

  Position getPosition() const noexcept
  {
    Position pos;
    pos.ln = this->cursor.getLine();
    pos.col = this->cursor.getColumn();
    pos.idx = this->cursor.getOffset();
    return pos;
  }

  std::vector<Definition> definitions;

  std::vector<Diagnostic> diagnostics;

  CharCursor cursor;
};

} // namespace

//====================
// }}} Grammar Parsing

// {{{ Symbol Resolution
//======================

namespace {

class SymbolResolver final : public ExprMutator
{
public:
  SymbolResolver(std::vector<std::string> names)
    : ruleNames(std::move(names))
  {}

  bool mutate(GroupExpr&) const override { return true; }

  bool mutate(LiteralExpr&) const override { return true; }

  bool mutate(ClassExpr&) const override { return true; }

  bool mutate(DotExpr&) const override { return true; }

  bool mutate(ReferenceExpr& referenceExpr) const override
  {
    auto tok = referenceExpr.getToken();

    auto index = this->findRule(tok);

    referenceExpr.setRuleIndex(index);

    return true;
  }

  bool mutate(SlashExpr& slashExpr) const override
  {
    auto success = true;

    for (size_t i = 0; i < slashExpr.getSequenceCount(); i++)
      success &= slashExpr.acceptSequenceMutator(i, *this);

    return success;
  }

  bool mutate(SuffixExpr& suffixExpr) const override
  {
    return suffixExpr.acceptPrimaryExprMutator(*this);
  }

  bool mutate(PrefixExpr& prefixExpr) const override
  {
    return prefixExpr.acceptSuffixExprMutator(*this);
  }

  bool mutate(Sequence& sequence) const override
  {
    return sequence.acceptPrefixExprMutator(*this);
  }

private:
  size_t findRule(const Token& nameTok) const
  {
    auto name = nameTok.toString();

    for (size_t i = 0; i < ruleNames.size(); i++) {
      if (ruleNames[i] == name)
        return i;
    }

    return std::numeric_limits<size_t>::max();
  }

  std::vector<std::string> ruleNames;
};

class SymbolResolutionChecker final : public RecursiveExprVisitor
{
public:
  SymbolResolutionChecker(const char* s, size_t l)
    : diagFactory(s, l)
  {}

  std::vector<Diagnostic> getDiagnostics()
  {
    return std::move(this->diagnostics);
  }

  bool visit(const ReferenceExpr& referenceExpr) override
  {
    auto tok = referenceExpr.getToken();

    auto index = referenceExpr.getRuleIndex();

    if (index == std::numeric_limits<size_t>::max()) {
      auto diag = diagFactory.make(tok, [](std::ostream& errStream) {
        errStream << "Definition for this is missing.";
      });

      this->diagnostics.emplace_back(diag);

      return false;
    }

    return true;
  }

private:
  DiagnosticFactory diagFactory;

  std::vector<Diagnostic> diagnostics;
};

} // namespace

//======================
// }}} Symbol Resolution

// {{{ Reference Checker
//======================

namespace {

/// Checks to see if the name of a rule is found in an expression.
class ReferenceChecker final : public RecursiveExprVisitor
{
public:
  ReferenceChecker(const char* n)
    : name(n)
  {}

  bool foundReference() const noexcept { return this->foundFlag; }

  bool visit(const ReferenceExpr& referenceExpr) override
  {
    foundFlag |= referenceExpr.toString() == name;
    return true;
  }

private:
  bool foundFlag = false;
  const char* name = nullptr;
};

} // namespace

//======================
// }}} Reference Checker

// {{{ Grammar
//============

class GrammarImpl final
{
public:
  /// @brief Gets the name of the starting rule of the grammar.
  ///
  /// @return The name of the starting rule of the grammar. If the grammar is
  /// empty then a string of length zero is returned instead.
  std::string getStartRuleName() const
  {
    if (this->definitions.empty())
      return "";

    if (this->startRuleName.size() > 0)
      return this->startRuleName.c_str();

    return this->definitions[0].getName();
  }

  std::vector<std::string> getRuleNames() const
  {
    std::vector<std::string> names;

    for (const auto& def : this->definitions)
      names.emplace_back(def.getName());

    return names;
  }

  const Definition* getDefinition(size_t index) const
  {
    if (index < this->definitions.size())
      return &this->definitions[index];
    else
      return nullptr;
  }

  size_t findDefinitionIndex(const char* name) const
  {
    for (size_t i = 0; i < this->definitions.size(); i++) {
      if (this->definitions[i].hasName(name))
        return i;
    }

    return std::numeric_limits<size_t>::max();
  }

  void resolveSymbols()
  {
    SymbolResolver symResolver(this->getRuleNames());

    for (auto& def : this->definitions)
      def.acceptExprMutator(symResolver);
  }

  void runSemanticChecks(const char* source, size_t sourceLen)
  {
    checkSymbolResolution(source, sourceLen);

    checkRuleUsage(source, sourceLen);
  }

  void load(const char* src, size_t len, const char* name)
  {
    if (name)
      this->pathOrGrammarName = name;
    else
      this->pathOrGrammarName = "<untitled-grammar>";

    GrammarParser parser(src, len);

    while (parser.parseDef())
      ;

    this->definitions = parser.getDefinitions();

    this->diagnostics = parser.getDiagnostics();

    this->resolveSymbols();

    this->runSemanticChecks(src, len);
  }

private:
  void visitAllExprs(ExprVisitor& v) const
  {
    for (const auto& def : this->definitions)
      def.acceptExprVisitor(v);
  }

  void checkSymbolResolution(const char* source, size_t sourceLen)
  {
    SymbolResolutionChecker checker(source, sourceLen);

    visitAllExprs(checker);

    auto diags = checker.getDiagnostics();

    for (auto& diag : diags)
      this->diagnostics.emplace_back(std::move(diag));
  }

  void checkRuleUsage(const char* source, size_t sourceLen)
  {
    DiagnosticFactory diagFactory(source, sourceLen);

    for (const auto& def : this->definitions) {

      auto defName = def.getName();

      if (defName == this->getStartRuleName())
        continue;

      ReferenceChecker checker(defName.c_str());

      for (const auto& otherDef : this->definitions) {
        if (&otherDef == &def)
          continue;

        otherDef.acceptExprVisitor(checker);
      }

      auto formatter = [&defName](std::ostream& errStream) {
        errStream << "'" << defName << "' is never referenced.";
      };

      auto defNameTok = def.getIdentifierToken();

      if (!checker.foundReference()) {

        auto diag = diagFactory.make(defNameTok, formatter, Severity::Warning);

        this->diagnostics.emplace_back(std::move(diag));
      }
    }
  }

  friend Grammar;

  friend GrammarParser;

  std::string pathOrGrammarName;

  std::string startRuleName;

  std::vector<Definition> definitions;

  std::vector<Diagnostic> diagnostics;
};

Grammar::Grammar(const char* grammarSpec)
  : Grammar()
{
  load(grammarSpec, std::strlen(grammarSpec));
}

Grammar::Grammar(Grammar&& other) noexcept
  : implPtr(other.implPtr)
{
  other.implPtr = nullptr;
}

Grammar::~Grammar()
{
  delete this->implPtr;
}

GrammarImpl&
Grammar::getImpl()
{
  if (!this->implPtr)
    this->implPtr = new GrammarImpl();

  return *this->implPtr;
}

bool
Grammar::hasDiagnostics() const noexcept
{
  if (!this->implPtr)
    return false;

  return this->implPtr->diagnostics.size() > 0;
}

bool
Grammar::hasErrors() const noexcept
{
  if (!this->implPtr)
    return false;

  auto predicate = [](const Diagnostic& diag) -> bool {
    return diag.getSeverity() == Severity::Error;
  };

  auto b = this->implPtr->diagnostics.begin();
  auto e = this->implPtr->diagnostics.end();

  return std::find_if(b, e, predicate) != e;
}

void
Grammar::printDiagnostics(std::ostream& stream) const
{
  if (!this->implPtr)
    return;

  for (const auto& diag : this->implPtr->diagnostics) {

    stream << this->implPtr->pathOrGrammarName << ':';

    diag.print(stream);
  }
}

void
Grammar::load(const char* src, size_t len, const char* name)
{
  this->getImpl().load(src, len, name);
}

void
Grammar::load(const char* s)
{
  this->load(s, std::strlen(s));
}

//============
// }}} Grammar

// {{{ Parser
//===========

class Parser final : public ExprVisitor
{
public:
  Parser(const GrammarImpl& g, const char* str, size_t len)
    : grammar(g)
    , input(str)
    , inputLength(len)
  {}

  std::unique_ptr<NodeImpl> popBranch()
  {
    if (this->branchStack.size() == 0)
      return nullptr;

    auto nonTerm = std::move(this->branchStack.back());

    this->branchStack.pop_back();

    return nonTerm;
  }

  bool parseRule(size_t defIndex)
  {
    const auto* def = this->grammar.getDefinition(defIndex);
    if (!def)
      return false;

    this->beginNonTerm(def->getName());

    if (!def->acceptExprVisitor(*this)) {
      this->abortNonTerm();
      return false;
    }

    this->completeNonTerm();

    return true;
  }

  bool visit(const GroupExpr&) override { return false; }

  bool visit(const LiteralExpr& literalExpr) override
  {
    const auto& data = literalExpr.getData();

    for (size_t i = 0; i < data.size(); i++) {
      if (!this->equalAt(i, data[i]))
        return false;
    }

    this->produceLeaf(data.size());

    return true;
  }

  class ClassExprParser final : public ClassExpr::SubExprVisitor
  {
  public:
    /// @param in A single UTF-8 code point to be matched.
    ClassExprParser(std::string&& in)
      : utf8Char(std::move(in))
    {}

    bool visit(const ClassExpr::EqualitySubExpr& equalityExpr) override
    {
      return equalityExpr.getExpected().getData() == this->utf8Char;
    }

    bool visit(const ClassExpr::IntervalSubExpr& intervalExpr) override
    {
      const auto& lowerBound = intervalExpr.getLowerBound().getData();

      const auto& upperBound = intervalExpr.getUpperBound().getData();

      return (this->utf8Char >= lowerBound) && (this->utf8Char <= upperBound);
    }

  private:
    std::string utf8Char;
  };

  bool visit(const ClassExpr& classExpr) override
  {
    auto utf8Char = this->getUtf8CharAt(0);

    if (!utf8Char.size())
      return false;

    size_t utf8CharLength = utf8Char.size();

    // TODO : Check for error in input string.

    ClassExprParser classExprParser(std::move(utf8Char));

    if (!classExpr.acceptSubExprVisitor(classExprParser))
      return false;

    this->produceLeaf(utf8CharLength);

    return true;
  }

  bool visit(const DotExpr&) override
  {
    if (this->remaining() < 1)
      return false;

    auto length = this->getUtf8LengthAt(0);
    if (!length)
      return false;

    this->produceLeaf(length);

    return true;
  }

  bool visit(const ReferenceExpr& referenceExpr) override
  {
    return parseRule(referenceExpr.getRuleIndex());
  }

  bool visit(const SlashExpr& slashExpr) override
  {
    return slashExpr.acceptSequenceVisitor(0, *this);
  }

  template<typename ExprParser>
  bool matchExprRange(ExprParser exprParser, size_t min, size_t max)
  {
    size_t matchCount = 0;

    while (matchCount < max) {

      auto match = exprParser();

      static_assert(std::is_same<decltype(match), bool>::value,
                    "Expression parser must return a boolean type");

      if (!match)
        break;

      matchCount++;
    }

    return matchCount >= min;
  }

  bool visit(const SuffixExpr& suffixExpr) override
  {
    auto exprParser = [this, &suffixExpr]() -> bool {
      return suffixExpr.acceptPrimaryExprVisitor(*this);
    };

    auto exprMax = std::numeric_limits<size_t>::max();

    switch (suffixExpr.getKind()) {
      case SuffixExpr::Kind::None:
        return matchExprRange(exprParser, 1, 1);
      case SuffixExpr::Kind::Question:
        return matchExprRange(exprParser, 0, 1);
      case SuffixExpr::Kind::Star:
        return matchExprRange(exprParser, 0, exprMax);
      case SuffixExpr::Kind::Plus:
        return matchExprRange(exprParser, 1, exprMax);
    }

    return false;
  }

  bool visit(const PrefixExpr& prefixExpr) override
  {
    return prefixExpr.acceptSuffixExprVisitor(*this);
  }

  bool visit(const Sequence& sequenceExpr) override
  {
    return sequenceExpr.acceptPrefixExprVisitor(*this);
  }

private:
  void beginNonTerm(std::string ruleName)
  {
    this->pushState();

    this->branchStack.emplace_back(new NodeImpl(std::move(ruleName)));
  }

  void abortNonTerm() { this->restoreLastState(); }

  void completeNonTerm()
  {
    this->discardLastState();

    if (this->branchStack.size() <= 1)
      return;

    auto& dst = this->branchStack[this->branchStack.size() - 2];

    auto& src = this->branchStack[this->branchStack.size() - 1];

    dst->appendChild(std::move(src));

    this->branchStack.pop_back();
  }

  void pushState()
  {
    State state;
    state.inputOffset = this->inputOffset;
    state.nonTerminalCount = this->branchStack.size();
    this->stateStack.push_back(state);
  }

  void restoreLastState()
  {
    assert(this->stateStack.size() > 0);

    auto state = this->stateStack.back();

    this->stateStack.pop_back();

    this->inputOffset = state.inputOffset;

    this->branchStack.resize(state.nonTerminalCount);
  }

  void discardLastState()
  {
    assert(this->stateStack.size() > 0);

    this->stateStack.pop_back();
  }

  NodeImpl& getCurrentNonTerm()
  {
    assert(this->branchStack.size() > 0);

    return *this->branchStack.back();
  }

  void produceLeaf(size_t length)
  {
    auto& parentNonTerm = this->getCurrentNonTerm();

    const char* termPtr = this->input + this->inputOffset;

    parentNonTerm.appendLeaf(Leaf(termPtr, length));

    this->inputOffset += length;
  }

  UChar getUtf8LengthAt(size_t relOffset) const
  {
    if (relOffset >= remaining())
      return 0;
    else
      return utf8Length(this->input[this->inputOffset + relOffset]);
  }

  std::string getUtf8CharAt(size_t relOffset) const
  {
    auto len = this->getUtf8LengthAt(relOffset);

    std::string utf8Char;

    for (size_t i = 0; i < len; i++)
      utf8Char.push_back(this->input[this->inputOffset + relOffset + i]);

    return utf8Char;
  }

  bool equalAt(size_t relOffset, char c)
  {
    size_t absOffset = this->inputOffset + relOffset;

    if (absOffset < this->inputLength)
      return this->input[absOffset] == c;
    else
      return false;
  }

  size_t remaining() const noexcept
  {
    return this->inputLength - this->inputOffset;
  }

  const GrammarImpl& grammar;

  const char* input = nullptr;
  size_t inputLength = 0;
  size_t inputOffset = 0;

  struct State final
  {
    size_t inputOffset = 0;
    size_t nonTerminalCount = 0;
  };

  std::vector<State> stateStack;

  std::vector<std::unique_ptr<NodeImpl>> branchStack;
};

//===========
// }}} Parser

std::unique_ptr<Node>
Grammar::parse(const char* input, size_t length) const
{
  if (!this->implPtr)
    return nullptr;

  auto startRuleName = this->implPtr->getStartRuleName();

  auto ruleIndex = this->implPtr->findDefinitionIndex(startRuleName.c_str());

  Parser parser(*this->implPtr, input, length);

  parser.parseRule(ruleIndex);

  return parser.popBranch();
}

std::unique_ptr<Node>
Grammar::parse(const char* input) const
{
  return Grammar::parse(input, std::strlen(input));
}

} // namespace peg
