#include <pegtool.h>

#include <algorithm>
#include <iomanip>
#include <map>
#include <memory>
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

char
Token::operator[](size_t i) const noexcept
{
  if (i >= this->size)
    return 0;
  else
    return this->data[i];
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

  for (size_t i = 0; i < this->lnSize; i++)
    stream << this->lnPtr[i];

  stream << std::endl;

  stream << " " << convertNumberToSpaces(this->start.ln) << " | ";

  for (size_t i = this->lnOffset; i < this->start.idx; i++)
    stream << ((this->lnPtr[i - this->lnOffset] == '\t') ? '\t' : ' ');

  for (size_t i = 0; i < len; i++)
    stream << '~';

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

private:
  Token token;
  /// The character literal data may not be the same as what's in the token
  /// because of escape sequences. Short string optimization should prevent
  /// anything from being allocated here.
  std::string data;
};

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
  bool accept(ExprVisitor& v) const override { return v.visit(*this); }

  bool acceptMutator(const ExprMutator& m) override { return m.mutate(*this); }

  std::string toString() const
  {
    std::string result;

    for (size_t i = 2; i < this->token.getLength(); i++)
      result.push_back(this->token[i - 1]);

    return result;
  }

private:
  friend GrammarParser;

  LiteralExpr() = default;

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

  Kind getKind() const noexcept { return this->kind; }

private:
  friend GrammarParser;

  Kind kind = Kind::None;

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

class ParseTreePrinter final : public SymbolVisitor
{
public:
  ParseTreePrinter(std::ostream& s)
    : stream(s)
  {}

  void visit(const Terminal& term) override
  {
    this->indent() << '\'';

    const char* data = term.getData();

    for (size_t i = 0; i < term.getLength(); i++)
      this->stream << data[i];

    this->stream << '\'' << std::endl;
  }

  void visit(const NonTerminal& nonTerm) override
  {
    this->indent() << nonTerm.getName() << ':' << std::endl;

    this->indentLevel++;

    if (nonTerm.getChildrenCount() == 0)
      this->indent() << "(empty)" << std::endl;
    else
      nonTerm.acceptChildrenVisitor(*this);

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

class NonTerminalImpl final : public NonTerminal
{
public:
  NonTerminalImpl(std::string&& n)
    : name(std::move(n))
  {}

  void acceptChildrenVisitor(SymbolVisitor& v) const override
  {
    for (const auto& child : this->children)
      child->accept(v);
  }

  void appendChild(Symbol* s) { this->children.emplace_back(s); }

  void appendChild(std::unique_ptr<NonTerminalImpl>&& child)
  {
    this->children.emplace_back(std::unique_ptr<Symbol>(child.release()));
  }

  const char* getName() const noexcept override { return this->name.c_str(); }

  bool hasName(const char* n) const noexcept override
  {
    return this->name == n;
  }

  size_t getChildrenCount() const noexcept override
  {
    return this->children.size();
  }

private:
  std::string name;
  std::vector<std::unique_ptr<Symbol>> children;
};

} // namespace

void
NonTerminal::print(std::ostream& stream) const
{
  ParseTreePrinter printer(stream);

  printer.visit(*this);
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

    while (!this->cursor.outOfBounds(len)) {

      auto last = this->cursor.peek(len);

      if (last != first) {
        len++;
        continue;
      }

      auto literalExpr = std::unique_ptr<LiteralExpr>(new LiteralExpr());

      produce(literalExpr->token, len + 1);

      return UniqueExprPtr(literalExpr.release());
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

    return nullptr;
  }

  bool parseSuffixExpr(SuffixExpr& suffixExpr, bool& errFlag)
  {
    suffixExpr.primaryExpr = parsePrimaryExpr(errFlag);

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

  bool parseExactly(Token& token, const char* expected)
  {
    size_t len = 0;

    for (;;) {

      auto expectedChar = expected[len];

      if (!expectedChar)
        return produce(token, len);

      if (this->cursor.peek(len) != expectedChar)
        break;

      len++;
    }

    return false;
  }

  bool parseCharLiteral(CharLiteral& literal)
  {
    (void)literal;
    return false;
#if 0
    if (!this->cursor.atEnd() && this->cursor.peek(0) != '\\') {
      literal.push_pack(this->cursor.peek(0));
      return true;
    }

    if (this->cursor.peek(0) != '\\')
      return false;

    auto escapedChar = this->cursor.peek(1);

    switch (escapedChar) {
      case 'n':
      case 'r':
      case 't':
      case '\'':
      case '"':
      case '[':
      case ']':
        break;
    }
#endif
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

  bool produce(Token& token, size_t len)
  {
    token = Token(this->cursor.getOffsetPtr(), len, this->getPosition());

    this->cursor.next(len);

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
  auto& implRef = this->getImpl();

  if (name)
    implRef.pathOrGrammarName = name;
  else
    implRef.pathOrGrammarName = "<untitled-grammar>";

  GrammarParser parser(src, len);

  while (parser.parseDef())
    ;

  implRef.definitions = parser.getDefinitions();

  implRef.diagnostics = parser.getDiagnostics();

  implRef.resolveSymbols();

  implRef.runSemanticChecks(src, len);
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

  std::unique_ptr<NonTerminalImpl> popNonTerminal()
  {
    if (this->nonTerminalStack.size() == 0)
      return nullptr;

    auto nonTerm = std::move(this->nonTerminalStack.back());

    this->nonTerminalStack.pop_back();

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
    // TODO : Do not dynamically allocate this.
    auto data = literalExpr.toString();

    for (size_t i = 0; i < data.size(); i++) {
      if (!this->equalAt(i, data[i]))
        return false;
    }

    this->produceTerm(data.size());

    return true;
  }

  bool visit(const ClassExpr&) override { return false; }

  bool visit(const DotExpr&) override
  {
    if (this->remaining() < 1)
      return false;

    this->produceTerm(1);

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

  bool visit(const SuffixExpr& suffixExpr) override
  {
    return suffixExpr.acceptPrimaryExprVisitor(*this);
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

    this->nonTerminalStack.emplace_back(
      new NonTerminalImpl(std::move(ruleName)));
  }

  void abortNonTerm() { this->restoreLastState(); }

  void completeNonTerm()
  {
    this->discardLastState();

    if (this->nonTerminalStack.size() <= 1)
      return;

    auto& dst = this->nonTerminalStack[this->nonTerminalStack.size() - 2];

    auto& src = this->nonTerminalStack[this->nonTerminalStack.size() - 1];

    dst->appendChild(std::move(src));

    this->nonTerminalStack.pop_back();
  }

  void pushState()
  {
    State state;
    state.inputOffset = this->inputOffset;
    state.nonTerminalCount = this->nonTerminalStack.size();
    this->stateStack.push_back(state);
  }

  void restoreLastState()
  {
    assert(this->stateStack.size() > 0);

    auto state = this->stateStack.back();

    this->stateStack.pop_back();

    this->inputOffset = state.inputOffset;

    this->nonTerminalStack.resize(state.nonTerminalCount);
  }

  void discardLastState()
  {
    assert(this->stateStack.size() > 0);

    this->stateStack.pop_back();
  }

  NonTerminalImpl& getCurrentNonTerm()
  {
    assert(this->nonTerminalStack.size() > 0);

    return *this->nonTerminalStack.back();
  }

  void produceTerm(size_t length)
  {
    Terminal term(this->input + this->inputOffset, length);

    auto& parentNonTerm = this->getCurrentNonTerm();

    const char* termPtr = this->input + this->inputOffset;

    parentNonTerm.appendChild(new Terminal(termPtr, length));

    this->inputOffset += length;
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

  std::vector<std::unique_ptr<NonTerminalImpl>> nonTerminalStack;
};

//===========
// }}} Parser

std::unique_ptr<NonTerminal>
Grammar::parse(const char* input, size_t length) const
{
  if (!this->implPtr)
    return nullptr;

  auto startRuleName = this->implPtr->getStartRuleName();

  auto ruleIndex = this->implPtr->findDefinitionIndex(startRuleName.c_str());

  Parser parser(*this->implPtr, input, length);

  parser.parseRule(ruleIndex);

  return parser.popNonTerminal();
}

std::unique_ptr<NonTerminal>
Grammar::parse(const char* input) const
{
  return Grammar::parse(input, std::strlen(input));
}

} // namespace peg
