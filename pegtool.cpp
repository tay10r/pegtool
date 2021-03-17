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
  CharCursor(const char* s, size_t len)
    : source(s)
    , charMax(len)
  {}

  bool atEnd() const noexcept;

  const char* getOffsetPtr() const noexcept
  {
    return this->source + this->charIdx;
  }

  const char* getLinePtr(size_t idx) const noexcept
  {
    for (size_t i = idx; (i > 0) && (i <= this->charIdx); i--) {

      char c = this->source[i - 1];

      if (c == '\n')
        return this->source + i;
    }

    return this->source;
  }

  size_t getStartingIndexOfLine(size_t idx) const noexcept
  {
    for (size_t i = idx; (i > 0) && (i <= this->charIdx); i--) {

      char c = this->source[i - 1];

      if (c == '\n')
        return idx - i;
    }

    return 0;
  }

  size_t getLineSize(size_t lineOffset) const noexcept
  {
    for (size_t i = lineOffset; i < this->charMax; i++) {

      char c = this->source[i];

      if (c == '\n')
        return i - lineOffset;
    }

    return this->charMax - lineOffset;
  }

  size_t getLine() const noexcept { return this->ln; }

  size_t getColumn() const noexcept { return this->col; }

  size_t getOffset() const noexcept { return this->charIdx; }

  bool outOfBounds(size_t relOffset) const noexcept;

  char peek(size_t offset) const noexcept;

  void next(size_t count) noexcept;

  void skipUnused();

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
  Warning,
  Error
};

/// Contains information regarding an error or a warning issued by the grammar
/// parser. It contains extra data for showing the line and range of characters
/// that caused the diagnostic.
class Diagnostic final
{
public:
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

// {{{ Expression Tree
//====================

namespace {

class PrimaryExprVisitor;

class PrimaryExpr
{
public:
  virtual ~PrimaryExpr() = default;

  virtual void accept(PrimaryExprVisitor&) const = 0;
};

using UniquePrimaryExprPtr = std::unique_ptr<PrimaryExpr>;

class GroupExpr;
class LiteralExpr;
class ClassExpr;
class DotExpr;

class PrimaryExprVisitor
{
public:
  virtual ~PrimaryExprVisitor() = default;
  virtual void visit(const GroupExpr&) = 0;
  virtual void visit(const LiteralExpr&) = 0;
  virtual void visit(const ClassExpr&) = 0;
  virtual void visit(const DotExpr&) = 0;
};

class LiteralExpr final : public PrimaryExpr
{
public:
  void accept(PrimaryExprVisitor& v) const override;

  std::string toString() const;

private:
  friend GrammarParser;

  LiteralExpr() = default;

  Token token;
};

class SuffixExpr final
{
public:
  enum class Kind
  {
    None,
    Question,
    Star,
    Plus
  };

  void visitPrimaryExpr(PrimaryExprVisitor& v) const;

private:
  friend GrammarParser;

  // Kind kind = Kind::None;

  UniquePrimaryExprPtr primaryExpr;
};

class PrefixExpr final
{
public:
  enum class Kind
  {
    None,
    And,
    Not
  };

  template<typename Visitor>
  void visitSuffixExpr(Visitor v) const
  {
    v(this->suffixExpr);
  }

private:
  friend GrammarParser;

  SuffixExpr suffixExpr;
};

class Sequence final
{
public:
  size_t getPrefixExprCount() const noexcept;

  template<typename Visitor>
  void visitPrefixExprs(Visitor v) const
  {
    for (const auto& prefixExpr : this->prefixExprs)
      v(prefixExpr);
  }

private:
  friend GrammarParser;

  std::vector<PrefixExpr> prefixExprs;
};

class Expr final
{
public:
  using SlashSeqPair = std::pair<Token, Sequence>;

  size_t getSequenceCount() const noexcept;

  template<typename Visitor>
  void visitSequences(Visitor v) const
  {
    v(firstSequence);

    for (const auto& seq : this->otherSequenceExprs)
      v(seq.second);
  }

private:
  friend GrammarParser;

  Sequence firstSequence;

  std::vector<SlashSeqPair> otherSequenceExprs;
};

void
LiteralExpr::accept(PrimaryExprVisitor& v) const
{
  v.visit(*this);
}

std::string
LiteralExpr::toString() const
{
  std::string result;

  for (size_t i = 2; i < this->token.getLength(); i++) {
    result.push_back(this->token[i - 1]);
  }

  return result;
}

size_t
Expr::getSequenceCount() const noexcept
{
  return 1 + otherSequenceExprs.size();
}

void
SuffixExpr::visitPrimaryExpr(PrimaryExprVisitor& v) const
{
  if (this->primaryExpr)
    this->primaryExpr->accept(v);
}

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

  bool hasName(const char* name) const { return identifier == name; }

  template<typename Visitor>
  void visitExpr(Visitor v) const
  {
    v(this->expr);
  }

private:
  friend GrammarParser;

  Definition() = default;

  Token identifier;

  Token arrow;

  Expr expr;
};

} // namespace

//===============
// }}} Definition

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

    if (!parseExpr(def.expr)) {
      formatErr(def.arrow, [](std::ostream& errStream) {
        errStream << "Expected an expression after this.";
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

  template<typename Formatter>
  bool formatErr(const Token& tok, Formatter formatter)
  {
    std::ostringstream msgStream;

    formatter(static_cast<std::ostream&>(msgStream));

    auto pos = tok.getPosition();

    Diag diag;
    diag.severity = Severity::Error;
    diag.start = pos;
    diag.msg = msgStream.str();
    diag.lnPtr = this->cursor.getLinePtr(pos.idx);
    diag.lnOffset = this->cursor.getStartingIndexOfLine(pos.idx);
    diag.lnSize = this->cursor.getLineSize(diag.lnOffset);
    diag.len = tok.getLength();

    this->diagnostics.emplace_back(std::move(diag));

    return false;
  }

  UniquePrimaryExprPtr parseLiteralExpr()
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

      return UniquePrimaryExprPtr(literalExpr.release());
    }

    // TODO : Unterminated string error

    return nullptr;
  }

  UniquePrimaryExprPtr parsePrimaryExpr()
  {
    auto literalExpr = parseLiteralExpr();
    if (literalExpr)
      return literalExpr;

    return nullptr;
  }

  bool parseSuffixExpr(SuffixExpr& suffixExpr)
  {
    suffixExpr.primaryExpr = parsePrimaryExpr();

    return !!suffixExpr.primaryExpr;
  }

  bool parsePrefixExpr(PrefixExpr& prefixExpr)
  {
    return parseSuffixExpr(prefixExpr.suffixExpr);
  }

  bool parseSequence(Sequence& sequence)
  {
    while (!this->cursor.atEnd()) {

      PrefixExpr prefixExpr;

      if (parsePrefixExpr(prefixExpr))
        sequence.prefixExprs.emplace_back(std::move(prefixExpr));
      else
        break;
    }

    return sequence.prefixExprs.size() > 0;
  }

  bool parseExpr(Expr& expr)
  {
    if (!parseSequence(expr.firstSequence))
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

  const Definition* findDef(const char* name) const
  {
    for (const auto& def : this->definitions) {
      if (def.hasName(name))
        return &def;
    }

    return nullptr;
  }

private:
  friend Grammar;

  friend GrammarParser;

  std::string pathOrGrammarName;

  std::string startRuleName;

  std::vector<Definition> definitions;

  std::vector<Diagnostic> diagnostics;
};

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
Grammar::parse(const char* src, size_t len, const char* name)
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
}

void
Grammar::parse(const char* s)
{
  this->parse(s, std::strlen(s));
}

//============
// }}} Grammar

// {{{ Bytecode
//=============

namespace {

class RetInst;
class ExpectInst;
class ProduceTermInst;

class InstVisitor
{
public:
  virtual ~InstVisitor() = default;

  virtual void visit(const RetInst&) = 0;

  virtual void visit(const ExpectInst&) = 0;

  virtual void visit(const ProduceTermInst&) = 0;
};

class Inst
{
public:
  virtual ~Inst() = default;

  virtual void accept(InstVisitor&) const = 0;
};

using UniqueInstPtr = std::unique_ptr<Inst>;

class ExpectInst final : public Inst
{
public:
  ExpectInst(std::string e) noexcept
    : expected(std::move(e))
  {}

  void accept(InstVisitor& v) const override { v.visit(*this); }

  const std::string& getExpected() const noexcept { return this->expected; }

private:
  std::string expected;
};

class RetInst final : public Inst
{
public:
  void accept(InstVisitor& v) const override { v.visit(*this); }
};

class ProduceTermInst final : public Inst
{
public:
  void accept(InstVisitor& v) const override { v.visit(*this); }
};

} // namespace

//=============
// }}} Bytecode

// {{{ Inst Printer
//===================

namespace {

class InstPrinter final : public InstVisitor
{
public:
  InstPrinter(std::ostream& s, size_t maxInstOffset)
    : stream(s)
    , maxOffsetChars(getRequiredDecChars(maxInstOffset))
  {}

  void visit(const ExpectInst& expectInst) override
  {
    this->printPrefix("expect");
    this->stream << " '" << expectInst.getExpected() << "'";
    this->stream << std::endl;
  }

  void visit(const RetInst&) override { this->printPrefix("ret") << std::endl; }

  void visit(const ProduceTermInst&) override
  {
    this->printPrefix("produce_term") << std::endl;
  }

  void printLabel(const char* label)
  {
    this->stream << label << ':' << std::endl;
  }

  void nextOffset() noexcept { this->instOffset++; }

private:
  std::ostream& printPrefix(const char* inst)
  {
    this->stream << "  " << std::setfill('0') << std::setw(maxOffsetChars)
                 << std::right << std::hex << this->instOffset;

    this->stream << "  " << inst;

    return this->stream;
  }

  static size_t getRequiredDecChars(size_t n) noexcept
  {
    if (n < 10)
      return 1;
    else if (n < 100)
      return 2;
    else if (n < 1000)
      return 3;
    else if (n < 10000)
      return 4;
    else if (n < 100000)
      return 5;

    return 8;
  }

  std::ostream& stream;

  size_t instOffset = 0;

  size_t maxOffsetChars = 4;
};

} // namespace

//===================
// }}} Inst Printer

// {{{ Module
//===========

namespace {

class ModuleBuilder;

} // namespace

class ModuleImpl final
{
  friend Module;

  friend ModuleBuilder;

  /// Used to associate bytecode addresses to the rules that they originated
  /// from.
  std::map<size_t, std::string> ruleMap;

  std::vector<UniqueInstPtr> code;

public:
  const char* findLabelAt(size_t offset) const
  {
    auto it = this->ruleMap.find(offset);

    if (it != this->ruleMap.end())
      return it->second.c_str();

    return nullptr;
  }
};

Module::Module(Module&& other) noexcept
  : implPtr(other.implPtr)
{
  other.implPtr = nullptr;
}

Module::~Module()
{
  delete this->implPtr;
}

void
Module::print(std::ostream& stream) const
{
  if (!this->implPtr)
    return;

  InstPrinter instPrinter(stream, this->implPtr->code.size());

  for (size_t i = 0; i < this->implPtr->code.size(); i++) {

    const auto& inst = this->implPtr->code[i];

    const char* label = this->implPtr->findLabelAt(i);
    if (label)
      instPrinter.printLabel(label);

    inst->accept(instPrinter);

    instPrinter.nextOffset();
  }
}

ModuleImpl&
Module::getImpl()
{
  if (!this->implPtr)
    this->implPtr = new ModuleImpl();

  return *this->implPtr;
}

//===========
// }}} Module

// {{{ Module Builder
//==================

namespace {

class ModuleBuilder final : public PrimaryExprVisitor
{
public:
  ModuleBuilder(const GrammarImpl& g, ModuleImpl& m)
    : grammar(g)
    , module(m)
  {}

  bool buildRule(const std::string& name)
  {
    const auto* def = this->grammar.findDef(name.c_str());

    if (def)
      return this->buildRule(*def);
    else
      return false;
  }

private:
  bool buildRule(const Definition& def)
  {
    this->module.ruleMap.emplace(this->module.code.size(), def.getName());

    auto exprVisitor = [this](const Expr& expr) { this->buildExpr(expr); };

    def.visitExpr(exprVisitor);

    this->insertInst(new ProduceTermInst());

    this->insertInst(new RetInst());

    return true;
  }

  void buildExpr(const Expr& expr)
  {
    if (expr.getSequenceCount() == 1) {
      auto seqVisitor = [this](const Sequence& seq) {
        this->buildSequence(seq);
      };
      expr.visitSequences(seqVisitor);
    } else {
      // TODO
    }
  }

  void buildSequence(const Sequence& seq)
  {
    auto seqVisitor = [this](const PrefixExpr& prefixExpr) {
      this->buildPrefixExpr(prefixExpr);
    };

    seq.visitPrefixExprs(seqVisitor);
  }

  void buildPrefixExpr(const PrefixExpr& prefixExpr)
  {
    auto suffixExprVisitor = [this](const SuffixExpr& suffixExpr) {
      this->buildSuffixExpr(suffixExpr);
    };

    prefixExpr.visitSuffixExpr(suffixExprVisitor);
  }

  void buildSuffixExpr(const SuffixExpr& suffixExpr)
  {
    suffixExpr.visitPrimaryExpr(*this);
  }

  void visit(const GroupExpr&) override {}

  void visit(const LiteralExpr& literalExpr) override
  {
    auto str = literalExpr.toString();

    this->insertInst(new ExpectInst(std::move(str)));
  }

  void visit(const ClassExpr&) override {}

  void visit(const DotExpr&) override {}

  void insertInst(Inst* inst) { this->module.code.emplace_back(inst); }

  const GrammarImpl& grammar;

  ModuleImpl& module;
};

} // namespace

//==================
// }}} Module Builder

bool
Module::load(const Grammar& grammar)
{
  if (!grammar.implPtr)
    return false;

  ModuleBuilder moduleBuilder(*grammar.implPtr, getImpl());

  auto startRule = grammar.implPtr->getStartRuleName();

  moduleBuilder.buildRule(startRule);

  return true;
}

// {{{ Parse Tree Printer
//=======================

namespace {

class ParseTreePrinter final : public SymbolVisitor
{
public:
  ParseTreePrinter(std::ostream& s)
    : stream(s)
  {}

  void visit(const ParseTree& parseTree, const Terminal& term) override
  {
    this->indent() << '\'';

    const char* data = parseTree.getCharData(term);

    for (size_t i = 0; i < term.getLength(); i++)
      this->stream << data[i];

    this->stream << '\'' << std::endl;
  }

  void visit(const ParseTree& parseTree, const NonTerminal& nonTerm) override
  {
    this->indent() << parseTree.getName(nonTerm) << ':' << std::endl;

    this->indentLevel++;

    if (nonTerm.getChildrenCount() == 0)
      this->indent() << "(empty)" << std::endl;
    else
      parseTree.visitChildren(nonTerm, *this);

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

// {{{ Parse Tree
//===============

namespace {

const NonTerminal&
nullNonTerm() noexcept
{
  static NonTerminal nonTerm;
  return nonTerm;
}

} // namespace

class ParseTreeImpl final
{
public:
  struct SymbolIndex final
  {
    /// Indicates what array the index belongs to.
    bool isTerminal : 1;

    /// This is either the index within the terminal array or the index within
    /// the non-terminal array.
    size_t index : (sizeof(size_t) * 8) - 1;
  };

  /// @param addr The address of the function for the non-terminal within the
  /// bytecode module.
  ///
  /// @return The index of the terminal within the symbol table.
  SymbolIndex appendNonTerm(size_t addr)
  {
    NonTerminal nonTerm;

    nonTerm.address = addr;

    this->nonTerminals.emplace_back(nonTerm);

    SymbolIndex index{ false, this->nonTerminals.size() - 1 };

    this->symbolIndices.emplace_back(index);

    return index;
  }

  void appendChild(NonTerminal& nonTerm) { nonTerm.childrenCount++; }

  /// @return The offset of the terminal within the symbol index table.
  size_t appendTerm(size_t start, size_t length)
  {
    Terminal term;
    term.offset = start;
    term.length = length;

    this->terminals.emplace_back(term);

    SymbolIndex index{ true, this->terminals.size() - 1 };

    this->symbolIndices.emplace_back(index);

    return this->symbolIndices.size() - 1;
  }

  void copyRuleMap(const std::map<size_t, std::string>& ruleMap_)
  {
    this->ruleMap = ruleMap_;
  }

  NonTerminal& getNonTerminal(const SymbolIndex& sym)
  {
    return this->nonTerminals[sym.index];
  }

  void setChildrenOffset(NonTerminal& nonTerm, size_t offset)
  {
    nonTerm.childrenOffset = offset;
  }

  void setSource(const char* s, size_t len)
  {
    this->source = s;
    this->sourceLen = len;
  }

private:
  friend ParseTree;

  const char* source = "";

  size_t sourceLen = 0;

  std::map<size_t, std::string> ruleMap;

  std::vector<Terminal> terminals;

  std::vector<NonTerminal> nonTerminals;

  std::vector<SymbolIndex> symbolIndices;
};

ParseTree::ParseTree(ParseTree&& other) noexcept
  : implPtr(other.implPtr)
{
  other.implPtr = nullptr;
}

ParseTree::~ParseTree()
{
  delete this->implPtr;
}

ParseTreeImpl&
ParseTree::getImpl()
{
  if (!this->implPtr)
    this->implPtr = new ParseTreeImpl();

  return *this->implPtr;
}

const char*
ParseTree::getCharData(const Terminal& term) const
{
  if (!this->implPtr)
    return "";

  if (term.offset >= this->implPtr->sourceLen)
    // This may be a futile check, since the terminal probably has a length
    // that is greater than zero if something like this happens. In any case,
    // leaving this check probably won't hurt.
    return "";

  return this->implPtr->source + term.offset;
}

const char*
ParseTree::getName(const NonTerminal& nonTerm) const
{
  if (!this->implPtr)
    return "";

  auto it = this->implPtr->ruleMap.find(nonTerm.address);

  if (it == this->implPtr->ruleMap.end())
    return "";
  else
    return it->second.c_str();
}

const NonTerminal&
ParseTree::getRoot() const noexcept
{
  if (!this->implPtr || this->implPtr->nonTerminals.empty())
    return nullNonTerm();

  return this->implPtr->nonTerminals[0];
}

void
ParseTree::print(const NonTerminal& nonTerminal, std::ostream& stream) const
{
  if (!this->implPtr)
    return;

  ParseTreePrinter printer(stream);

  printer.visit(*this, nonTerminal);
}

void
ParseTree::visitChildren(const NonTerminal& nonTerm,
                         SymbolVisitor& visitor) const
{
  if (!this->implPtr)
    return;

  for (size_t i = 0; i < nonTerm.childrenCount; i++) {

    size_t childIndex = nonTerm.childrenOffset + i;

    assert(childIndex < this->implPtr->symbolIndices.size());

    auto symIndex = this->implPtr->symbolIndices[childIndex];

    if (symIndex.isTerminal) {
      assert(symIndex.index < this->implPtr->terminals.size());
      visitor.visit(*this, this->implPtr->terminals[symIndex.index]);
    } else {
      assert(symIndex.index < this->implPtr->nonTerminals.size());
      visitor.visit(*this, this->implPtr->nonTerminals[symIndex.index]);
    }
  }
}

//===============
// }}} Parse Tree

// {{{ VM
//=======

namespace {

struct StackFrame final
{
  size_t returnAddr = 0;
  /// The index of the VM within the input being parsed at the time that the
  /// function call was made. This value gets discarded if the function call
  /// does not get aborted.
  size_t sourceIdx = 0;
};

class VM final : public InstVisitor
{
public:
  VM(const UniqueInstPtr* c,
     size_t cs,
     const char* src,
     size_t len,
     ParseTreeImpl& pt)
    : code(c)
    , codeSize(cs)
    , source(src)
    , sourceLen(len)
    , parseTree(pt)
  {}

  void exec()
  {
    this->instPtr = 0;

    this->sourceIdx = 0;

    this->callStack.clear();

    this->nonTerminalStack.clear();

    this->call(0);

    while ((this->callStack.size() > 0) && (this->instPtr < this->codeSize))
      this->code[this->instPtr++]->accept(*this);
  }

private:
  void visit(const RetInst&) override
  {
    if (this->callStack.empty()) {
      this->abortVM();
      return;
    }

    this->instPtr = this->callStack.back().returnAddr;

    this->callStack.pop_back();
  }

  void visit(const ExpectInst& expectInst) override
  {
    const auto& expected = expectInst.getExpected();

    if (this->getRemaining() < expected.size()) {
      abortFunctionCall();
      return;
    }

    for (size_t i = 0; i < expected.size(); i++) {
      if (this->peek(i) != expected[i]) {
        abortFunctionCall();
        return;
      }
    }

    this->advance(expected.size());
  }

  void visit(const ProduceTermInst&) override
  {
    if (this->callStack.empty())
      return;

    const auto& currentFrame = this->callStack.back();

    auto length = this->sourceIdx - currentFrame.sourceIdx;

    produceTerm(currentFrame.sourceIdx, length);
  }

  void produceTerm(size_t start, size_t length)
  {
    if (this->nonTerminalStack.empty())
      return;

    size_t termOffset = this->parseTree.appendTerm(start, length);

    auto& parentNonTerminal = getCurrentNonTerminal();

    if (parentNonTerminal.getChildrenCount() == 0)
      this->parseTree.setChildrenOffset(parentNonTerminal, termOffset);

    this->parseTree.appendChild(parentNonTerminal);
  }

  NonTerminal& getCurrentNonTerminal()
  {
    return this->parseTree.getNonTerminal(this->nonTerminalStack.back());
  }

  void abortFunctionCall()
  {
    if (this->callStack.empty())
      return;

    auto frame = this->callStack.back();
    this->sourceIdx = frame.sourceIdx;
    this->instPtr = frame.returnAddr;

    this->callStack.pop_back();
  }

  void call(size_t addr)
  {
    StackFrame stackFrame;
    stackFrame.returnAddr = this->instPtr;
    stackFrame.sourceIdx = this->sourceIdx;

    this->callStack.emplace_back(stackFrame);

    this->instPtr = addr;

    this->nonTerminalStack.emplace_back(this->parseTree.appendNonTerm(addr));
  }

  void abortVM() { this->instPtr = std::numeric_limits<size_t>::max(); }

  char peek(size_t relativeOffset) const noexcept
  {
    auto absIndex = this->sourceIdx + relativeOffset;

    return (absIndex < this->sourceLen) ? this->source[absIndex] : 0;
  }

  size_t getRemaining() const noexcept
  {
    if (this->sourceIdx >= this->sourceLen)
      return 0;
    else
      return this->sourceLen - this->sourceIdx;
  }

  void advance(size_t count) noexcept
  {
    this->sourceIdx += count;

    this->sourceIdx = std::min(this->sourceIdx, this->sourceLen);
  }

  std::vector<StackFrame> callStack;

  std::vector<ParseTreeImpl::SymbolIndex> nonTerminalStack;

  const UniqueInstPtr* code = nullptr;

  size_t codeSize = 0;

  const char* source = "";

  size_t sourceLen = 0;

  size_t sourceIdx = 0;

  size_t instPtr = 0;

  ParseTreeImpl& parseTree;
};

} // namespace

//=======
// }}} VM

auto
Module::exec(const char* input, size_t length) -> ParseTree
{
  ParseTree parseTree;

  if (!this->implPtr)
    return parseTree;

  parseTree.getImpl().setSource(input, length);

  parseTree.getImpl().copyRuleMap(this->implPtr->ruleMap);

  VM vm(this->implPtr->code.data(),
        this->implPtr->code.size(),
        input,
        length,
        parseTree.getImpl());

  vm.exec();

  return parseTree;
}

} // namespace peg
