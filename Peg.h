#ifndef PARSER_EXPR_GRAMMAR_H
#define PARSER_EXPR_GRAMMAR_H

#include <memory>
#include <string>
#include <vector>
#include <iosfwd>

#include <stddef.h>

namespace peg {

class GrammarParser;

struct Token final
{
  const char *data = "";
  size_t size = 0;
  size_t ln = 1;
  size_t col = 1;
};

// {{{ Expressions
//================

class PrimaryExprVisitor;

class PrimaryExpr
{
public:
  virtual ~PrimaryExpr() = default;

  virtual void accept(PrimaryExprVisitor &) const = 0;
};

using UniquePrimaryExpr = std::unique_ptr<PrimaryExpr>;

class GroupExpr;
class LiteralExpr;
class ClassExpr;
class DotExpr;

class PrimaryExprVisitor
{
public:
  virtual ~PrimaryExprVisitor() = default;
  virtual void visit(const GroupExpr &) = 0;
  virtual void visit(const LiteralExpr &) = 0;
  virtual void visit(const ClassExpr &) = 0;
  virtual void visit(const DotExpr &) = 0;
};

class SuffixExpr final
{
public:
  enum class Kind {
    None,
    Question,
    Star,
    Plus
  };

private:
  Kind kind = Kind::None;
  UniquePrimaryExpr primaryExpr;
};

class PrefixExpr final
{
public:
  enum class Kind {
    None,
    And,
    Not
  };
private:
  SuffixExpr suffixExpr;
};

class Sequence final
{
public:
private:
  std::vector<PrefixExpr> prefixExprs;
};

class Expr final
{
public:
  using SlashSeqPair = std::pair<Token, Sequence>;

private:
  Sequence firstSequence;

  std::vector<SlashSeqPair> otherSequenceExprs;
};

//================
// }}} Expressions

class Definition final
{
private:
  friend GrammarParser;

  Token identifier;

  Expr expr;
};

enum class Severity
{
  Warning,
  Error
};

class Diagnostic final
{
public:
  void print(std::ostream &) const;

private:
  friend GrammarParser;

  size_t ln = 0;

  size_t col = 0;

  std::string msg;

  Severity severity = Severity::Error;
};

class DiagnosticIterable final
{
public:
  using Iterator = std::vector<Diagnostic>::const_iterator;

  DiagnosticIterable() = delete;

  Iterator begin() const { return this->beginIt; }

  Iterator end() const { return this->endIt; }

private:
  friend class Grammar;

  DiagnosticIterable(Iterator a, Iterator b)
    : beginIt(a)
    , endIt(b)
  {}

  Iterator beginIt;

  Iterator endIt;
};

class Grammar final
{
public:
  bool hasErrors() const noexcept { return this->diagList.size() > 0; }

  DiagnosticIterable getDiagnostics() const
  {
    return DiagnosticIterable(diagList.begin(), diagList.end());
  }
private:
  friend class GrammarParser;

  std::vector<Definition> definitionMap;

  std::vector<Diagnostic> diagList;
};

Grammar parseGrammar(const char *source, size_t len);

} // namespace peg

#endif // PARSER_EXPR_GRAMMAR_H
