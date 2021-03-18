#ifndef PEGTOOL_H
#define PEGTOOL_H

#include <iosfwd>
#include <memory>

#include <stddef.h>

namespace peg {

class GrammarImpl;
class NonTerminal;
class Symbol;
class SymbolVisitor;
class Terminal;

/// @brief This class contains all the definitions of a grammar.
///
/// @detail When using this library, this class is the first to be logically
/// declared. When an instance of a grammar class is made, it is initially
/// empty. The caller is required to parse the grammar file to initialize it
/// with the appropriate grammar definitions.
class Grammar final
{
public:
  Grammar() = default;

  Grammar(Grammar&&) noexcept;

  ~Grammar();

  /// Indicates if there are any diagnostics in the grammar. It does not mean
  /// the grammar is malformed if this function returns true, since diagnostics
  /// may only be warnings.
  bool hasDiagnostics() const noexcept;

  /// Indicates if the grammar has at least one diagnostic that is considered an
  /// error. If this function returns true, the grammar should no longer be
  /// processed.
  bool hasErrors() const noexcept;

  /// Prints the diagnostics in the grammar to a stream.
  void printDiagnostics(std::ostream&) const;

  /// @brief Parses a string containing a grammar specification.
  ///
  /// @detail Diagnostics may be generated after calling this function. It is a
  /// good idea to call @ref Grammar::hasErrors to figure out if the grammar is
  /// malformed or not. Calling @ref Grammar::hasDiagnostics can also indicate
  /// if there are any warnings (or errors) instead of just checking for errors.
  ///
  /// @param src
  /// A pointer to the string containing the grammar specification.
  ///
  /// @param len
  /// The number of bytes in the grammar specification, not including any null
  /// terminating character.
  ///
  /// @param name
  /// The name of the grammar. This is used when generating diagnostics and can
  /// either be the path of the grammar file or the name of the language that it
  /// is for.
  void load(const char* src, size_t len, const char* name = nullptr);

  /// @brief Does the same as the other parse function, except that the length
  /// of the string is determined by finding the null terminator and the name of
  /// the grammar is not specified.
  void load(const char* src);

  std::unique_ptr<NonTerminal> parse(const char* input, size_t length) const;

private:
  GrammarImpl* implPtr = nullptr;

  GrammarImpl& getImpl();
};

/// This can be used to access the derived symbols in the syntax tree.
class SymbolVisitor
{
public:
  virtual ~SymbolVisitor() = default;

  virtual void visit(const Terminal&) = 0;

  virtual void visit(const NonTerminal&) = 0;
};

class Symbol
{
public:
  virtual ~Symbol() = default;

  virtual void accept(SymbolVisitor&) const = 0;
};

/// Represents terminal symbols, which are symbols that appear on the right side
/// of the rule and do not appear on the left side. They consist of only
/// character content and have no child symbols.
class Terminal final : public Symbol
{
public:
  Terminal(const char* d, size_t l)
    : data(d)
    , length(l)
  {}

  void accept(SymbolVisitor& v) const override { v.visit(*this); }

  const char* getData() const noexcept { return this->data; }

  size_t getLength() const noexcept { return this->length; }

private:
  /// A pointer to the beginning of the terminal data.
  const char* data = nullptr;
  /// The number of bytes captured by the terminal.
  size_t length = 0;
};

class NonTerminal : public Symbol
{
public:
  void accept(SymbolVisitor& v) const override { v.visit(*this); }

  virtual void acceptChildrenVisitor(SymbolVisitor& v) const = 0;

  virtual size_t getChildrenCount() const noexcept = 0;

  virtual const char* getName() const noexcept = 0;

  virtual bool hasName(const char*) const noexcept = 0;

  void print(std::ostream&) const;
};

} // namespace peg

#endif // PEGTOOL_H
