#ifndef PEGTOOL_H
#define PEGTOOL_H

#include <stddef.h>

#include <iosfwd>
#include <memory>

namespace peg {

class Node;
class GrammarImpl;
class Leaf;

/// @brief This class contains all the definitions of a grammar.
///
/// @detail When using this library, this class is the first to be logically
/// declared. When an instance of a grammar class is made, it is initially
/// empty. The caller is required to parse the grammar file to initialize it
/// with the appropriate grammar definitions.
class Grammar final
{
public:
  /// Constructs the grammar with a string literal that contains the PEG source
  /// code.
  ///
  /// @param grammarSpec The string containing the definitions and parsing
  /// expressions. This should be a null-terminated string.
  Grammar(const char* grammarSpec);

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

  std::unique_ptr<Node> parse(const char* input, size_t length) const;

  std::unique_ptr<Node> parse(const char* input) const;

private:
  GrammarImpl* implPtr = nullptr;

  GrammarImpl& getImpl();
};

/// Represents pure text. Often the result of parsing expressions such as string
/// literals or character classes.
class Leaf final
{
public:
  // Not meant to be used publicly.
  Leaf(const char* d, size_t l)
    : data(d)
    , length(l)
  {}

  const char* getData() const noexcept { return this->data; }

  size_t getLength() const noexcept { return this->length; }

private:
  /// A pointer to the beginning of the character data.
  const char* data = nullptr;
  /// The number of bytes captured by the parsing expression.
  size_t length = 0;
};

/// This is a special type of leaf node that indicates an error and contains
/// a message that describes the error.
class Error
{
public:
  virtual ~Error() = default;

  /// @note The string returned here is null-terminated.
  virtual const char* getMessage() const noexcept = 0;

  virtual size_t getMessageLength() const noexcept = 0;

  /// @note The string returned here is not null-terminated. It is part of the
  /// same string data passed to the parser.
  virtual const char* getData() const noexcept = 0;

  virtual size_t getLength() const noexcept = 0;
};

class Node
{
public:
  virtual ~Node() = default;

  virtual size_t getErrorCount() const noexcept = 0;

  virtual const Error& getError(size_t index) const noexcept = 0;

  virtual size_t getLeafCount() const noexcept = 0;

  virtual const Leaf& getLeaf(size_t index) const noexcept = 0;

  virtual size_t getChildCount() const noexcept = 0;

  virtual const Node& getChild(size_t index) const noexcept = 0;

  virtual const char* getName() const noexcept = 0;

  virtual bool hasName(const char*) const noexcept = 0;

  void print(std::ostream&) const;
};

} // namespace peg

#endif // PEGTOOL_H
