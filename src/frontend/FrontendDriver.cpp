#include "FrontendDriver.h"

#include "AntlrVisitor.h"
#include "BlazeLexer.h"
#include "BlazeParser.h"
#include "TypeChecker.h"

#include <any>
#include <exception>
#include <memory>

#include <antlr4-runtime.h>

namespace blaze::frontend {

namespace {

core::SourceLocation tokenLocation(const antlr4::Token *token,
                                   const core::Source &source, size_t line,
                                   size_t column) {
  if (token == nullptr) {
    return core::SourceLocation::empty();
  }

  const auto startIndex = token->getStartIndex();
  const auto stopIndex = token->getStopIndex();
  if (startIndex < 0 || stopIndex < startIndex) {
    return core::SourceLocation::empty();
  }

  const auto offset = static_cast<core::size>(startIndex);
  const auto length = static_cast<core::size>(stopIndex - startIndex + 1);
  auto view = source.view(offset, length);
  return core::SourceLocation(view, static_cast<core::size>(line),
                              static_cast<core::size>(column));
}

class DiagnosticErrorListener : public antlr4::BaseErrorListener {
public:
  DiagnosticErrorListener(const core::Source &source,
                          core::DiagnosticList &diagnostics)
      : m_source(source), m_diagnostics(diagnostics) {}

  void syntaxError(antlr4::Recognizer * /*recognizer*/,
                   antlr4::Token *offendingSymbol, size_t line,
                   size_t charPositionInLine, const std::string &msg,
                   std::exception_ptr /*e*/) override {
    auto location =
        tokenLocation(offendingSymbol, m_source, line, charPositionInLine);
    m_diagnostics.reportError(core::ERROR_UNEXPECTED_TOKEN, msg, location);
  }

private:
  const core::Source &m_source;
  core::DiagnosticList &m_diagnostics;
};

} // namespace

const char *toString(ParseError error) {
  switch (error) {
  case ParseError::InvalidSource:
    return "Invalid parse source";
  case ParseError::NoAstProduced:
    return "Parse returned no AST";
  case ParseError::AstCastFailed:
    return "Parse failed due to AST cast error";
  case ParseError::Exception:
    return "Parse failed due to exception";
  case ParseError::SourceLoadFailed:
    return "Failed to load source";
  default:
    return "Unknown parse error";
  }
}

const char *toString(FrontendError error) {
  switch (error) {
  case FrontendError::ParseFailed:
    return "Parse failed";
  case FrontendError::ResolveFailed:
    return "Resolve failed";
  default:
    return "Unknown frontend error";
  }
}

const char *toString(TypeCheckError error) {
  switch (error) {
  case TypeCheckError::ParseFailed:
    return "Parse failed";
  case TypeCheckError::ResolveFailed:
    return "Resolve failed";
  case TypeCheckError::TypeCheckFailed:
    return "Type check failed";
  default:
    return "Unknown type check error";
  }
}

tl::expected<ParseAstResult, ParseError>
parseSource(const std::shared_ptr<core::Source> &source,
            core::DiagnosticList &diagnostics) {
  try {
    if (!source) {
      return tl::make_unexpected(ParseError::InvalidSource);
    }

    antlr4::ANTLRInputStream input(std::string(source->text()));
    BlazeLexer lexer(&input);
    antlr4::CommonTokenStream tokens(&lexer);
    BlazeParser parser(&tokens);

    DiagnosticErrorListener errorListener(*source, diagnostics);
    lexer.removeErrorListeners();
    parser.removeErrorListeners();
    lexer.addErrorListener(&errorListener);
    parser.addErrorListener(&errorListener);

    auto *program = parser.program();
    if (diagnostics.hasErrors()) {
      return ParseAstResult{nullptr, source};
    }

    BlazeVisitorImpl visitor(source);
    auto rootAny = program ? program->accept(&visitor) : std::any{};
    if (!rootAny.has_value()) {
      return tl::make_unexpected(ParseError::NoAstProduced);
    }

    return ParseAstResult{std::any_cast<std::shared_ptr<Root>>(rootAny),
                          source};
  } catch (const std::bad_any_cast &) {
    return tl::make_unexpected(ParseError::AstCastFailed);
  } catch (const std::exception &) {
    return tl::make_unexpected(ParseError::Exception);
  }
}

tl::expected<ParseAstResult, ParseError>
parseFile(const std::string &path, core::DiagnosticList &diagnostics) {
  auto source = core::Source::fromFile(path);
  if (!source) {
    diagnostics.reportError(core::ERROR_SOURCE_LOAD_FAILED,
                            core::toString(source.error()),
                            core::SourceLocation::empty());
    return tl::make_unexpected(ParseError::SourceLoadFailed);
  }

  return parseSource(std::make_shared<core::Source>(std::move(*source)),
                     diagnostics);
}

tl::expected<FrontendResult, FrontendError>
resolveSource(const std::shared_ptr<core::Source> &source,
              core::DiagnosticList &diagnostics) {
  auto parseResult = parseSource(source, diagnostics);
  if (!parseResult || !parseResult.value().root) {
    return tl::make_unexpected(FrontendError::ParseFailed);
  }

  Resolver resolver(diagnostics);
  auto resolveResult = resolver.resolve(parseResult.value().root);
  if (!resolveResult || diagnostics.hasErrors()) {
    return tl::make_unexpected(FrontendError::ResolveFailed);
  }

  return FrontendResult{parseResult.value(), resolveResult.value()};
}

tl::expected<FrontendResult, FrontendError>
resolveFile(const std::string &path, core::DiagnosticList &diagnostics) {
  auto parseResult = parseFile(path, diagnostics);
  if (!parseResult || !parseResult.value().root) {
    return tl::make_unexpected(FrontendError::ParseFailed);
  }

  Resolver resolver(diagnostics);
  auto resolveResult = resolver.resolve(parseResult.value().root);
  if (!resolveResult || diagnostics.hasErrors()) {
    return tl::make_unexpected(FrontendError::ResolveFailed);
  }

  return FrontendResult{parseResult.value(), resolveResult.value()};
}

tl::expected<TypeCheckResult, TypeCheckError>
checkSource(const std::shared_ptr<core::Source> &source,
            core::DiagnosticList &diagnostics) {
  auto resolved = resolveSource(source, diagnostics);
  if (!resolved) {
    return tl::make_unexpected(resolved.error() == FrontendError::ParseFailed
                                   ? TypeCheckError::ParseFailed
                                   : TypeCheckError::ResolveFailed);
  }

  TypeChecker checker(resolved->resolve.symbols, resolved->resolve.builtins,
                      diagnostics);
  checker.checkProgram(resolved->parse.root);
  if (diagnostics.hasErrors()) {
    return tl::make_unexpected(TypeCheckError::TypeCheckFailed);
  }

  return std::move(resolved.value());
}

tl::expected<TypeCheckResult, TypeCheckError>
checkFile(const std::string &path, core::DiagnosticList &diagnostics) {
  auto source = core::Source::fromFile(path);
  if (!source) {
    diagnostics.reportError(core::ERROR_SOURCE_LOAD_FAILED,
                            core::toString(source.error()),
                            core::SourceLocation::empty());
    return tl::make_unexpected(TypeCheckError::ParseFailed);
  }

  return checkSource(std::make_shared<core::Source>(std::move(*source)),
                     diagnostics);
}

} // namespace blaze::frontend
