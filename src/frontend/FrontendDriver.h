#pragma once

#include "core/Errors.h"
#include "core/Source.h"
#include "frontend/Ast.h"
#include "frontend/Resolver.h"

#include <memory>
#include <string>

#include <tl/expected.hpp>

namespace blaze::frontend {

enum class ParseError {
  InvalidSource,
  NoAstProduced,
  AstCastFailed,
  Exception,
  SourceLoadFailed
};

const char *toString(ParseError error);

struct ParseAstResult {
  std::shared_ptr<Root> root;
  std::shared_ptr<core::Source> source;
};

tl::expected<ParseAstResult, ParseError>
parseFile(const std::string &path, core::DiagnosticList &diagnostics);

tl::expected<ParseAstResult, ParseError>
parseSource(const std::shared_ptr<core::Source> &source,
            core::DiagnosticList &diagnostics);

enum class FrontendError { ParseFailed, ResolveFailed };

const char *toString(FrontendError error);

struct FrontendResult {
  ParseAstResult parse;
  ResolveResult resolve;
};

enum class TypeCheckError { ParseFailed, ResolveFailed, TypeCheckFailed };

const char *toString(TypeCheckError error);

// TypeCheckResult is structurally identical to FrontendResult — the distinction
// lives in the error type, not the success payload.
using TypeCheckResult = FrontendResult;

tl::expected<FrontendResult, FrontendError>
resolveSource(const std::shared_ptr<core::Source> &source,
              core::DiagnosticList &diagnostics);

tl::expected<FrontendResult, FrontendError>
resolveFile(const std::string &path, core::DiagnosticList &diagnostics);

tl::expected<TypeCheckResult, TypeCheckError>
checkSource(const std::shared_ptr<core::Source> &source,
            core::DiagnosticList &diagnostics);

tl::expected<TypeCheckResult, TypeCheckError>
checkFile(const std::string &path, core::DiagnosticList &diagnostics);

} // namespace blaze::frontend
