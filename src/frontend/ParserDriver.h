#pragma once

#include "core/Errors.h"
#include "core/Source.h"
#include "frontend/Ast.h"

#include <string>
#include <memory>

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
} // namespace blaze::frontend
