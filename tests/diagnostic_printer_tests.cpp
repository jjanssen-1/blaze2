#include <gtest/gtest.h>

#include "core/DiagnosticPrinter.h"
#include "core/Errors.h"
#include "core/Source.h"

using namespace blaze::core;

TEST(DiagnosticPrinterTest, FormatWithoutSource) {
  PrintOptions opts;
  opts.useColor = false;
  DiagnosticPrinter printer(opts);

  Diagnostic diag{error, 2005, "type mismatch", SourceLocation::empty()};

  std::string expected = "error[2005]: type mismatch\n";
  EXPECT_EQ(printer.format(diag), expected);
}

TEST(DiagnosticPrinterTest, FormatWithSource) {
  PrintOptions opts;
  opts.useColor = false;
  opts.contextLines = 1;
  DiagnosticPrinter printer(opts);

  Source source("line 1\nline 2 error here\nline 3\n", "test.blz");
  SourceLocation loc(SourceView::empty(), 2, 8, 14, 1);

  Diagnostic diag{error, 2005, "type mismatch", loc};

  std::string expected = "error[2005]: type mismatch\n"
                         "  --> test.blz:2:8\n"
                         "   |\n"
                         " 1 | line 1\n"
                         " 2 | line 2 error here\n"
                         "   |        ^\n"
                         " 3 | line 3\n"
                         "   |\n";

  EXPECT_EQ(printer.format(diag, &source), expected);
}

TEST(DiagnosticPrinterTest, FormatDiagnosticList) {
  PrintOptions opts;
  opts.useColor = false;
  opts.contextLines = 0;
  DiagnosticPrinter printer(opts);

  Source source("a\nb\nc\n", "multi.blz");

  DiagnosticList list;
  list.reportError(1001, "first",
                   SourceLocation(SourceView::empty(), 1, 1, 0, 1));
  list.reportWarning(1002, "second",
                     SourceLocation(SourceView::empty(), 3, 1, 4, 1));

  std::string expected = "error[1001]: first\n"
                         "  --> multi.blz:1:1\n"
                         "   |\n"
                         " 1 | a\n"
                         "   | ^\n"
                         "   |\n"
                         "warning[1002]: second\n"
                         "  --> multi.blz:3:1\n"
                         "   |\n"
                         " 3 | c\n"
                         "   | ^\n"
                         "   |\n";

  EXPECT_EQ(printer.format(list, &source), expected);
}
