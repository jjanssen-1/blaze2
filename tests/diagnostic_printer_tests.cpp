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

// Column is 0-based in SourceLocation (ANTLR convention).
// DiagnosticPrinter must display it as 1-based in the "  --> file:line:col"
// header.  The caret is placed using the 0-based column value as the padding
// width (number of spaces between the pipe-space and the first caret).
//
// Gutter width is computed dynamically: max(2, digits(lastLineShown)).
// For a 3-line file with contextLines=1 the last shown line is 3, which
// fits in 1 digit, so gutterWidth = 2.
//
// Layout of a numbered line:
//   "{gutterColor}{lineNo:>gutterWidth} |{resetColor} {lineText}\n"
// Layout of a blank gutter line:
//   "{gutterColor}{spaces:gutterWidth} |{resetColor}\n"
// Layout of a caret line:
//   "{gutterColor}{spaces:gutterWidth} |{resetColor} {padding}{carets}\n"
//
// Example with gutterWidth=2, line 2, col=8, cpLen=1:
//   " 2 | line 2 error here\n"
//   "   |         ^\n"           (8 spaces of padding + "^")

TEST(DiagnosticPrinterTest, FormatWithSource) {
  PrintOptions opts;
  opts.useColor = false;
  opts.contextLines = 1;
  DiagnosticPrinter printer(opts);

  // Source has 3 lines; we will highlight column 8 (0-based) on line 2.
  // The token "error" starts at byte offset 14 within the source string:
  //   "line 1\n"       -> 7 bytes  (offsets 0-6)
  //   "line 2 error here\n" -> bytes starting at 7
  //   character at index 8 (0-based) within line 2 is 'e' of "error".
  //
  // SourceLocation(view, line=2, column=8, codepointOffset=14, cpLength=1)
  //
  // Expected header: "  --> test.blz:2:9"  (column + 1 = 9, 1-based)
  // gutterWidth = max(2, digits(3)) = 2
  // Blank sep : "   |\n"   -> "  " + " |" but we write "{2 spaces} |"
  // Line 1    : " 1 | line 1\n"
  // Line 2    : " 2 | line 2 error here\n"
  // Caret     : "   |         ^\n"  -> "  " + " | " + 8 spaces + "^"
  // Line 3    : " 3 | line 3\n"
  // Blank sep : "   |\n"

  Source source("line 1\nline 2 error here\nline 3\n", "test.blz");
  // column=8 (0-based), codepointLength=1
  SourceLocation loc(SourceView::empty(), 2, 8, 14, 1);

  Diagnostic diag{error, 2005, "type mismatch", loc};

  std::string expected = "error[2005]: type mismatch\n"
                         "  --> test.blz:2:9\n"
                         "   |\n"
                         " 1 | line 1\n"
                         " 2 | line 2 error here\n"
                         "   |         ^\n"
                         " 3 | line 3\n"
                         "   |\n";

  EXPECT_EQ(printer.format(diag, &source), expected);
}

// With contextLines=0 only the error line itself is shown.
// gutterWidth = max(2, digits(maxLine)).
//
// Diag 1: line=1, col=0 (0-based) -> displayCol=1, padding=0 spaces
//   header "  --> multi.blz:1:1"
//   gutterWidth = max(2, digits(1)) = 2
//   "   |\n"
//   " 1 | a\n"
//   "   | ^\n"   (0 spaces + "^")
//   "   |\n"
//
// Diag 2: line=3, col=0 (0-based) -> displayCol=1, padding=0 spaces
//   header "  --> multi.blz:3:1"
//   gutterWidth = max(2, digits(3)) = 2
//   same pattern, line 3

TEST(DiagnosticPrinterTest, FormatDiagnosticList) {
  PrintOptions opts;
  opts.useColor = false;
  opts.contextLines = 0;
  DiagnosticPrinter printer(opts);

  Source source("a\nb\nc\n", "multi.blz");

  DiagnosticList list;
  // column=0 (0-based): first character of the line
  list.reportError(1001, "first",
                   SourceLocation(SourceView::empty(), 1, 0, 0, 1));
  list.reportWarning(1002, "second",
                     SourceLocation(SourceView::empty(), 3, 0, 4, 1));

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

// Gutter width grows for files with many lines.
// A file with 100+ lines needs a 3-digit gutter.
TEST(DiagnosticPrinterTest, GutterWidthExpandsForLargeLineNumbers) {
  PrintOptions opts;
  opts.useColor = false;
  opts.contextLines = 0;
  DiagnosticPrinter printer(opts);

  // Build a 120-line source; highlight line 100.
  std::string src;
  for (int i = 1; i <= 120; ++i) {
    src += "line content\n";
  }
  Source source(src, "big.blz");

  // Line 100 is at codepoint offset 99 * len("line content\n") = 99 * 13 =
  // 1287. column=0 (0-based), cpLength=4 (underline "line").
  SourceLocation loc(SourceView::empty(), 100, 0, 99 * 13, 4);
  Diagnostic diag{error, 9999, "big file error", loc};

  std::string result = printer.format(diag, &source);

  // Header: column 0 -> displayCol 1
  EXPECT_NE(result.find("  --> big.blz:100:1"), std::string::npos)
      << "Header should show line 100 with 1-based column 1";

  // The line number "100" should be right-aligned with gutterWidth >= 3.
  // Pattern: "100 | line content"
  EXPECT_NE(result.find("100 | line content"), std::string::npos)
      << "Line 100 should appear with 3-digit gutter";

  // The blank gutter lines should use the same width.
  // With gutterWidth=3 a blank gutter line is "    |" (3 spaces + " |").
  EXPECT_NE(result.find("    |"), std::string::npos)
      << "Blank gutter lines should use 3-space indent for gutterWidth=3";

  // The caret should appear right below column 0 with length 4.
  EXPECT_NE(result.find("    | ^^^^"), std::string::npos)
      << "Caret line: 3-space gutter + ' | ' + no padding + 4 carets";
}

// Column-1 (0-based) on a line means one space of padding before the caret.
TEST(DiagnosticPrinterTest, CaretPaddingRespectsColumn) {
  PrintOptions opts;
  opts.useColor = false;
  opts.contextLines = 0;
  DiagnosticPrinter printer(opts);

  // "fn foo() -> i32 { return x; }"
  //  0123456789...
  // Highlight 'x' at column 26 (0-based), cpLength=1.
  Source source("fn foo() -> i32 { return x; }\n", "test.blz");
  SourceLocation loc(SourceView::empty(), 1, 26, 26, 1);
  Diagnostic diag{error, 2001, "unresolved symbol", loc};

  std::string result = printer.format(diag, &source);

  // Header should be 1-based: column 26 + 1 = 27
  EXPECT_NE(result.find("test.blz:1:27"), std::string::npos)
      << "Header column should be 1-based (27)";

  // The caret line should have 26 spaces of padding then '^'.
  std::string expectedCaretLine = "   | " + std::string(26, ' ') + "^\n";
  EXPECT_NE(result.find(expectedCaretLine), std::string::npos)
      << "Caret should be padded by 26 spaces (column 26, 0-based)";
}

// Multi-caret span: codepointLength > 1 produces multiple '^' characters.
TEST(DiagnosticPrinterTest, CaretSpanMatchesCodepointLength) {
  PrintOptions opts;
  opts.useColor = false;
  opts.contextLines = 0;
  DiagnosticPrinter printer(opts);

  // Highlight the keyword "return" (6 chars) at column 0, line 1.
  Source source("return 42;\n", "test.blz");
  SourceLocation loc(SourceView::empty(), 1, 0, 0, 6);
  Diagnostic diag{error, 1003, "illegal statement", loc};

  std::string result = printer.format(diag, &source);

  // Caret line: no padding, 6 carets.
  EXPECT_NE(result.find("   | ^^^^^^\n"), std::string::npos)
      << "Six carets should underline 'return' (cpLength=6)";
}

// Zero codepointLength falls back to a single caret.
TEST(DiagnosticPrinterTest, ZeroCodepointLengthFallsBackToSingleCaret) {
  PrintOptions opts;
  opts.useColor = false;
  opts.contextLines = 0;
  DiagnosticPrinter printer(opts);

  Source source("bad;\n", "test.blz");
  // codepointLength = 0 -> should fall back to 1 caret
  SourceLocation loc(SourceView::empty(), 1, 0, 0, 0);
  Diagnostic diag{error, 1000, "unexpected token", loc};

  std::string result = printer.format(diag, &source);

  EXPECT_NE(result.find("   | ^\n"), std::string::npos)
      << "Zero cpLength should fall back to a single caret";
}

TEST(DiagnosticPrinterTest, TabExpansionAffectsCaretPadding) {
  PrintOptions opts;
  opts.useColor = false;
  opts.contextLines = 0;
  opts.tabWidth = 4;
  DiagnosticPrinter printer(opts);

  Source source("\treturn x;\n", "tabs.blz");
  // Column 1 (0-based) points to 'r' after a leading tab.
  SourceLocation loc(SourceView::empty(), 1, 1, 1, 1);
  Diagnostic diag{error, 1000, "unexpected token", loc};

  std::string result = printer.format(diag, &source);

  // With tabWidth=4, the tab expands to 4 spaces, so caret padding is 4.
  EXPECT_NE(result.find("   |     ^\n"), std::string::npos)
      << "Tab should expand to 4 spaces before the caret";
}

TEST(DiagnosticPrinterTest, UnicodeCodepointPaddingIsAccurate) {
  PrintOptions opts;
  opts.useColor = false;
  opts.contextLines = 0;
  DiagnosticPrinter printer(opts);

  // 'é' is a multi-byte codepoint; column counts codepoints, not bytes.
  Source source("café x\n", "unicode.blz");
  // 'x' is at codepoint column 5 (0-based): c(0) a(1) f(2) é(3) (4) x(5)
  SourceLocation loc(SourceView::empty(), 1, 5, 5, 1);
  Diagnostic diag{error, 1000, "unexpected token", loc};

  std::string result = printer.format(diag, &source);

  // Expect 5 spaces of padding after the pipe-space.
  EXPECT_NE(result.find("   |      ^\n"), std::string::npos)
      << "Unicode codepoints should not skew caret alignment";
}
