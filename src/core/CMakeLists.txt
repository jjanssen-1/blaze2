add_library(core STATIC
  Source.cpp
  Errors.cpp
  DiagnosticPrinter.cpp
)

target_include_directories(core PUBLIC
  ${BLAZE_SRC_DIR}
)

target_link_libraries(core PUBLIC tl::expected fmt::fmt)
