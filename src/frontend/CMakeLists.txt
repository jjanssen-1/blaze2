antlr_target(BlazeGrammar ${CMAKE_CURRENT_SOURCE_DIR}/Blaze.g4
 LEXER PARSER VISITOR
)

add_library(frontend STATIC
 FrontendDriver.cpp
 Resolver.cpp
 Builtins.cpp
 AntlrVisitor.cpp
 SymbolTable.cpp
 TypeChecker.cpp
 ${ANTLR_BlazeGrammar_CXX_OUTPUTS}
)

target_include_directories(frontend PUBLIC
 ${CMAKE_CURRENT_SOURCE_DIR}
 ${ANTLR_BlazeGrammar_OUTPUT_DIR}
)

target_link_libraries(frontend PUBLIC antlr4_static core)
