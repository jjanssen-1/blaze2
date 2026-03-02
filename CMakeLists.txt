CMAKE_MINIMUM_REQUIRED(VERSION 3.12)

project(
  blaze
  VERSION 999
)

set(CMAKE_CXX_STANDARD 20)

option(BLAZE_USE_SYSTEM_FMT "Use system-installed fmt" OFF)
option(BLAZE_USE_SYSTEM_GTEST "Use system-installed GoogleTest" OFF)
option(BLAZE_USE_SYSTEM_Z3 "Use system-installed Z3" OFF)
option(BLAZE_USE_SYSTEM_ANTLR4_RUNTIME "Use system-installed ANTLR4 runtime" OFF)

set(BLAZE_EXTERNAL_DIR "${CMAKE_SOURCE_DIR}/build-external" CACHE PATH "External dependencies build/download directory")
file(MAKE_DIRECTORY "${BLAZE_EXTERNAL_DIR}")
if(NOT DEFINED FETCHCONTENT_BASE_DIR)
  set(FETCHCONTENT_BASE_DIR "${BLAZE_EXTERNAL_DIR}/_deps" CACHE PATH "FetchContent base directory")
endif()

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

set(BLAZE_SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
if(CMAKE_CONFIGURATION_TYPES)
  foreach(config ${CMAKE_CONFIGURATION_TYPES})
    string(TOUPPER ${config} config_upper)
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_${config_upper}
        ${CMAKE_BINARY_DIR}/bin/${config})
  endforeach()
endif()


set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include(CTest)
enable_testing()

include(FetchContent)
# Libraries

set(EXPECTED_BUILD_TESTS OFF CACHE BOOL "" FORCE)
set(EXPECTED_BUILD_PACKAGE OFF CACHE BOOL "" FORCE)
FetchContent_Declare(
  expected
  GIT_REPOSITORY https://github.com/TartanLlama/expected.git
  GIT_TAG v1.1.0
)
FetchContent_MakeAvailable(expected)

if(BLAZE_USE_SYSTEM_FMT)
  find_package(fmt CONFIG REQUIRED)
else()
  FetchContent_Declare(
    fmt
    GIT_REPOSITORY https://github.com/fmtlib/fmt.git
    GIT_TAG        10.2.1  # Use a specific release tag
  )

  FetchContent_MakeAvailable(fmt)
endif()

FetchContent_Declare(
  cli11
  GIT_REPOSITORY https://github.com/CLIUtils/CLI11.git
  GIT_TAG        v2.4.2
)
FetchContent_MakeAvailable(cli11)

# ANTLR4 versions and download information
set(ANTLR4_TAG 4.13.2)
set(ANTLR_JAR_NAME antlr-${ANTLR4_TAG}-complete.jar)
set(ANTLR_JAR_URL https://www.antlr.org/download/${ANTLR_JAR_NAME})
set(ANTLR_TOOLS_DIR ${BLAZE_EXTERNAL_DIR}/tools)
set(ANTLR_JAR_PATH ${ANTLR_TOOLS_DIR}/${ANTLR_JAR_NAME})

# Ensure the jar exists at configure time
file(MAKE_DIRECTORY ${ANTLR_TOOLS_DIR})
if(NOT EXISTS ${ANTLR_JAR_PATH})
  message(STATUS "Downloading ${ANTLR_JAR_NAME}...")
  file(DOWNLOAD
    ${ANTLR_JAR_URL}
    ${ANTLR_JAR_PATH}
    SHOW_PROGRESS
    STATUS _dl_status
  )
  list(GET _dl_status 0 _dl_code)
  if(NOT _dl_code EQUAL 0)
    list(GET _dl_status 1 _dl_msg)
    message(FATAL_ERROR "Failed to download ANTLR jar: ${_dl_msg}")
  endif()
endif()

set(ANTLR_EXECUTABLE ${ANTLR_JAR_PATH})

add_definitions(-DANTLR4CPP_STATIC)
set(ANTLR4_WITH_STATIC_CRT OFF)
include(ExternalAntlr4Cpp)

# add macros to generate ANTLR Cpp code from grammar
find_package(ANTLR REQUIRED)

# Call macro to add lexer and grammar to your build dependencies.
# antlr_target(SampleGrammarLexer TLexer.g4 LEXER
#              PACKAGE antlrcpptest)
# antlr_target(SampleGrammarParser TParser.g4 PARSER
#              PACKAGE antlrcpptest
#              DEPENDS_ANTLR SampleGrammarLexer
#              COMPILE_FLAGS -lib ${ANTLR_SampleGrammarLexer_OUTPUT_DIR})
#
# include generated files in project environment
# include_directories(${ANTLR_SampleGrammarLexer_OUTPUT_DIR})
# include_directories(${ANTLR_SampleGrammarParser_OUTPUT_DIR})

# add generated grammar to demo binary target
# add_executable(demo main.cpp
#                ${ANTLR_SampleGrammarLexer_CXX_OUTPUTS}
#                ${ANTLR_SampleGrammarParser_CXX_OUTPUTS})
# target_link_libraries(demo antlr4_static)
add_subdirectory(src)
add_subdirectory(executable)
add_subdirectory(tests)
