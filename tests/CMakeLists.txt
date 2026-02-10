include(FetchContent)

FetchContent_Declare(
 googletest
 URL https://github.com/google/googletest/archive/refs/tags/v1.15.2.zip
 DOWNLOAD_EXTRACT_TIMESTAMP TRUE
)

set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

FetchContent_MakeAvailable(googletest)

add_executable(example_tests
 parser_tests.cpp
 ast_parse_tests.cpp
 resolver_driver_tests.cpp
 type_checker_tests.cpp
)
target_link_libraries(example_tests PRIVATE GTest::gtest_main frontend)
target_include_directories(example_tests PRIVATE
 ${BLAZE_SRC_DIR}
 ${BLAZE_SRC_DIR}/frontend
)

include(GoogleTest)
gtest_discover_tests(example_tests)
