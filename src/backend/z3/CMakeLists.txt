FetchContent_Declare(Z3
        GIT_REPOSITORY https://github.com/Z3Prover/z3
        GIT_TAG        z3-4.15.3
)
FetchContent_MakeAvailable(Z3)

# Add the C++ API include directory for z3++.h
if(TARGET libz3)
  target_include_directories(libz3 INTERFACE
    $<BUILD_INTERFACE:${z3_SOURCE_DIR}/src/api/c++>
  )
endif()

add_library(z3_backend STATIC
  z3backend.cpp
)

target_link_libraries(z3_backend PUBLIC libz3 core frontend)
