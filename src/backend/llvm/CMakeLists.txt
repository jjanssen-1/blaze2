set(LLVM_USE_CMAKE_TARGETS OFF CACHE BOOL "Avoid importing LLVM targets to prevent opt collision" FORCE)
find_package(LLVM CONFIG REQUIRED)

message(STATUS "Found LLVM via LLVMConfig: ${LLVM_PACKAGE_VERSION} (in ${LLVM_DIR})")

set(LLVM_COMPONENTS
  core
  support
  nativecodegen
  target
  mc
  mcparser
  asmprinter
  X86AsmParser
  X86CodeGen
  X86Desc
  X86Info
)

llvm_map_components_to_libnames(LLVM_LIBS ${LLVM_COMPONENTS})

set(LLVM_DEFINITIONS_LIST ${LLVM_DEFINITIONS})
separate_arguments(LLVM_DEFINITIONS_LIST)

set(BLAZE_LLVM_CXXFLAGS "" CACHE STRING "LLVM cxxflags")
set(BLAZE_LLVM_LDFLAGS "${LLVM_EXPORTED_LINKER_FLAGS}" CACHE STRING "LLVM ldflags")
set(BLAZE_LLVM_INCLUDEDIR "${LLVM_INCLUDE_DIRS}" CACHE STRING "LLVM include dir")
set(BLAZE_LLVM_LIBS "${LLVM_LIBS}" CACHE STRING "LLVM libfiles")
set(BLAZE_LLVM_SYSTEM_LIBS "${LLVM_SYSTEM_LIBS}" CACHE STRING "LLVM system libs")
set(BLAZE_LLVM_LINK_FLAGS "${LLVM_EXPORTED_LINKER_FLAGS}" CACHE STRING "LLVM link flags")

add_library(llvm_backend STATIC
  llvmbackend.cpp
)

target_include_directories(llvm_backend PUBLIC ${LLVM_INCLUDE_DIRS})
target_link_directories(llvm_backend PUBLIC ${LLVM_LIBRARY_DIRS})
target_compile_options(llvm_backend PUBLIC ${LLVM_DEFINITIONS_LIST})
target_link_libraries(llvm_backend PUBLIC ${LLVM_LIBS} ${LLVM_SYSTEM_LIBS} core frontend)
