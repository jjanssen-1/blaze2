set(LLVM_USE_CMAKE_TARGETS
    OFF CACHE BOOL
    "Avoid importing LLVM targets to prevent opt collision"
    FORCE
)
find_package(LLVM CONFIG REQUIRED)

message(STATUS "Found LLVM via LLVMConfig: ${LLVM_PACKAGE_VERSION} (in ${LLVM_DIR})")

# Use native target components so this builds on any architecture (X86, AArch64, etc.).
# nativecodegen expands to ${LLVM_NATIVE_ARCH}CodeGen, Desc, and Info automatically.
# We add the native AsmParser explicitly since there is no nativeasmparser pseudo-component.
set(LLVM_COMPONENTS
  core
  support
  nativecodegen
  target
  mc
  mcparser
  asmprinter
  ${LLVM_NATIVE_ARCH}AsmParser
)

llvm_map_components_to_libnames(LLVM_LIBS ${LLVM_COMPONENTS})

set(LLVM_DEFINITIONS_LIST ${LLVM_DEFINITIONS})
separate_arguments(LLVM_DEFINITIONS_LIST)

add_library(llvm_backend STATIC
  llvmbackend.cpp
)

target_include_directories(llvm_backend PUBLIC ${LLVM_INCLUDE_DIRS})
target_link_directories(llvm_backend PUBLIC ${LLVM_LIBRARY_DIRS})
target_compile_options(llvm_backend PUBLIC ${LLVM_DEFINITIONS_LIST})
target_link_libraries(llvm_backend PUBLIC ${LLVM_LIBS} ${LLVM_SYSTEM_LIBS} core frontend)

# Export LLVM linker flags so that executables linking llvm_backend can pick
# them up if needed (e.g. RPATH settings from the LLVM installation).
set(BLAZE_LLVM_LDFLAGS "${LLVM_EXPORTED_LINKER_FLAGS}" CACHE INTERNAL "LLVM linker flags")
