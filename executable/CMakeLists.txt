add_executable(blaze
  main.cpp
  commands/VerifyCommand.cpp
  commands/BuildCommand.cpp
  commands/EmitCommand.cpp
  commands/RunCommand.cpp
)

target_link_libraries(blaze PRIVATE driver CLI11::CLI11)

# LLVM linker flags (e.g. RPATH) are exported by the llvm_backend CMakeLists.
# LLVM libraries themselves propagate transitively via llvm_backend's PUBLIC linkage.
if(BLAZE_LLVM_LDFLAGS)
  set_property(TARGET blaze APPEND_STRING PROPERTY LINK_FLAGS " ${BLAZE_LLVM_LDFLAGS}")
endif()
