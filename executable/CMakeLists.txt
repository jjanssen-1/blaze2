add_executable(blaze_cli
  main.cpp
)

target_link_libraries(blaze_cli PRIVATE frontend llvm_backend)

add_executable(emit_llvm
  emit_llvm.cpp
)

target_link_libraries(emit_llvm PRIVATE frontend llvm_backend)

# LLVM linker flags (e.g. RPATH) are exported by the llvm_backend CMakeLists.
# LLVM libraries themselves propagate transitively via llvm_backend's PUBLIC linkage.
if(BLAZE_LLVM_LDFLAGS)
  set_property(TARGET blaze_cli APPEND_STRING PROPERTY LINK_FLAGS " ${BLAZE_LLVM_LDFLAGS}")
  set_property(TARGET emit_llvm APPEND_STRING PROPERTY LINK_FLAGS " ${BLAZE_LLVM_LDFLAGS}")
endif()
