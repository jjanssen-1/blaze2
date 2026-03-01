add_executable(blaze_cli
  main.cpp
)

target_link_libraries(blaze_cli PRIVATE frontend llvm_backend)

if(BLAZE_LLVM_LIBS)

  set(LLVM_LIBFILES ${BLAZE_LLVM_LIBS})
  set(LLVM_SYSTEM_LIBS ${BLAZE_LLVM_SYSTEM_LIBS})
  set(LLVM_LDFLAGS ${BLAZE_LLVM_LDFLAGS})
  set(LLVM_LINK_FLAGS ${BLAZE_LLVM_LINK_FLAGS})

  separate_arguments(LLVM_LIBFILES UNIX_COMMAND)
  separate_arguments(LLVM_SYSTEM_LIBS UNIX_COMMAND)
  separate_arguments(LLVM_LDFLAGS UNIX_COMMAND)
  separate_arguments(LLVM_LINK_FLAGS UNIX_COMMAND)

  target_link_libraries(blaze_cli PRIVATE ${LLVM_LIBFILES} ${LLVM_SYSTEM_LIBS})
  set_property(TARGET blaze_cli APPEND_STRING PROPERTY LINK_FLAGS " ${BLAZE_LLVM_LDFLAGS} ${BLAZE_LLVM_LINK_FLAGS}")
endif()
