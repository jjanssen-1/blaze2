add_library(driver STATIC
  Driver.cpp
)

target_include_directories(driver PUBLIC
  ${BLAZE_SRC_DIR}
)

target_link_libraries(driver PUBLIC
  core
  frontend
  llvm_backend
  z3_backend
)
