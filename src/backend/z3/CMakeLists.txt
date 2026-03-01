include(ExternalProject)
include(GNUInstallDirs)

if(BLAZE_USE_SYSTEM_Z3)
  find_package(Z3 CONFIG REQUIRED)

  if(TARGET z3::libz3 AND NOT TARGET libz3)
    add_library(libz3 ALIAS z3::libz3)
  elseif(NOT TARGET libz3)
    if(Z3_LIBRARIES)
      add_library(libz3 UNKNOWN IMPORTED)
      set_target_properties(libz3 PROPERTIES
        IMPORTED_LOCATION "${Z3_LIBRARIES}"
        INTERFACE_INCLUDE_DIRECTORIES "${Z3_INCLUDE_DIRS}"
      )
    else()
      message(FATAL_ERROR "System Z3 requested but no libz3 target or Z3_LIBRARIES found.")
    endif()
  endif()
else()
  set(Z3_GIT_REPOSITORY https://github.com/Z3Prover/z3)
  set(Z3_GIT_TAG z3-4.15.3)

  if(DEFINED BLAZE_EXTERNAL_DIR)
    set(Z3_BASE_DIR ${BLAZE_EXTERNAL_DIR}/z3)
  else()
    set(Z3_BASE_DIR ${CMAKE_BINARY_DIR}/_deps/z3)
  endif()

  set(Z3_SOURCE_DIR ${Z3_BASE_DIR}/src)
  set(Z3_BINARY_DIR ${Z3_BASE_DIR}/build)
  set(Z3_INSTALL_DIR ${Z3_BASE_DIR}/install)
  set(Z3_INSTALL_LIBDIR ${CMAKE_INSTALL_LIBDIR})
  if(NOT Z3_INSTALL_LIBDIR)
    set(Z3_INSTALL_LIBDIR lib)
  endif()

  set(Z3_LIBRARY ${Z3_INSTALL_DIR}/${Z3_INSTALL_LIBDIR}/libz3.a)
  set(Z3_INCLUDE_DIR ${Z3_INSTALL_DIR}/include)
  set(Z3_STAMP_DIR ${Z3_BASE_DIR}/stamp)
  set(Z3_TMP_DIR ${Z3_BASE_DIR}/tmp)
  file(MAKE_DIRECTORY ${Z3_INCLUDE_DIR})
  file(MAKE_DIRECTORY ${Z3_STAMP_DIR})
  file(MAKE_DIRECTORY ${Z3_TMP_DIR})

  ExternalProject_Add(z3_ep
    PREFIX ${Z3_BASE_DIR}
    GIT_REPOSITORY ${Z3_GIT_REPOSITORY}
    GIT_TAG ${Z3_GIT_TAG}
    UPDATE_COMMAND ""
    SOURCE_DIR ${Z3_SOURCE_DIR}
    BINARY_DIR ${Z3_BINARY_DIR}
    DOWNLOAD_DIR ${Z3_BASE_DIR}/download
    STAMP_DIR ${Z3_STAMP_DIR}
    TMP_DIR ${Z3_TMP_DIR}
    CMAKE_ARGS
      -DCMAKE_INSTALL_PREFIX=${Z3_INSTALL_DIR}
      -DCMAKE_INSTALL_LIBDIR=${Z3_INSTALL_LIBDIR}
      -DZ3_BUILD_EXECUTABLE=OFF
      -DZ3_BUILD_TEST_EXECUTABLES=OFF
      -DZ3_BUILD_DOCUMENTATION=OFF
      -DZ3_BUILD_PYTHON_BINDINGS=OFF
      -DZ3_BUILD_DOTNET_BINDINGS=OFF
      -DZ3_BUILD_JAVA_BINDINGS=OFF
      -DZ3_BUILD_OCAML_BINDINGS=OFF
      -DZ3_BUILD_JULIA_BINDINGS=OFF
      -DZ3_BUILD_LIBZ3_SHARED=OFF
    BUILD_BYPRODUCTS ${Z3_LIBRARY}
  )

  add_library(libz3 STATIC IMPORTED GLOBAL)
  set_target_properties(libz3 PROPERTIES
    IMPORTED_LOCATION ${Z3_LIBRARY}
    INTERFACE_INCLUDE_DIRECTORIES ${Z3_INCLUDE_DIR}
  )
  add_dependencies(libz3 z3_ep)
endif()

add_library(z3_backend STATIC
  z3backend.cpp
)

target_link_libraries(z3_backend PUBLIC libz3 core frontend)
