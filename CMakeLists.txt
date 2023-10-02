cmake_minimum_required(VERSION 3.22)
project(scribble C)

set(CMAKE_C_STANDARD 23)

set(scribble_NAME Scribble)
set(scribble_VERSION_MAJOR 1)
set(scribble_VERSION_MINOR 0)

#if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX ${PROJECT_BINARY_DIR} CACHE PATH "..." FORCE)
#endif()
set(scribble_DIR ${CMAKE_INSTALL_PREFIX})
set(scribble_DATADIR ${CMAKE_INSTALL_PREFIX}/share)

configure_file (
  "${PROJECT_SOURCE_DIR}/config.h.in"
  "${PROJECT_BINARY_DIR}/config.h"
)

# add the binary tree to the search path for include files
# so that we will find config.h
include_directories("${PROJECT_BINARY_DIR}")

add_subdirectory("src")
add_subdirectory("rt")
add_subdirectory("test")

install(TARGETS scribble scribblert testlib sv_test
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib)
