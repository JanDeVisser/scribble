cmake_minimum_required(VERSION 3.22)
project(scribble C)

set(CMAKE_C_STANDARD 23)

message("System: ${CMAKE_SYSTEM_NAME} Processor: ${CMAKE_SYSTEM_PROCESSOR} Version: ${CMAKE_SYSTEM_VERSION}")

if(APPLE)
  set(IS_APPLE TRUE)
  set(OS macos)
elseif(WIN32)
  set(IS_WINDOWS TRUE)
  set(OS windows)
else()
  set(IS_LINUX TRUE)
  set(OS linux)
endif()

set(scribble_NAME Scribble)
set(scribble_VERSION_MAJOR 1)
set(scribble_VERSION_MINOR 0)

# if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
set(CMAKE_INSTALL_PREFIX ${PROJECT_BINARY_DIR} CACHE PATH "..." FORCE)

# endif()
set(scribble_DIR ${CMAKE_INSTALL_PREFIX})
set(scribble_DATADIR ${CMAKE_INSTALL_PREFIX}/share)

include(CheckCSourceCompiles)

check_c_source_compiles("
   [[noreturn]] void testFunc()
   {}

   int main(void)
   {
     return 0;
   }
" HAVE_C23_ATTRIBUTE_NORETURN
)

check_c_source_compiles("
   __attribute__((noreturn)) void testFunc()
   {}

   int main(void)
   {
     return 0;
   }
" HAVE_LEGACY_ATTRIBUTE_NORETURN
)

include(CheckStructHasMember)
check_struct_has_member("struct dirent" d_namlen dirent.h HAVE_DIRENT_D_NAMLEN LANGUAGE C)

configure_file(
  "${PROJECT_SOURCE_DIR}/config.h.in"
  "${PROJECT_BINARY_DIR}/config.h"
)

# add the binary tree to the search path for include files
# so that we will find config.h
include_directories("src" "src/base" "src/scribble" "src/arch/arm64" "${PROJECT_BINARY_DIR}")

# add_compile_options(--save-temps)
add_subdirectory("src")
add_subdirectory("rt")
add_subdirectory("test")

install(TARGETS scribblert testlib sv_test type_test
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib)
install(DIRECTORY share DESTINATION .)
