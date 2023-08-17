cmake_minimum_required(VERSION 3.25)
project(scribble C)

set(CMAKE_C_STANDARD 23)

include_directories(src)

add_executable(
        scribble
        src/binder.c
        src/datum.c
        src/error.c
        src/execute.c
        src/graph.c
        src/intermediate.c
        src/io.c
        src/lexer.c
        src/log.c
        src/main.c
        src/mem.c
        src/parser.c
        src/sv.c
        src/type.c
)

add_executable(
        sv_test
        src/log.c
        src/mem.c
        src/sv.c
)

target_compile_definitions(sv_test PUBLIC SV_TEST)

add_subdirectory("rt/arch/arm64")
