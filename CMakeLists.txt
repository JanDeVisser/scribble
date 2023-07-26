cmake_minimum_required(VERSION 3.25)
project(scribble C)

set(CMAKE_C_STANDARD 23)

include_directories(src)

add_executable(
        scribble
        src/binder.c
        src/error.c
        src/graph.c
        src/io.c
        src/lexer.c
        src/log.c
        src/main.c
        src/mem.c
        src/parser.c
        src/sv.c
        src/type.c
)
