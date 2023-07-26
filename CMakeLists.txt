cmake_minimum_required(VERSION 3.25)
project(scribble C)

set(CMAKE_C_STANDARD 23)

include_directories(.)

add_executable(
        scribble
        binder.c
        error.c
        graph.c
        io.c
        lexer.c
        log.c
        main.c
        mem.c
        parser.c
        sv.c
        type.c
)
