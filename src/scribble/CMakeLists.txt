add_executable(
        scribble
        binder.c
        datum.c
        execute.c
        graph.c
        intermediate.c
        ir.c
        lexer.c
        main.c
        native.c
        parser.c
        type.c
        op.h
)

target_link_libraries(scribble scb_base scb_arm64 scribblert)

add_library(
        testlib
        SHARED
        testlib.c
)
