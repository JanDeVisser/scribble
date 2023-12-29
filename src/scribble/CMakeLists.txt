add_library(
        scb_impl
        STATIC
        binder.c
        datum.c
        execute.c
        graph.c
        intermediate.c
        ir.c
        lexer.c
        native.c
        parser.c
        type.c
        op.h
        engine.c
)

add_executable(
        scribble
        main.c
)

target_link_libraries(scribble scb_impl scb_base scb_arm64 scribblert)

add_library(
        testlib
        SHARED
        testlib.c
)

add_executable(
        type_test
        type.c
)

target_link_libraries(type_test scb_base scb_impl)
target_compile_definitions(type_test PUBLIC TYPE_TEST)
