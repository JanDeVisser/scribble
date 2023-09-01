include_directories(.)

add_executable(
        scribble
        binder.c
        datum.c
        error.c
        execute.c
        graph.c
        intermediate.c
        io.c
        lexer.c
        log.c
        main.c
        mem.c
        mutex.c
        native.c
        parser.c
        sv.c
        threadonce.h
        type.c
        testlib.c)

target_link_libraries(scribble scribblert)

add_executable(
        sv_test
        log.c
        mem.c
        sv.c
)

target_compile_definitions(sv_test PUBLIC SV_TEST)

add_library(
        testlib
        SHARED
        testlib.c
)
