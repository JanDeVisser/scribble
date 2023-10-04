include_directories(.)

add_executable(
        scribble
        binder.c
        datum.c
        error_or.c
        execute.c
        graph.c
        hash.c
        intermediate.c
        io.c
        lexer.c
        log.c
        main.c
        mem.c
        mutex.c
        native.c
        parser.c
        process.c
        options.c
        sl.c
        sv.c
        threadonce.h
        type.c

        arch/arm64/arm64.c
        arch/arm64/assembly.c
        arch/arm64/code.c
        arch/arm64/context.c
        arch/arm64/function.c
        arch/arm64/generate.c
        arch/arm64/variable.c
)

target_link_libraries(scribble scribblert)

add_executable(
        sv_test
        log.c
        mem.c
        sl.c
        sv.c
)

target_compile_definitions(sv_test PUBLIC SV_TEST)

add_library(
        testlib
        SHARED
        testlib.c
)
