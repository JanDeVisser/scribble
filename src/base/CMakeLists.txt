include_directories(.)

add_library(
        scb_base
        STATIC
        da.h
        error_or.c
        fn.c
        hash.c
        io.c
        log.c
        mem.c
        mutex.c
        resolve.c
        process.c
        options.c
        sl.c
        sv.c
        threadonce.h
)

add_executable(
        sv_test
        sv.c
)

target_link_libraries(sv_test scb_base)
target_compile_definitions(sv_test PUBLIC SV_TEST)
