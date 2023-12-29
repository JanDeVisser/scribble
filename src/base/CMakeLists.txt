include_directories(.)

add_library(
        scb_base
        STATIC
        da.h
        error_or.c
        fmt.c
        fn.c
        fs.c
        hash.c
        http.c
        integer.c
        io.c
        json.c
        log.c
        mem.c
        mutex.c
        resolve.c
        process.c
        options.c
        sb.c
        sl.c
        ss.c
        sv.c
        threadonce.h
)

add_executable(
        sv_test
        sv.c
)

target_link_libraries(sv_test scb_base)
target_compile_definitions(sv_test PUBLIC SV_TEST)

add_executable(
        fmt_test
        fmt.c
)

target_link_libraries(fmt_test scb_base)
target_compile_definitions(fmt_test PUBLIC FMT_TEST)
