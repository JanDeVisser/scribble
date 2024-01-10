add_library(
        scb_impl
        STATIC
        backend.c
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
)

add_executable(
        scribble
        engine.h
        frontend.c
)

target_link_libraries(scribble scb_base)

add_executable(
        scribble-backend
        backend_main.c
        engine.h
)

target_link_libraries(scribble-backend scb_impl scb_base scb_arm64 scribblert)

add_executable(
        scribble-threaded
        engine.h
        frontend.c
)

target_link_libraries(scribble-threaded scb_impl scb_base scb_arm64 scribblert)
target_compile_definitions(scribble-threaded PUBLIC SCRIBBLE_THREADED_BACKEND)

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

install(TARGETS scribble scribble-backend scb_base testlib
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib)
