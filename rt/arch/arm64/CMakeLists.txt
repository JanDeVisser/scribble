
include_directories(.)

enable_language(ASM-ATT)
add_library(
        scribblert
        STATIC
        alloc.s
        puts.s
        putln.s
        start.s
        trampoline.s
)

install(TARGETS scribblert
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib)
