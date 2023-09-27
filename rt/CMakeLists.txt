
include_directories(.)

enable_language(ASM-ATT)
add_library(
        scribblert
        STATIC
        arch/arm64/alloc.s
        arch/arm64/endln.s
        arch/arm64/open.s
        arch/arm64/puthex.s
        arch/arm64/putint.s
        arch/arm64/puts.s
        arch/arm64/putln.s
        arch/arm64/start.s
        arch/arm64/to_string.s
        arch/arm64/trampoline.s
)

install(TARGETS scribblert
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib)
