
include_directories(.)

enable_language(ASM-ATT)
add_library(
        scribblert
        STATIC
        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/alloc.s
        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/endln.s
        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/open.s
        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/puthex.s
        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/putint.s
        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/puts.s
        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/putln.s
        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/start.s
        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/to_string.s
        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/trampoline.s
)

install(TARGETS scribblert
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib)
