include_directories(.)

add_executable(
        http_server
        http_server.c
)

add_executable(
        http_client
        http_client.c
)

target_link_libraries(http_server scb_base)
target_link_libraries(http_client scb_base)

install(TARGETS http_server http_client
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib)
