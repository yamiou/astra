set (SimpleViewer_SOURCES main.cpp)

add_executable(SimpleViewer ${SimpleViewer_SOURCES})

target_link_libraries(SimpleViewer SenseKit)
