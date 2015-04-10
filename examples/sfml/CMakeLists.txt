set (DepthViewer_SOURCES
  main.cpp
  )
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake")
find_package(SFML COMPONENTS graphics window system audio)
add_executable(DepthViewer ${DepthViewer_SOURCES})
include_directories(DepthViewer ${SFML_INCLUDE_DIR})
target_link_libraries(DepthViewer SenseKit SenseKitUL ${SFML_LIBRARIES})
