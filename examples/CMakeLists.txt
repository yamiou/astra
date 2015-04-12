# add examples
cmake_minimum_required(VERSION 3.2)
project(sensekit-samples)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake")
find_package(SenseKit REQUIRED)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

add_subdirectory(SimpleViewer)
add_subdirectory(SimpleColorViewer)
add_subdirectory(DepthReader)
add_subdirectory(DepthReaderEvent)
add_subdirectory(HandReader)
add_subdirectory(cpp/DepthReader)
add_subdirectory(sfml)
