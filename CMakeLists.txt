cmake_minimum_required (VERSION 3.2)

project (SenseKit)

# Version
set (SenseKit_VERSION_MAJOR 0)
set (SenseKit_VERSION_MINOR 1)
set (SenseKit_VERSION_PATCH 0)
set (SenseKit_VERSION_BUILD 1)

if (NOT WIN32)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
endif()

#Override FindSenseKit equivalents
set(SENSEKIT_SDK_BUILD TRUE)
set(SENSEKIT_LIBRARIES
        SenseKit
        SenseKitAPI
        SenseKitUL
        )
set(SENSEKIT_INCLUDE_DIR
    ${PROJECT_SOURCE_DIR}/include
    )

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

add_subdirectory(src)
add_subdirectory(examples)

install(DIRECTORY ${PROJECT_SOURCE_DIR}/include
        DESTINATION ${PROJECT_BINARY_DIR}/sdk
        FILES_MATCHING PATTERN "*.h"
        PATTERN "include/SenseKit/Plugins" EXCLUDE
        )

install(DIRECTORY ${PROJECT_SOURCE_DIR}/examples
        DESTINATION ${PROJECT_BINARY_DIR}/sdk
        PATTERN "build" EXCLUDE
        PATTERN ".gitignore" EXCLUDE
        )
