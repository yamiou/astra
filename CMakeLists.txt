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

set(include_PATHS
  ${PROJECT_SOURCE_DIR}/include
  ${PROJECT_SOURCE_DIR}/include/SenseKit
  ${PROJECT_SOURCE_DIR}/include/SenseKitUL
  $ENV{OPENNI2_INCLUDE}
  )
include_directories ("${include_PATHS}")

if (WIN32)
  find_library(OpenNI2_LIBRARY NAMES "OpenNI2.lib" HINTS "$ENV{OPENNI2_LIB}" PATH_SUFFIXES lib)
elseif (APPLE)
  find_library(OpenNI2_LIBRARY NAMES "libOpenNI2.dylib" HINTS "$ENV{OPENNI2_REDIST}" PATH_SUFFIXES lib)
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
