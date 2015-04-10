cmake_minimum_required (VERSION 3.1)

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

add_subdirectory(src)
add_subdirectory(examples)