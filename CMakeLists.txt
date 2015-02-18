cmake_minimum_required (VERSION 3.1)

project (SenseKit)

# Version
set (SenseKit_VERSION_MAJOR 0)
set (SenseKit_VERSION_MINOR 1)
set (SenseKit_VERSION_PATCH 0)
set (SenseKit_VERSION_BUILD 1)

include_directories ("${PROJECT_SOURCE_DIR}/include")
include_directories ("$ENV{OPENNI2_INCLUDE}")

find_library(OpenNI2_LIBRARY NAMES "libOpenNI2.dylib" HINTS "$ENV{OPENNI2_REDIST}" PATH_SUFFIXES lib)
add_subdirectory(src)

