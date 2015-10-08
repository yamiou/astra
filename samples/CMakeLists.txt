# add examples
cmake_minimum_required(VERSION 3.2)
project(astra-samples)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake")
find_package(Astra REQUIRED)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

if(CMAKE_SIZEOF_VOID_P EQUAL 8)
  set(ASTRA_64 ON)
else()
  set(ASTRA_32 ON)
endif()

include(ConfigUserFile)

function(add_astra_sdk_dependency _targetname)
  if(NOT TARGET ${_targetname})
    message("add_sdk_dependency: There is no target named '${_targetname}'")
    return()
  endif()
  if(TARGET ${SDK_DEPENDENT_TARGET})
    add_dependencies(${_targetname} ${SDK_DEPENDENT_TARGET})
  endif()
endfunction()

include_directories(${ASTRA_INCLUDE_DIR})

get_filename_component(SAMPLE_COMMON_DIR "./common" ABSOLUTE)

include_directories(${SAMPLE_COMMON_DIR})

if("${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang")
  set(ASTRA_CLANG ON)
elseif("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
  set(ASTRA_GCC ON)
elseif("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel")
  set(ASTRA_INTELC ON)
elseif("${CMAKE_CXX_COMPILER_ID}" STREQUAL "MSVC")
  set(ASTRA_MSVC ON)
endif()

if(ASTRA_GCC OR ASTRA_CLANG)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -Wall -fPIC")

  if(ASTRA_ANDROID)
    set(ANDROID_CXX_FLAGS "${ANDROID_CXX_FLAGS} -std=c++14 -Wall -fPIC")
  endif()
endif()

add_subdirectory(c-api)
add_subdirectory(cpp-api)
if(NOT ASTRA_ANDROID)
  add_subdirectory(sfml)
  add_subdirectory(tools)
  add_subdirectory(plugins)
  if (ASTRA_SKELETON)
      add_dependencies(SimpleSkeletonViewer-SFML orbbec_skeleton)
  endif()
endif()
