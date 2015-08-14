# add examples
cmake_minimum_required(VERSION 3.2)
project(astra-samples)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake")
find_package(Astra REQUIRED)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

include(ConfigUserFile)

include_directories(${ASTRA_INCLUDE_DIR})

get_filename_component(SAMPLE_COMMON_DIR "./common" ABSOLUTE)

include_directories(${SAMPLE_COMMON_DIR})

add_subdirectory(c-api)
add_subdirectory(cpp-api)
if(NOT ASTRA_ANDROID)
  if(ASTRA_SDK_BUILD)
    add_subdirectory(gl)
  endif()
  add_subdirectory(sfml)
  add_subdirectory(tools)
  add_subdirectory(plugins)
  if (ASTRA_SKELETON)
      add_dependencies(SimpleSkeletonViewer-SFML orbbec_skeleton)
  endif()
endif()
