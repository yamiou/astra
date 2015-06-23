# add examples
cmake_minimum_required(VERSION 3.2)
project(sensekit-samples)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake")
find_package(SenseKit REQUIRED)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

include(ConfigUserFile)

include_directories(${SENSEKIT_INCLUDE_DIR})

get_filename_component(SAMPLE_COMMON_DIR "./common" ABSOLUTE)

include_directories(${SAMPLE_COMMON_DIR})

function(add_sensekit_sdk_dependency _targetname)
  if(NOT TARGET ${_targetname})
    message("add_sdk_dependency: There is no target named '${_targetname}'")
    return()
  endif()
  if(TARGET ${SDK_DEPENDENT_TARGET})
    add_dependencies(${_targetname} ${SDK_DEPENDENT_TARGET})
  endif()
endfunction()

add_subdirectory(c-api)
add_subdirectory(cpp-api)
if(NOT SENSEKIT_ANDROID)
  add_subdirectory(gl)
  add_subdirectory(sfml)
  add_dependencies(SimpleSkeletonViewer-SFML orbbec_skeleton)
  add_subdirectory(tools)
  add_subdirectory(plugins)
endif()
