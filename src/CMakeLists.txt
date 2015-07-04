# add subdirs

include_directories(${SENSEKIT_INCLUDE_DIR})

set(CMAKE_DEBUG_POSTFIX "-d")

include(InstallPDB)

add_subdirectory(SenseKit)
add_subdirectory(SenseKitUL)
add_subdirectory(SenseKitAPI)
add_subdirectory(plugins)

if (NOT SENSEKIT_ANDROID)
  add_subdirectory(common)
endif()

if (SENSEKIT_ANDROID)
  add_subdirectory(jni)
  add_subdirectory(android)
endif()

set (_projname "ProxyPreprocessor")

add_custom_target(${_projname} ALL
  COMMENT "=== LPP ==="
  COMMAND ${CLISP_EXECUTABLE} "lpp.cl" "../include"
  WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}/lpp/"
  COMMAND ${CLISP_EXECUTABLE} "lpp.cl" "../src"
  WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}/lpp/")

add_dependencies(SenseKit ${_projname})
add_dependencies(SenseKitUL ${_projname})
add_dependencies(SenseKitAPI ${_projname})
add_dependencies(openni_sensor ${_projname})
add_dependencies(orbbec_hand ${_projname})
add_dependencies(orbbec_xs ${_projname})

if (NOT SENSEKIT_ANDROID)
  add_dependencies(dummy_plugin ${_projname})
endif()

add_dependencies(${SDK_DEPENDENT_TARGET} SenseKit)
add_dependencies(${SDK_DEPENDENT_TARGET} SenseKitUL)
add_dependencies(${SDK_DEPENDENT_TARGET} SenseKitAPI)
add_dependencies(${SDK_DEPENDENT_TARGET} openni_sensor)
add_dependencies(${SDK_DEPENDENT_TARGET} orbbec_hand)
add_dependencies(${SDK_DEPENDENT_TARGET} orbbec_xs)

if (NOT SENSEKIT_ANDROID)
  add_dependencies(${SDK_DEPENDENT_TARGET} dummy_plugin)
  add_dependencies(${SDK_DEPENDENT_TARGET} copytoml)
  add_dependencies(${SDK_DEPENDENT_TARGET} FrameSerialization)
endif()
