# add subdirs

include_directories(${ASTRA_INCLUDE_DIR})

set(CMAKE_DEBUG_POSTFIX "-d")

include(InstallPDB)

add_subdirectory(Astra)
add_subdirectory(AstraUL)
add_subdirectory(AstraAPI)
add_subdirectory(plugins)

if (NOT ASTRA_ANDROID)
  add_subdirectory(common)
endif()

if (ASTRA_ANDROID)
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

add_dependencies(Astra ${_projname})
add_dependencies(AstraUL ${_projname})
add_dependencies(AstraAPI ${_projname})
add_dependencies(openni_sensor ${_projname})

add_dependencies(${SDK_DEPENDENT_TARGET} Astra)
add_dependencies(${SDK_DEPENDENT_TARGET} AstraUL)
add_dependencies(${SDK_DEPENDENT_TARGET} AstraAPI)
add_dependencies(${SDK_DEPENDENT_TARGET} openni_sensor)

if (ASTRA_XS AND ASTRA_HAND)
  add_dependencies(${SDK_DEPENDENT_TARGET} orbbec_hand)
  add_dependencies(orbbec_hand ${_projname})
endif()

if (ASTRA_XS)
  add_dependencies(${SDK_DEPENDENT_TARGET} orbbec_xs)
  add_dependencies(orbbec_xs ${_projname})
endif()

if (NOT ASTRA_ANDROID)
  if (ASTRA_MOCK_DEVICE)
    add_dependencies(${SDK_DEPENDENT_TARGET} mock_sensor)
  endif()

  add_dependencies(${SDK_DEPENDENT_TARGET} copytoml_astra)

  if (ASTRA_HAND)
    add_dependencies(${SDK_DEPENDENT_TARGET} copytoml_hand)
  endif()

  add_dependencies(${SDK_DEPENDENT_TARGET} FrameSerialization)
endif()
