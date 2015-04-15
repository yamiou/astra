# add subdirs

set(include_PATHS
  ${PROJECT_SOURCE_DIR}/include
  )

include_directories ("${include_PATHS}")

set(CMAKE_DEBUG_POSTFIX "-d")

include(InstallPDB)

add_custom_target(ProxyPreprocessor ALL
  COMMENT "=== LPP ==="
  COMMAND ${CLISP_EXECUTABLE} "lpp.cl" "../include"
  WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}/lpp/"
  COMMAND ${CLISP_EXECUTABLE} "lpp.cl" "../src"
  WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}/lpp/")

add_subdirectory(SenseKit)
add_subdirectory(SenseKitUL)
add_subdirectory(SenseKitAPI)
add_subdirectory(plugins)

add_dependencies(SenseKit ProxyPreprocessor)
add_dependencies(SenseKitUL ProxyPreprocessor)
add_dependencies(SenseKitAPI ProxyPreprocessor)
add_dependencies(DummyPlugin ProxyPreprocessor)
add_dependencies(OpenNIPlugin ProxyPreprocessor)
add_dependencies(orbbec_hands ProxyPreprocessor)
