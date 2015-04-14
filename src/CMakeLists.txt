# add subdirs

set(include_PATHS
  ${PROJECT_SOURCE_DIR}/include
  )

include_directories ("${include_PATHS}")

set(CMAKE_DEBUG_POSTFIX "-d")

include(InstallPDB)

add_custom_target(ProxyPreprocessor ALL
  COMMAND ${CLISP_EXECUTABLE} "lpp.cl" "../include"
  WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}/lpp/"
  COMMAND ${CLISP_EXECUTABLE} "lpp.cl" "../src"
  WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}/lpp/")

add_subdirectory(SenseKit)
add_subdirectory(SenseKitUL)
add_subdirectory(SenseKitAPI)
add_subdirectory(plugins)
