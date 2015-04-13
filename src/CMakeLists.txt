# add subdirs

set(include_PATHS
  ${PROJECT_SOURCE_DIR}/include
  ${PROJECT_SOURCE_DIR}/include/SenseKit
  ${PROJECT_SOURCE_DIR}/include/SenseKitUL
  )
include_directories ("${include_PATHS}")

set(CMAKE_DEBUG_POSTFIX "-d")

include(InstallPDB)

add_subdirectory(SenseKit)
add_subdirectory(SenseKitUL)
add_subdirectory(SenseKitAPI)
add_subdirectory(plugins)
