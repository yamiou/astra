cmake_minimum_required (VERSION 3.2)

project (Astra)

if (ANDROID)
  set(ASTRA_ANDROID TRUE)
endif()

# Version
string (TIMESTAMP BUILD_DATE "%Y%m%dT%H%M%SZ" UTC)
set (Astra_VERSION_MAJOR 0)
set (Astra_VERSION_MINOR 2)
set (Astra_VERSION_PATCH 2)
set (Astra_VERSION_BUILD 0)

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
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -Wall")

  if(ASTRA_ANDROID)
    set(ANDROID_CXX_FLAGS "${ANDROID_CXX_FLAGS} -std=c++14 -Wall")
  endif()

endif()

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake")
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

if(ASTRA_ANDROID)
  find_host_package(CLISP REQUIRED)
else()
  find_package(CLISP REQUIRED)
endif()

#Override FindAstra equivalents
set(ASTRA_SDK_BUILD TRUE)
set(ASTRA_LIBRARIES
  Astra
  AstraAPI
  AstraUL
  )
set(ASTRA_INCLUDE_DIR
  ${PROJECT_SOURCE_DIR}/include
  )

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

set(SAMPLE_DIR_FOLDER "samples/")

set (SDK_DEPENDENT_TARGET "SDK_dependent")
add_custom_target(${SDK_DEPENDENT_TARGET})

set(SHINY_INCLUDE "${PROJECT_SOURCE_DIR}/vendor/shinyprofiler/include")
set(CATCH_INCLUDE_DIR "${PROJECT_SOURCE_DIR}/vendor/catch/include" CACHE INTERNAL "Path to include folder for Catch")

add_subdirectory(vendor)
add_subdirectory(src)
add_subdirectory(samples)
add_subdirectory(tests)

set(CMAKE_INSTALL_PREFIX "${PROJECT_BINARY_DIR}/sdk")
MESSAGE("CMAKE_INSTALL_PREFIX : ${CMAKE_INSTALL_PREFIX}")

install(DIRECTORY ${PROJECT_SOURCE_DIR}/include
  DESTINATION .
  FILES_MATCHING PATTERN "*.h"
  PATTERN "include/AstraUL/Plugins" EXCLUDE
  )

install(DIRECTORY ${PROJECT_SOURCE_DIR}/samples
  DESTINATION .
  PATTERN "build" EXCLUDE
  PATTERN ".gitignore" EXCLUDE
  PATTERN "samples/plugins/orbbec_skeleton" EXCLUDE
  PATTERN "samples/sfml/SimpleSkeletonViewer-SFML" EXCLUDE
  PATTERN "samples/tools/StreamRecorder" EXCLUDE
  PATTERN "samples/gl" EXCLUDE
  )

install(DIRECTORY ${PROJECT_SOURCE_DIR}/cmake
  DESTINATION samples
  PATTERN "build" EXCLUDE
  PATTERN ".gitignore" EXCLUDE
  )

install(DIRECTORY ${PROJECT_SOURCE_DIR}/android/
  DESTINATION android
  USE_SOURCE_PERMISSIONS
  PATTERN ".gitignore" EXCLUDE
  PATTERN ".git" EXCLUDE
  PATTERN ".gitmodules" EXCLUDE
  )

if (MSVC)
  install(DIRECTORY ${PROJECT_SOURCE_DIR}/samples-aux/vs2013
    DESTINATION samples
    PATTERN "build" EXCLUDE
    PATTERN ".gitignore" EXCLUDE
    PATTERN "thirdparty/.git" EXCLUDE
    PATTERN "thirdparty/glut_windows" EXCLUDE
    PATTERN "thirdparty/copy_to_bin_dir/glut*.dll" EXCLUDE
    )

  file(GLOB_RECURSE THIRD_PARTY_FILES "${PROJECT_SOURCE_DIR}/samples-aux/vs2013/thirdparty/copy_to_bin_dir/*.*")
  install(DIRECTORY ${PROJECT_SOURCE_DIR}/samples-aux/vs2013/thirdparty/copy_to_bin_dir/
    DESTINATION bin
    PATTERN "glut*.dll" EXCLUDE
    )

  set(SAMPLES_SOURCE_DIR "${PROJECT_BINARY_DIR}/sdk/samples")
  set(SAMPLES_BUILD_TEMP_DIR "${PROJECT_BINARY_DIR}/temp/vs2013/")

  configure_file("${PROJECT_SOURCE_DIR}/samples-aux/README.txt"
    "${PROJECT_BINARY_DIR}/temp/README.txt"
    @ONLY
    NEWLINE_STYLE CRLF)

  install(FILES ${PROJECT_BINARY_DIR}/temp/README.txt
    DESTINATION .
    )

  configure_file("${PROJECT_SOURCE_DIR}/InstallVS.txt.in"
    "${PROJECT_SOURCE_DIR}/InstallVS.txt"
    @ONLY)
  install(SCRIPT "InstallVS.txt")

  install(DIRECTORY ${SAMPLES_BUILD_TEMP_DIR}
    DESTINATION samples/vs2013
    PATTERN "build" EXCLUDE
    PATTERN ".gitignore" EXCLUDE
    )

  install(CODE "file(REMOVE_RECURSE ${SAMPLES_BUILD_TEMP_DIR})")
else()
  install(FILES ${PROJECT_SOURCE_DIR}/samples-aux/README.txt
    DESTINATION .
    )
endif()

set(CPACK_PACKAGE_NAME "AstraSDK")
set(CPACK_PACKAGE_VENDOR "Orbbec")
set(CPACK_PACKAGE_DIRECTORY "${PROJECT_BINARY_DIR}/sdk_package")
set(CPACK_PACKAGE_VERSION_MAJOR ${Astra_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${Astra_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH "${Astra_VERSION_PATCH}-${BUILD_DATE}")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Orbbec Astra SDK")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "Orbbec/Astra")

if (WIN32)
  set(CPACK_GENERATOR "ZIP")
else()
  set(CPACK_GENERATOR "ZIP;TGZ")
endif()

INCLUDE(CPack)
