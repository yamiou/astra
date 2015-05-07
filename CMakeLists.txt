cmake_minimum_required (VERSION 3.2)

project (SenseKit)

# Version
string (TIMESTAMP BUILD_DATE "%Y%m%dT%H%M%SZ" UTC)
set (SenseKit_VERSION_MAJOR 0)
set (SenseKit_VERSION_MINOR 1)
set (SenseKit_VERSION_PATCH 6)
set (SenseKit_VERSION_BUILD 0)

if (NOT WIN32)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
endif()

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake")
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

find_package(CLISP REQUIRED)

#Override FindSenseKit equivalents
set(SENSEKIT_SDK_BUILD TRUE)
set(SENSEKIT_LIBRARIES
  SenseKit
  SenseKitAPI
  SenseKitUL
  )
set(SENSEKIT_INCLUDE_DIR
  ${PROJECT_SOURCE_DIR}/include
  )

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

set(SAMPLE_DIR_FOLDER "samples/")

set (SDK_DEPENDENT_TARGET "SDK_dependent")
add_custom_target(${SDK_DEPENDENT_TARGET})

add_subdirectory(src)
add_subdirectory(samples)

set(CMAKE_INSTALL_PREFIX "${PROJECT_BINARY_DIR}/sdk")
MESSAGE("CMAKE_INSTALL_PREFIX : ${CMAKE_INSTALL_PREFIX}")

install(DIRECTORY ${PROJECT_SOURCE_DIR}/include
  DESTINATION .
  FILES_MATCHING PATTERN "*.h"
#  PATTERN "include/SenseKit/Plugins" EXCLUDE
  PATTERN "include/SenseKitUL/Plugins" EXCLUDE
  )

install(DIRECTORY ${PROJECT_SOURCE_DIR}/samples
  DESTINATION .
  PATTERN "build" EXCLUDE
  PATTERN ".gitignore" EXCLUDE
  )

install(DIRECTORY ${PROJECT_SOURCE_DIR}/cmake
  DESTINATION samples
  PATTERN "build" EXCLUDE
  PATTERN ".gitignore" EXCLUDE
  )

if (MSVC)

    install(DIRECTORY ${PROJECT_SOURCE_DIR}/samples-aux/vs2013
        DESTINATION samples
        PATTERN "build" EXCLUDE
        PATTERN ".gitignore" EXCLUDE
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

    install(CODE "file(REMOVE_RECURSE ${PROJECT_BINARY_DIR}/temp/)")
else()
    install(FILES ${PROJECT_SOURCE_DIR}/samples-aux/README.txt
      DESTINATION .
      )
endif()

set(CPACK_PACKAGE_NAME "SenseKitSDK")
set(CPACK_PACKAGE_VENDOR "Orbbec")
set(CPACK_PACKAGE_DIRECTORY "${PROJECT_BINARY_DIR}/sdk_package")
set(CPACK_PACKAGE_VERSION_MAJOR ${SenseKit_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${SenseKit_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH "${SenseKit_VERSION_PATCH}-${BUILD_DATE}")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Orbbec Sensekit SDK")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "Orbbec/SenseKit")

if (WIN32)
  set(CPACK_GENERATOR "ZIP")
else()
  set(CPACK_GENERATOR "ZIP;TGZ")
endif()
INCLUDE(CPack)
