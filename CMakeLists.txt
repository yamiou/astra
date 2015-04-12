cmake_minimum_required (VERSION 3.2)

project (SenseKit)

# Version
set (SenseKit_VERSION_MAJOR 0)
set (SenseKit_VERSION_MINOR 1)
set (SenseKit_VERSION_PATCH 0)
set (SenseKit_VERSION_BUILD 1)

if (NOT WIN32)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
endif()

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

add_subdirectory(src)
add_subdirectory(examples)

set(CMAKE_INSTALL_PREFIX "${PROJECT_BINARY_DIR}/sdk")
MESSAGE("CMAKE_INSTALL_PREFIX : ${CMAKE_INSTALL_PREFIX}")

install(DIRECTORY ${PROJECT_SOURCE_DIR}/include
        DESTINATION .
        FILES_MATCHING PATTERN "*.h"
        PATTERN "include/SenseKit/Plugins" EXCLUDE
        )

install(DIRECTORY ${PROJECT_SOURCE_DIR}/examples
        DESTINATION .
        PATTERN "build" EXCLUDE
        PATTERN ".gitignore" EXCLUDE
        )

set(CPACK_PACKAGE_NAME "SenseKitSDK")
set(CPACK_PACKAGE_VENDOR "Orbbec")
set(CPACK_PACKAGE_DIRECTORY "${PROJECT_BINARY_DIR}/sdk_package")
set(CPACK_PACKAGE_VERSION_MAJOR ${SenseKit_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${SenseKit_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${SenseKit_VERSION_PATCH})
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Orbbec Sensekit SDK")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "Orbbec/SenseKit")

if (WIN32)
    set(CPACK_GENERATOR "ZIP")
else()
    set(CPACK_GENERATOR "ZIP;TGZ")
endif()
INCLUDE(CPack)
