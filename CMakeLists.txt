cmake_minimum_required (VERSION 3.2)

project (Astra)

if (ANDROID)
  set(ASTRA_ANDROID TRUE)
elseif (WIN32)
  set(ASTRA_WINDOWS TRUE)
elseif (UNIX)
  set(ASTRA_UNIX TRUE)
endif()

# Version
string (TIMESTAMP BUILD_DATE "%Y%m%dT%H%M%SZ" UTC)
set (Astra_VERSION_MAJOR 0)
set (Astra_VERSION_MINOR 3)
set (Astra_VERSION_PATCH 0)
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

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if(ASTRA_GCC OR ASTRA_CLANG)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -Wall -fPIC")

  if(ASTRA_ANDROID)
    set(ANDROID_CXX_FLAGS "${ANDROID_CXX_FLAGS} -std=c++14 -Wall -fPIC")
  endif()
endif()

message(${CMAKE_CXX_FLAGS})

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

function(install_lib _targetname)
  set(extrapath ${ARGV1})
  install(TARGETS ${_targetname}
    RUNTIME DESTINATION bin/${extrapath}
    LIBRARY DESTINATION lib/${extrapath}
    ARCHIVE DESTINATION lib/${extrapath}
    )
  if (MSVC)
    install_pdb(${_targetname} bin/)
    set(INSTALL_PATHS 
      samples/vs2013/copy_to_bin_dir/${extrapath}
      samples/vs2013/bin/Debug/${extrapath}
      samples/vs2013/bin/Release/${extrapath}
    )
    foreach(INSTALL_PATH ${INSTALL_PATHS})
      install(TARGETS ${_targetname}
        RUNTIME DESTINATION ${INSTALL_PATH}
        )
      install_pdb(${_targetname} ${INSTALL_PATH})
    endforeach()
  endif()
endfunction()

function(install_file _targetfile)
  set(extrapath ${ARGV1})
  if (MSVC)
    set(INSTALL_PATHS 
      bin/${extrapath}
      samples/vs2013/copy_to_bin_dir/${extrapath}
      samples/vs2013/bin/Debug/${extrapath}
      samples/vs2013/bin/Release/${extrapath}
    )
    foreach(INSTALL_PATH ${INSTALL_PATHS})
      install(FILES ${_targetfile}
        DESTINATION ${INSTALL_PATH}
        )
    endforeach()
  else()
    install(FILES ${_targetfile}
      DESTINATION lib/${extrapath}
    )
  endif()
endfunction()

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

if (MSVC)
  #Lack of a trailing / is important here, ends up in samples/vs2013/
  install(DIRECTORY ${PROJECT_SOURCE_DIR}/samples-aux/vs2013/thirdparty
    DESTINATION samples/vs2013/
    PATTERN "build" EXCLUDE
    PATTERN ".gitignore" EXCLUDE
    PATTERN ".git" EXCLUDE
    )

  set(INSTALL_PATHS
    bin/
    samples/vs2013/copy_to_bin_dir/
    samples/vs2013/bin/Debug/
    samples/vs2013/bin/Release/
  )
  foreach(INSTALL_PATH ${INSTALL_PATHS})
   install(DIRECTORY ${PROJECT_SOURCE_DIR}/samples-aux/vs2013/copy_to_bin_dir/
     DESTINATION ${INSTALL_PATH}
     )
  endforeach()

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
