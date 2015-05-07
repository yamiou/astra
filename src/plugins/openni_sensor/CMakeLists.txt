set (_projname "openni_sensor")

set(${_projname}_SOURCES
  OpenNIPlugin.cpp
  OpenNIPlugin.h
  OniDeviceStream.h
  OniDepthStream.h
  OniDepthStream.cpp
  OniColorStream.h
  OniColorStream.cpp
  OniDeviceStreamSet.h
  OniDeviceStreamSet.cpp

  )

add_definitions(-DSENSEKIT_BUILD)

if (WIN32)
  find_library(OpenNI2_LIBRARY NAMES "OpenNI2.lib" HINTS "$ENV{OPENNI2_LIB}" PATH_SUFFIXES lib)
elseif (APPLE)
  find_library(OpenNI2_LIBRARY NAMES "libOpenNI2.dylib" HINTS "$ENV{OPENNI2_REDIST}" PATH_SUFFIXES lib)
elseif (ANDROID)
  find_library(OpenNI2_LIBRARY NAMES "libOpenNI2.so" PATHS "${CMAKE_SOURCE_DIR}/android-lib" NO_CMAKE_FIND_ROOT_PATH)
endif()

include_directories ("$ENV{OPENNI2_INCLUDE}")

add_library(${_projname} SHARED ${${_projname}_SOURCES})

set_target_properties(${_projname} PROPERTIES FOLDER "plugins")

target_link_libraries(${_projname} SenseKitAPI ${OpenNI2_LIBRARY})

install(TARGETS ${_projname}
  RUNTIME DESTINATION bin/Plugins/
  LIBRARY DESTINATION bin/Plugins/
  )

install_pdb(${_projname} bin/Plugins/)
