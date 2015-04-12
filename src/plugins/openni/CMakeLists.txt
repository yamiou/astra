set(OpenNIPlugin_SOURCES
  OpenNIPlugin.cpp
  OpenNIPlugin.h
  )

add_definitions(-DSENSEKIT_BUILD)

if (WIN32)
  find_library(OpenNI2_LIBRARY NAMES "OpenNI2.lib" HINTS "$ENV{OPENNI2_LIB}" PATH_SUFFIXES lib)
elseif (APPLE)
  find_library(OpenNI2_LIBRARY NAMES "libOpenNI2.dylib" HINTS "$ENV{OPENNI2_REDIST}" PATH_SUFFIXES lib)
endif()

include_directories ("$ENV{OPENNI2_INCLUDE}")

add_library(OpenNIPlugin SHARED ${OpenNIPlugin_SOURCES})

# force c++11 (has no effect currently)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD 11)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD_REQUIRED ON)

target_link_libraries(OpenNIPlugin SenseKitAPI ${OpenNI2_LIBRARY})

install(TARGETS OpenNIPlugin
        RUNTIME DESTINATION bin/Plugins/
        LIBRARY DESTINATION bin/Plugins/
        )