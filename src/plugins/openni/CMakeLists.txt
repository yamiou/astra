set(OpenNIPlugin_SOURCES
  OpenNIPlugin.cpp
  OpenNIPlugin.h
  )

add_definitions(-DSENSEKIT_BUILD)

add_library(OpenNIPlugin SHARED ${OpenNIPlugin_SOURCES})

# force c++11 (has no effect currently)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD 11)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD_REQUIRED ON)

target_link_libraries(OpenNIPlugin SenseKitAPI ${OpenNI2_LIBRARY})
