set(SenseKitAPI_SOURCES
  ../../include/SenseKitAPI/SenseKitAPI.h
  SenseKitAPI.cpp
  )
  
add_definitions(-DSENSEKIT_BUILD_API_PROXY)
add_definitions(-DSENSEKIT_BUILD)

add_library(SenseKitAPI SHARED ${SenseKitAPI_SOURCES})

# force c++11 (has no effect currently)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD 11)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD_REQUIRED ON)
