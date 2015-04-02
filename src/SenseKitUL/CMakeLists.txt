set(SenseKitUL_SOURCES
  ../../include/SenseKitUL/SenseKitUL.h
  ../../include/SenseKitUL/streams/Depth.h
  ../../include/SenseKitUL/streams/depth_capi.h
  ../../include/SenseKitUL/streams/depth_types.h
  ../../include/SenseKitUL/streams/color.h
  ../../include/SenseKitUL/streams/color_types.h
  depthstream_capi.cpp
  colorstream_capi.cpp
  SenseKitUL.cpp
  )

add_library(SenseKitUL SHARED ${SenseKitUL_SOURCES})

add_definitions(-DSENSEKIT_BUILD_EX)

# force c++11 (has no effect currently)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD 11)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD_REQUIRED ON)

target_link_libraries(SenseKitUL SenseKitAPI)
