set(SenseKitUL_SOURCES
  ../../include/SenseKitUL/SenseKitUL.h
  ../../include/SenseKitUL/skul_capi.h
  ../../include/SenseKitUL/StreamTypes.h
  ../../include/SenseKitUL/streams/depth.h
  ../../include/SenseKitUL/streams/depth_capi.h
  ../../include/SenseKitUL/streams/depth_types.h
  ../../include/SenseKitUL/streams/color.h
  ../../include/SenseKitUL/streams/color_capi.h
  ../../include/SenseKitUL/streams/color_types.h
  ../../include/SenseKitUL/streams/hand_types.h
  ../../include/SenseKitUL/streams/hand_capi.h
  depthstream_capi.cpp
  colorstream_capi.cpp
  hand_capi.cpp
  SenseKitUL.cpp
  generic_stream_api.h
  SenseKitUL_internal.h
  )

add_library(SenseKitUL SHARED ${SenseKitUL_SOURCES})

add_definitions(-DSENSEKIT_BUILD_EX)

# force c++11 (has no effect currently)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD 11)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD_REQUIRED ON)

target_link_libraries(SenseKitUL SenseKitAPI)

install(TARGETS SenseKitUL
        RUNTIME DESTINATION ${PROJECT_BINARY_DIR}/sdk/bin/
        LIBRARY DESTINATION ${PROJECT_BINARY_DIR}/sdk/bin/
        ARCHIVE DESTINATION ${PROJECT_BINARY_DIR}/sdk/lib/
        )