set (_projname "SenseKitUL")

set(${_projname}_SOURCES
  ../../include/SenseKitUL/SenseKitUL.h
  ../../include/SenseKitUL/skul_capi.h
  ../../include/SenseKitUL/StreamTypes.h
  ../../include/SenseKitUL/Vectorx.h
  ../../include/SenseKitUL/Vector2.h
  ../../include/SenseKitUL/Vector3.h
  ../../include/SenseKitUL/cvector.h
  ../../include/SenseKitUL/streams/Depth.h
  ../../include/SenseKitUL/streams/depth_capi.h
  ../../include/SenseKitUL/streams/depth_types.h
  ../../include/SenseKitUL/streams/Color.h
  ../../include/SenseKitUL/streams/color_capi.h
  ../../include/SenseKitUL/streams/color_types.h
  ../../include/SenseKitUL/streams/Hand.h
  ../../include/SenseKitUL/streams/HandDebug.h
  ../../include/SenseKitUL/streams/hand_capi.h
  ../../include/SenseKitUL/streams/hand_types.h
  ../../include/SenseKitUL/streams/image_types.h
  ../../include/SenseKitUL/Plugins/stream_types.h
  depthstream_capi.cpp
  colorstream_capi.cpp
  hand_capi.cpp
  SenseKitUL.cpp
  generic_stream_api.h
  )

add_library(${_projname} SHARED ${${_projname}_SOURCES})

add_definitions(-DSENSEKIT_BUILD_EX)

target_link_libraries(${_projname} SenseKitAPI)

install(TARGETS ${_projname}
        RUNTIME DESTINATION bin/
        LIBRARY DESTINATION bin/
        ARCHIVE DESTINATION lib/
        )

install_pdb(${_projname} bin/)