set(SenseKit_SOURCES
  ../include/SenseKit.h
  ../include/sensekit_core.h
  ../include/sensekit_types.h
  ../include/sensekit_capi.h
  SenseKitContext.h
  SenseKitContext.cpp
  SenseKit.cpp
  Stream.h
  Stream.cpp
  plugins/PluginBase.cpp
  plugins/PluginBase.h
  PluginService.cpp
  PluginService.h
  plugins/OpenNIPlugin.cpp
  plugins/OpenNIPlugin.h
  StreamSetFactory.h
  StreamSet.cpp
  StreamSet.h
  StreamConnection.h
  StreamConnection.cpp
  StreamBin.h
  StreamBin.cpp
  )

set (SenseKitSL_SOURCES
  ../include/SenseKitSL.h
  ../include/streams/depth.h
  ../include/streams/depth_types.h
  ../include/streams/color.h
  ../include/streams/color_types.h
  depthstream_capi.cpp
  colorstream_capi.cpp
  )

add_definitions(-DSENSEKIT_EXPORT)

add_library(SenseKit SHARED ${SenseKit_SOURCES})
add_library(SenseKitSL SHARED ${SenseKitSL_SOURCES})

# force c++11 (has no effect currently)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD 11)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD_REQUIRED ON)

target_link_libraries(SenseKit ${OpenNI2_LIBRARY})
target_link_libraries(SenseKitSL SenseKit)

install(TARGETS SenseKit
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)

install(TARGETS SenseKitSL
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)

add_subdirectory(tests)

add_executable(OpenNITest "openni/test.cpp")

target_link_libraries(OpenNITest ${OpenNI2_LIBRARY})
