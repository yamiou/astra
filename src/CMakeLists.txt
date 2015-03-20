set(SenseKit_SOURCES
  SenseKitContext.h
  SenseKitContext.cpp
  ../include/SenseKit.h
  ../include/ClientDepth.h
  ../include/DepthTypes.h
  SenseKit.cpp
  Stream.h
  Stream.cpp
  PluginBase.cpp
  PluginBase.h
  PluginService.cpp
  PluginService.h
  OpenNIPlugin.cpp
  OpenNIPlugin.h
  StreamSetFactory.h
  StreamSet.cpp
  StreamSet.h
  StreamConnection.h
  StreamConnection.cpp
  StreamBin.h
  StreamBin.cpp
  )

add_library(SenseKit SHARED ${SenseKit_SOURCES})

# force c++11 (has no effect currently)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD 11)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD_REQUIRED ON)

target_link_libraries(SenseKit ${OpenNI2_LIBRARY})

install(TARGETS SenseKit
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)

add_subdirectory(tests)

add_executable(OpenNITest "openni/test.cpp")

target_link_libraries(OpenNITest ${OpenNI2_LIBRARY})
