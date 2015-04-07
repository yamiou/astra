set(SenseKit_SOURCES
  ../../include/SenseKit/SenseKit.h
  ../../include/SenseKit/sensekit_core.h
  ../../include/SenseKit/sensekit_types.h
  ../../include/SenseKit/sensekit_capi.h
  ../../include/SenseKit/Plugins/PluginBase.h
  ../../include/SenseKit/Plugins/PluginServiceProxy.h
  ../../include/SenseKit/Plugins/PluginServiceProxyBase.h
  ../../include/SenseKit/Plugins/StreamServiceProxy.h
  ../../include/SenseKit/Plugins/StreamServiceProxyBase.h
  sensekit_internal.h
  SenseKitContext.h
  SenseKitContext.cpp
  CreateStreamProxy.h
  CreatePluginProxy.h
  SenseKit.cpp
  Stream.h
  Stream.cpp
  StreamImpl.h
  StreamImpl.cpp
  PluginService.cpp
  PluginService.h
  PluginServiceDelegate.h
  StreamServiceDelegate.h
  StreamSet.cpp
  StreamSet.h
  StreamConnection.h
  StreamConnection.cpp
  StreamBin.h
  StreamBin.cpp
  StreamReader.h
  StreamReader.cpp
  Core/shared_library.h
  Core/shared_library_linux.cpp
  Core/shared_library_windows.cpp
  )

set(SenseKit_SOURCES_GEN
  SenseKitContext.h.lpp
  StreamServiceDelegate.h.lpp
  SenseKit.cpp.lpp
  PluginServiceDelegate.h.lpp
  CreateStreamProxy.h.lpp
  CreatePluginProxy.h.lpp
  ../../include/SenseKit/sensekit_capi.h.lpp
  ../../include/SenseKit/Plugins/StreamServiceProxyBase.h.lpp
  ../../include/SenseKit/Plugins/StreamServiceProxy.h.lpp
  ../../include/SenseKit/Plugins/PluginServiceProxyBase.h.lpp
  ../../include/SenseKit/Plugins/PluginServiceProxy.h.lpp
  )

source_group(templates, ${SenseKit_SOURCE_GEN})

add_definitions(-DSENSEKIT_BUILD)

add_library(SenseKit SHARED ${SenseKit_SOURCES} ${SenseKit_SOURCES_GEN})

# force c++11 (has no effect currently)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD 11)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD_REQUIRED ON)

target_link_libraries(SenseKit SenseKitAPI)

add_subdirectory(Core/tests)

add_executable(OpenNITest "openni/test.cpp")

target_link_libraries(OpenNITest ${OpenNI2_LIBRARY})
