set (_projname "SenseKit")

set(${_projname}_SOURCES
  ../../include/SenseKit/SenseKit.h
  ../../include/SenseKit/Sensor.h
  ../../include/SenseKit/StreamDescription.h
  ../../include/SenseKit/Frame.h
  ../../include/SenseKit/FrameReadyListener.h
  ../../include/SenseKit/StreamReader.h
  ../../include/SenseKit/DataStream.h
  ../../include/SenseKit/sensekit_capi.h
  ../../include/SenseKit/sensekit_defines.h
  ../../include/SenseKit/sensekit_types.h
  ../../include/SenseKit/Plugins/plugin_api.h
  ../../include/SenseKit/Plugins/PluginBase.h
  ../../include/SenseKit/Plugins/Stream.h
  ../../include/SenseKit/Plugins/StreamCallbackListener.h
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
  StreamBackend.h
  StreamBackend.cpp
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

set(${_projname}_SOURCES_GEN
  SenseKitContext.h.lpp
  StreamServiceDelegate.h.lpp
  SenseKit.cpp.lpp
  PluginService.h.lpp
  PluginServiceDelegate.h.lpp
  CreateStreamProxy.h.lpp
  CreatePluginProxy.h.lpp
  ../../include/SenseKit/sensekit_capi.h.lpp
  ../../include/SenseKit/Plugins/StreamServiceProxyBase.h.lpp
  ../../include/SenseKit/Plugins/StreamServiceProxy.h.lpp
  ../../include/SenseKit/Plugins/PluginServiceProxyBase.h.lpp
  ../../include/SenseKit/Plugins/PluginServiceProxy.h.lpp
  )

include_directories(${PROJECT_SOURCE_DIR}/src/SenseKitAPI)

source_group(templates FILES ${${_projname}_SOURCES_GEN})

add_definitions(-DSENSEKIT_BUILD)

add_library(${_projname} SHARED ${${_projname}_SOURCES} ${${_projname}_SOURCES_GEN})

target_link_libraries(${_projname} SenseKitAPI)

add_subdirectory(Core/tests)

install(TARGETS ${_projname}
        RUNTIME DESTINATION bin/
        LIBRARY DESTINATION bin/
        ARCHIVE DESTINATION lib/
        )

install_pdb(${_projname} bin/)
