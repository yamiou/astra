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
  ../../include/SenseKit/host_events.h
  ../../include/SenseKit/Plugins/PluginKit.h
  ../../include/SenseKit/Plugins/plugin_capi.h
  ../../include/SenseKit/Plugins/plugin_callbacks.h
  ../../include/SenseKit/Plugins/PluginBase.h
  ../../include/SenseKit/Plugins/Stream.h
  ../../include/SenseKit/Plugins/StreamCallbackListener.h
  ../../include/SenseKit/Plugins/SingleBinStream.h
  ../../include/SenseKit/Plugins/StreamBin.h
  ../../include/SenseKit/Plugins/MultiBinStream.h
  ../../include/SenseKit/Plugins/PluginServiceProxy.h
  ../../include/SenseKit/Plugins/PluginServiceProxyBase.h
  ../../include/SenseKit/Plugins/StreamServiceProxy.h
  ../../include/SenseKit/Plugins/StreamServiceProxyBase.h
  SenseKitContext.h
  SenseKitContext.cpp
  SenseKitContextImpl.h
  SenseKitContextImpl.cpp
  CreateStreamProxy.h
  CreatePluginProxy.h
  Configuration.h
  Configuration.cpp
  SenseKit.cpp
  Stream.h
  Stream.cpp
  StreamBackend.h
  StreamBackend.cpp
  PluginService.cpp
  PluginService.h
  PluginServiceDelegate.h
  PluginManager.h
  PluginManager.cpp
  ParameterBin.h
  StreamServiceDelegate.h
  StreamSetCatalog.h
  StreamSetCatalog.cpp
  StreamSet.cpp
  StreamSet.h
  StreamSetConnection.h
  StreamSetConnection.cpp
  StreamRegisteredEventArgs.h
  StreamUnregisteringEventArgs.h
  Logger.h
  Logger.cpp
  Logging.h
  Logging.cpp
  StreamConnection.h
  StreamConnection.cpp
  StreamBin.h
  StreamBin.cpp
  StreamReader.h
  StreamReader.cpp
  Core/OSProcesses.h
  Core/OSProcesses.cpp
  Core/shared_library.h
  Core/shared_library_linux.cpp
  Core/shared_library_windows.cpp
  Registry.h
  Registry.cpp
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
  ../../include/SenseKit/Plugins/StreamCallbackListener.h.lpp
  ../../include/SenseKit/Plugins/plugin_callbacks.h.lpp
  )

source_group(templates FILES ${${_projname}_SOURCES_GEN})

add_definitions(-DSENSEKIT_BUILD)

if (ANDROID)
  add_definitions(-DSENSEKIT_ANDROID)
endif()

add_library(${_projname} SHARED ${${_projname}_SOURCES} ${${_projname}_SOURCES_GEN})

include_directories(${PROJECT_SOURCE_DIR}/src/SenseKitAPI)

target_link_libraries(${_projname} SenseKitAPI)

add_subdirectory(Core/tests)

install(TARGETS ${_projname}
        RUNTIME DESTINATION bin/
        LIBRARY DESTINATION lib/
        ARCHIVE DESTINATION lib/
        )

install_pdb(${_projname} bin/)
