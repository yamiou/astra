set (_projname "Astra")

set(${_projname}_SOURCES
  ../../include/Astra/Astra.h
  ../../include/Astra/Sensor.h
  ../../include/Astra/StreamDescription.h
  ../../include/Astra/Frame.h
  ../../include/Astra/FrameReadyListener.h
  ../../include/Astra/StreamReader.h
  ../../include/Astra/DataStream.h
  ../../include/Astra/astra_capi.h
  ../../include/Astra/astra_defines.h
  ../../include/Astra/astra_types.h
  ../../include/Astra/host_events.h
  ../../include/Astra/Plugins/PluginKit.h
  ../../include/Astra/Plugins/plugin_capi.h
  ../../include/Astra/Plugins/plugin_callbacks.h
  ../../include/Astra/Plugins/PluginBase.h
  ../../include/Astra/Plugins/Stream.h
  ../../include/Astra/Plugins/StreamCallbackListener.h
  ../../include/Astra/Plugins/SingleBinStream.h
  ../../include/Astra/Plugins/StreamBin.h
  ../../include/Astra/Plugins/MultiBinStream.h
  ../../include/Astra/Plugins/PluginServiceProxy.h
  ../../include/Astra/Plugins/PluginServiceProxyBase.h
  ../../include/Astra/Plugins/StreamServiceProxy.h
  ../../include/Astra/Plugins/StreamServiceProxyBase.h
  AstraContext.h
  AstraContext.cpp
  AstraContextImpl.h
  AstraContextImpl.cpp
  CreateStreamProxy.h
  CreatePluginProxy.h
  Configuration.h
  Configuration.cpp
  Astra.cpp
  Stream.h
  Stream.cpp
  StreamBackend.h
  StreamBackend.cpp
  PluginService.cpp
  PluginService.h
  PluginServiceImpl.h
  PluginServiceImpl.cpp
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
  AstraContext.h.lpp
  StreamServiceDelegate.h.lpp
  Astra.cpp.lpp
  PluginService.h.lpp
  PluginServiceDelegate.h.lpp
  CreateStreamProxy.h.lpp
  CreatePluginProxy.h.lpp
  ../../include/Astra/astra_capi.h.lpp
  ../../include/Astra/Plugins/StreamServiceProxyBase.h.lpp
  ../../include/Astra/Plugins/StreamServiceProxy.h.lpp
  ../../include/Astra/Plugins/PluginServiceProxyBase.h.lpp
  ../../include/Astra/Plugins/PluginServiceProxy.h.lpp
  ../../include/Astra/Plugins/StreamCallbackListener.h.lpp
  ../../include/Astra/Plugins/plugin_callbacks.h.lpp
  )

source_group(templates FILES ${${_projname}_SOURCES_GEN})

add_definitions(-DASTRA_BUILD)

if (ANDROID)
  add_definitions(-DASTRA_ANDROID)
endif()

add_library(${_projname} SHARED ${${_projname}_SOURCES} ${${_projname}_SOURCES_GEN})

include_directories(${PROJECT_SOURCE_DIR}/src/AstraAPI)

target_link_libraries(${_projname} AstraAPI)

add_subdirectory(Core/tests)

install(TARGETS ${_projname}
        RUNTIME DESTINATION bin/
        LIBRARY DESTINATION lib/
        ARCHIVE DESTINATION lib/
        )

install_pdb(${_projname} bin/)
