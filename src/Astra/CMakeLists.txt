set (_projname "Astra")

set(${_projname}_SOURCES
  ../../include/Astra/Astra.h
  ../../include/Astra/StreamSet.h
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
  astra_context.hpp
  astra_context.cpp
  astra_context_impl.hpp
  astra_context_impl.cpp
  astra_create_stream_proxy.hpp
  astra_create_plugin_proxy.hpp
  astra_configuration.hpp
  astra_configuration.cpp
  astra.cpp
  astra_stream.hpp
  astra_stream.cpp
  astra_stream_backend.hpp
  astra_stream_backend.cpp
  astra_plugin_service.hpp
  astra_plugin_service.cpp
  astra_plugin_service_impl.hpp
  astra_plugin_service_impl.cpp
  astra_plugin_service_delegate.hpp
  astra_plugin_manager.hpp
  astra_plugin_manager.cpp
  astra_parameter_bin.hpp
  astra_stream_service_delegate.hpp
  astra_streamset_catalog.hpp
  astra_streamset_catalog.cpp
  astra_streamset.hpp
  astra_streamset.cpp
  astra_streamset_connection.hpp
  astra_streamset_connection.cpp
  astra_stream_registered_event_args.hpp
  astra_stream_unregistering_event_args.hpp
  astra_logger.hpp
  astra_logger.cpp
  astra_logging.hpp
  astra_logging.cpp
  astra_signal.hpp
  astra_stream_connection.hpp
  astra_stream_connection.cpp
  astra_stream_bin.hpp
  astra_stream_bin.cpp
  astra_stream_reader.hpp
  astra_stream_reader.cpp
  astra_shared_library.hpp
  astra_registry.hpp
  astra_registry.cpp
  astra_filesystem.cpp
  astra_string.cpp
  )

set(${_projname}_ANDROID_SOURCES
  android/astra_environment_android.cpp
  unix/astra_filesystem_unix.cpp
  unix/astra_shared_library_unix.cpp
  )

set(${_projname}_WIN32_SOURCES
  win32/astra_environment_win32.cpp
  win32/astra_filesystem_win32.cpp
  win32/astra_shared_library_win32.cpp
  )

set(${_projname}_UNIX_SOURCES
  unix/astra_environment_unix.cpp
  unix/astra_filesystem_unix.cpp
  unix/astra_shared_library_unix.cpp
  )

if (ASTRA_ANDROID)
  set(${_projname}_SOURCES ${${_projname}_SOURCES} ${${_projname}_ANDROID_SOURCES})
elseif(ASTRA_WINDOWS)
  set(${_projname}_SOURCES ${${_projname}_SOURCES} ${${_projname}_WIN32_SOURCES})
elseif(ASTRA_UNIX)
  set(${_projname}_SOURCES ${${_projname}_SOURCES} ${${_projname}_UNIX_SOURCES})
endif()

set(${_projname}_SOURCES_GEN
  astra_context.hpp.lpp
  astra_context.cpp.lpp
  astra_context_impl.hpp.lpp
  astra_stream_service_delegate.hpp.lpp
  astra.cpp.lpp
  astra_plugin_service.hpp.lpp
  astra_plugin_service_impl.hpp.lpp
  astra_plugin_service_delegate.hpp.lpp
  astra_create_stream_proxy.hpp.lpp
  astra_create_plugin_proxy.hpp.lpp
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

target_link_libraries(${_projname} AstraAPI ${CMAKE_DL_LIBS})

add_subdirectory(tests)

add_custom_target(copytoml_astra ALL
  #astra.toml
  COMMAND ${CMAKE_COMMAND} -E copy
  "${PROJECT_SOURCE_DIR}/src/Astra/astra.toml"
  "$<TARGET_FILE_DIR:${_projname}>")

set_target_properties(copytoml_astra PROPERTIES FOLDER CMakeCopyTargets)

install_lib(${_projname})
install_file("${PROJECT_SOURCE_DIR}/src/astra/astra.toml")