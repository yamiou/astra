set(SenseKit_SOURCES
  ../../include/SenseKit/SenseKit.h
  ../../include/SenseKit/sensekit_core.h
  ../../include/SenseKit/sensekit_types.h
  ../../include/SenseKit/Plugins/sensekit_plugin_types.h
  ../../include/SenseKit/sensekit_capi.h
  SenseKitContext.h
  SenseKitContext.cpp
  SenseKit.cpp
  Stream.h
  Stream.cpp
  PluginService.cpp
  PluginService.h
  StreamSet.cpp
  StreamSet.h
  StreamConnection.h
  StreamConnection.cpp
  StreamBin.h
  StreamBin.cpp
  Core/shared_library.h
  Core/shared_library_linux.cpp
  Core/shared_library_windows.cpp
  )

set(OpenNIPlugin_SOURCES
  plugins/OpenNIPlugin.cpp
  plugins/OpenNIPlugin.h
  )

set (DummyPlugin_SOURCES
  plugins/DummyPlugin.h
  plugins/DummyPlugin.cpp
  )

add_library(SenseKit SHARED ${SenseKit_SOURCES})
add_library(OpenNIPlugin SHARED ${OpenNIPlugin_SOURCES})
add_library(DummyPlugin SHARED ${DummyPlugin_SOURCES})

# force c++11 (has no effect currently)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD 11)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD_REQUIRED ON)

target_link_libraries(OpenNIPlugin ${OpenNI2_LIBRARY})
target_link_libraries(DummyPlugin SenseKitUL)

add_subdirectory(Core/tests)

add_executable(OpenNITest "openni/test.cpp")

target_link_libraries(OpenNITest ${OpenNI2_LIBRARY})

