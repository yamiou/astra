set(SenseKit_SOURCES
  Context.h
  Context.cpp
  MockContext.h
  MockContext.cpp
  StreamSourceRegistry.h
  StreamSourceRegistry.cpp
  StreamServerRegistry.h
  StreamServerRegistry.cpp
  StreamServer.h
  StreamServer.cpp
  Device.h
  Device.cpp
  DriverAdapter.h
  DriverAdapter.cpp
  ../include/SenseKit.h
  SenseKit.cpp
  DeviceRegistry.h
  DeviceRegistry.cpp
  Stream.h
  Stream.cpp
  ColorStream.h
  DepthStream.h
  PluginBase.cpp
  PluginBase.h
  PluginService.cpp
  PluginService.h
  OpenNIPlugin.cpp
  OpenNIPlugin.h
  PluginContext.h
  PluginContext.cpp
  openni/OniStreamServer.h
  openni/OniStreamServer.cpp
  openni/OniStreamSource.h
  openni/OniStreamSource.cpp
  driver/DeviceAdapter.h
  driver/Driver.h
  driver/openni/OpenNIDriver.h
  driver/openni/OpenNIDriver.cpp
  driver/openni/OpenNIDeviceAdapter.h
  driver/openni/OpenNIDeviceAdapter.cpp
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
