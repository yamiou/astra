set(SenseKit_SOURCES
  Context.h
  Context.cpp
  Device.h
  Device.cpp
  DriverAdapter.h
  DriverAdapter.cpp
  ../include/SenseKit.h
  SenseKit.cpp
  DeviceManager.h
  DeviceManager.cpp
  DeviceStreamSource.h
  DeviceStreamSource.cpp
  Stream.h
  Stream.cpp
  ColorStream.h
  DepthStream.h
  openni/OpenNIAdapter.h
  openni/OpenNIAdapter.cpp)

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
