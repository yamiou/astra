set(SenseKit_SOURCES
  Context.h
  Context.cpp
  Device.h
  Device.cpp
  Sensor.h
  Sensor.cpp
  DriverAdapter.h
  ../include/SenseKit.h
  SenseKit.cpp
  DeviceManager.h
  DeviceManager.cpp
  DriverService.h
  DriverService.cpp
  StreamSource.h
  StreamSource.cpp
  Stream.h
  Stream.cpp
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
