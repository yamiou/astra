set(SenseKit_SOURCES
  Context.h
  Context.cpp
  Sensor.h
  Sensor.cpp
  SensorAdapter.h
  SenseKit-private.h
  SenseKit.cpp
  SenseKitTypes.h
  openni/OpenNIAdapter.cpp
  openni/OpenNIAdapter.hpp)


add_library(SenseKit SHARED ${SenseKit_SOURCES})

# force c++11 (has no effect currently)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD 11)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD_REQUIRED ON)


target_link_libraries(SenseKit ${OpenNI2_LIBRARY})
