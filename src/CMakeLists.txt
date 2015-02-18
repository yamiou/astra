set(SenseKit_SOURCES
  SenseKit-adapter-private.h
  SenseKit-openni-private.h
  SenseKit-openni.c
  SenseKit-private.h
  SenseKit.c
  SenseKitTypes.h
  openni/OpenNIAdapter.cpp
  openni/OpenNIAdapter.hpp)


add_library(SenseKit SHARED ${SenseKit_SOURCES})

# force c++11 (has no effect currently)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD 11)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD_REQUIRED ON)


target_link_libraries(SenseKit ${OpenNI2_LIBRARY})
