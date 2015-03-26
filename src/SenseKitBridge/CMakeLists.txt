set(SenseKitBridge_SOURCES
  SenseKitAL.cpp
  )

add_library(SenseKitBridge STATIC ${SenseKitBridge_SOURCES})

# force c++11 (has no effect currently)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD 11)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD_REQUIRED ON)
