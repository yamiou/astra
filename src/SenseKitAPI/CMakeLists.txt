set(SenseKitAPI_SOURCES
  SenseKitAPI.cpp
  )

add_library(SenseKitAPI SHARED ${SenseKitAPI_SOURCES})

# force c++11 (has no effect currently)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD 11)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD_REQUIRED ON)
