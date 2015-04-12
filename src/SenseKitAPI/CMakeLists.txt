set(SenseKitAPI_SOURCES
  SenseKitAPI.h
  SenseKitAPI.cpp
  )

set(SenseKitAPI_SOURCES_GEN
  SenseKitAPI.cpp.lpp
  )

add_definitions(-DSENSEKIT_BUILD_API_PROXY)
add_definitions(-DSENSEKIT_BUILD)

add_library(SenseKitAPI SHARED ${SenseKitAPI_SOURCES} ${SenseKitAPI_SOURCES_GEN})

# force c++11 (has no effect currently)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD 11)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD_REQUIRED ON)

install(TARGETS SenseKitAPI
        RUNTIME DESTINATION bin/
        LIBRARY DESTINATION bin/
        ARCHIVE DESTINATION lib/
        )