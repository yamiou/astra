set(orbbec_hands_SOURCES
  ../../../include/SenseKit/SenseKit.h
  ../../../include/SenseKit/sensekit_core.h
  ../../../include/SenseKit/sensekit_types.h
  ../../../include/SenseKit/sensekit_plugin_types.h
  ../../../include/SenseKit/sensekit_capi.h
  ../../../include/SenseKitUL/StreamTypes.h
  ../../../include/SenseKitUL/SenseKitUL.h
  handtracker.cpp
  handtracker.h
  hands_plugin.cpp
  hands_plugin.h
)

find_package( OpenCV REQUIRED )

add_library(orbbec_hands SHARED ${orbbec_hands_SOURCES})

target_link_libraries(orbbec_hands SenseKitUL ${OpenCV_LIBS})

include_directories(orbbec_hands ${OpenCV_INCLUDE_DIRS})