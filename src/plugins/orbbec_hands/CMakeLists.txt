set(orbbec_hands_SOURCES
  handtracker.cpp
  handtracker.h
  depthutility.h
  depthutility.cpp
  segmentationutility.h
  segmentationutility.cpp
  trackedpoint.h
  vectorx.h
  vector2.h
  vector3.h
  pointprocessor.h
  pointprocessor.cpp
  trackingdata.h
  coordinateconversion.h
  hands_plugin.cpp
  hands_plugin.h
)

find_package( OpenCV REQUIRED )

add_library(orbbec_hands SHARED ${orbbec_hands_SOURCES})

target_link_libraries(orbbec_hands SenseKitUL ${OpenCV_LIBS})

include_directories(orbbec_hands ${OpenCV_INCLUDE_DIRS})