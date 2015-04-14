set(orbbec_hands_SOURCES
  handtracker.cpp
  handtracker.h
  debugvisualizer.h
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
  coordinateconverter.h
  hands_plugin.cpp
  hands_plugin.h
)

find_package( OpenCV REQUIRED )

add_library(orbbec_hands SHARED ${orbbec_hands_SOURCES})

set_target_properties(orbbec_hands PROPERTIES FOLDER "plugins")

target_link_libraries(orbbec_hands SenseKitAPI SenseKitUL ${OpenCV_LIBS})

include_directories(orbbec_hands ${OpenCV_INCLUDE_DIRS})

install(TARGETS orbbec_hands
        RUNTIME DESTINATION bin/Plugins/
        LIBRARY DESTINATION bin/Plugins/
        )

install_pdb(orbbec_hands bin/Plugins/)
