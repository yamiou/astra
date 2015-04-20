set (_projname "orbbec_hands")

set(${_projname}_SOURCES
  handtracker.cpp
  handtracker.h
  HandStream.h
  DebugHandStream.h
  DebugHandStream.cpp
  debugvisualizer.h
  depthutility.h
  depthutility.cpp
  segmentationutility.h
  segmentationutility.cpp
  trackedpoint.h
  pointprocessor.h
  pointprocessor.cpp
  trackingdata.h
  coordinateconverter.h
  hands_plugin.cpp
  hands_plugin.h
)

find_package( OpenCV REQUIRED core imgproc)
#maps relwithbedinfo to release, so don't include debug version of opencv
set_target_properties(${OpenCV_LIBS} PROPERTIES MAP_IMPORTED_CONFIG_RELWITHDEBINFO RELEASE)
set_target_properties(${OpenCV_LIBS} PROPERTIES MAP_IMPORTED_CONFIG_MINSIZEREL RELEASE)

add_library(${_projname} SHARED ${${_projname}_SOURCES})

set_target_properties(${_projname} PROPERTIES FOLDER "plugins")

target_link_libraries(${_projname} SenseKitAPI SenseKitUL ${OpenCV_LIBS})

include_directories(${_projname} ${OpenCV_INCLUDE_DIRS})

install(TARGETS ${_projname}
        RUNTIME DESTINATION bin/Plugins/
        LIBRARY DESTINATION bin/Plugins/
        )

install_pdb(${_projname} bin/Plugins/)
