set (_projname "orbbec_hand")

set(${_projname}_SOURCES
  HandTracker.cpp
  HandTracker.h
  HandStream.h
  HandStream.cpp
  TrajectoryAnalyzer.h
  TrajectoryAnalyzer.cpp
  DebugHandStream.h
  DebugHandStream.cpp
  DebugVisualizer.h
  DepthUtility.h
  DepthUtility.cpp
  Segmentation.h
  Segmentation.cpp
  TrackedPoint.h
  PointProcessor.h
  PointProcessor.cpp
  TrackingData.h
  ScalingCoordinateMapper.h
  ScalingCoordinateMapper.cpp
  HandPlugin.cpp
  HandPlugin.h
  HandSettings.h
  SettingsParser.h
  constants.h
  ${CMAKE_CURRENT_SOURCE_DIR}/../../SenseKit/vendor/cpptoml.h
  orbbec_hand.toml
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
