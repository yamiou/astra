set (_projname "orbbec_hand")

include_directories(${CPPTOML_INCLUDE_DIR})

set(ORBBEC_HAND_INCLUDE
  ${CPPTOML_INCLUDE_DIR}/cpptoml.h
  hnd_constants.hpp
  hnd_debug_handstream.hpp
  hnd_debug_visualizer.hpp
  hnd_depth_utility.hpp
  hnd_settings.hpp
  hnd_hand_tracker.hpp
  hnd_handstream.hpp
  hnd_plugin.hpp
  hnd_point_processor.hpp
  hnd_scaling_coordinate_mapper.hpp
  hnd_segmentation.hpp
  hnd_tracked_point.hpp
  hnd_tracking_data.hpp
  hnd_trajectory_analyzer.hpp
  )

set(ORBBEC_HAND_SRC
  hnd_debug_handstream.cpp
  hnd_depth_utility.cpp
  hnd_hand_tracker.cpp
  hnd_handstream.cpp
  hnd_plugin.cpp
  hnd_point_processor.cpp
  hnd_scaling_coordinate_mapper.cpp
  hnd_segmentation.cpp
  hnd_settings_parser.cpp
  hnd_trajectory_analyzer.cpp
  orbbec_hand.toml
  )

if (ANDROID)
  if (NOT OpenCV_ANDROID_SDK_ROOT)
    message(FATAL_ERROR "OpenCV_ANDROID_SDK_ROOT must be set if building for Android.")
  endif()
  set(OpenCV_DIR "${OpenCV_ANDROID_SDK_ROOT}/sdk/native/jni")
  find_package(OpenCV REQUIRED core imgproc NO_DEFAULT_PATH)
else()
  find_package(OpenCV REQUIRED core imgproc)
endif()

#maps relwithbedinfo to release, so don't include debug version of opencv
set_target_properties(${OpenCV_LIBS} PROPERTIES MAP_IMPORTED_CONFIG_RELWITHDEBINFO RELEASE)
set_target_properties(${OpenCV_LIBS} PROPERTIES MAP_IMPORTED_CONFIG_MINSIZEREL RELEASE)

add_library(${_projname} SHARED ${ORBBEC_HAND_SRC} ${ORBBEC_HAND_INCLUDE})

set_target_properties(${_projname} PROPERTIES FOLDER "plugins")

target_link_libraries(${_projname} astra_core_api astra Shiny ${OpenCV_LIBS})

include_directories(${_projname} ${OpenCV_INCLUDE_DIRS})

include_directories(${_projname} ${SHINY_INCLUDE_DIR})

add_custom_target(copytoml_hand ALL
  #orbbec_hand.toml
  COMMAND ${CMAKE_COMMAND} -E copy
  "${PROJECT_SOURCE_DIR}/src/plugins/orbbec_hand/orbbec_hand.toml"
  "$<TARGET_FILE_DIR:${_projname}>")
set_target_properties(copytoml_hand PROPERTIES FOLDER CMakeCopyTargets)

install_lib(${_projname} "Plugins/")
install_file("${PROJECT_SOURCE_DIR}/src/plugins/orbbec_hand/orbbec_hand.toml" lib "Plugins/")
