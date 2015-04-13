set (DepthViewer_SOURCES
  main.cpp
  )

find_package(SFML COMPONENTS graphics window system audio)

add_executable(DepthViewer ${DepthViewer_SOURCES})

include_directories(${SFML_INCLUDE_DIR})

target_link_libraries(DepthViewer ${SENSEKIT_LIBRARIES} ${SFML_LIBRARIES})

set_debug_working_dir(DepthViewer "$(TargetDir)")