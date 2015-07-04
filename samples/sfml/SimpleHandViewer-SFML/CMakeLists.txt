set (_projname "SimpleHandViewer-SFML")

set (${_projname}_SOURCES
  ../../common/LitDepthVisualizer.h
  main.cpp
  )

find_package(SFML COMPONENTS graphics window system)

add_executable(${_projname} ${${_projname}_SOURCES})

set_target_properties(${_projname} PROPERTIES FOLDER "${SAMPLE_DIR_FOLDER}sfml")

include_directories(${SFML_INCLUDE_DIR})

target_link_libraries(${_projname} ${SENSEKIT_LIBRARIES} ${SFML_LIBRARIES})

set_debug_working_dir(${_projname} "$(TargetDir)")

add_sensekit_sdk_dependency(${_projname})

install(TARGETS ${_projname}
        RUNTIME DESTINATION bin/
        LIBRARY DESTINATION lib/
        ARCHIVE DESTINATION lib/
        )
