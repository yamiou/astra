set (_projname "DepthReaderEvent")

set (${_projname}_SOURCES
  main.cpp
  ${SAMPLE_COMMON_DIR}/key_handler.h
  )

add_executable(${_projname} ${${_projname}_SOURCES})

set_target_properties(${_projname} PROPERTIES FOLDER "${SAMPLE_DIR_FOLDER}c-api")

include_directories(${SENSEKIT_INCLUDE_DIR})

target_link_libraries(${_projname} ${SENSEKIT_LIBRARIES})

set_debug_working_dir(${_projname} "$(TargetDir)")