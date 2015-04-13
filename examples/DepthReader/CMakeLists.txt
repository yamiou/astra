set (DepthReader_SOURCES
  main.c
  )

add_executable(DepthReader ${DepthReader_SOURCES})

include_directories(DepthReader ${SENSEKIT_INCLUDE_DIR})

target_link_libraries(DepthReader ${SENSEKIT_LIBRARIES})

set_debug_working_dir(DepthReader "$(TargetDir)")