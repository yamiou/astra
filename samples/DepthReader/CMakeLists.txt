set (DepthReader_SOURCES
  main.c
  )

add_executable(DepthReader ${DepthReader_SOURCES})

target_link_libraries(DepthReader ${SENSEKIT_LIBRARIES})

set_debug_working_dir(DepthReader "$(TargetDir)")