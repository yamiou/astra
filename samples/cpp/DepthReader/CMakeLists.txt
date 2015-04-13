set (DepthReaderCPP_SOURCES
  main.cpp
  )

add_executable(DepthReaderCPP ${DepthReaderCPP_SOURCES})

target_link_libraries(DepthReaderCPP ${SENSEKIT_LIBRARIES})

set_debug_working_dir(DepthReaderCPP "$(TargetDir)")