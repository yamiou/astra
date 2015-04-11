set (DepthReaderCPP_SOURCES
  main.cpp
  )

add_executable(DepthReaderCPP ${DepthReaderCPP_SOURCES})

include_directories(DepthReaderCPP ${SENSEKIT_INCLUDE_DIR})

target_link_libraries(DepthReaderCPP ${SENSEKIT_LIBRARIES})
