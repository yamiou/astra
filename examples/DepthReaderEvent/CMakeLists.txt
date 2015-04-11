set (DepthReaderEvent_SOURCES
  main.cpp
  )

add_executable(DepthReaderEvent ${DepthReaderEvent_SOURCES})

include_directories(DepthReaderEvent ${SENSEKIT_INCLUDE_DIR})

target_link_libraries(DepthReaderEvent ${SENSEKIT_LIBRARIES})
