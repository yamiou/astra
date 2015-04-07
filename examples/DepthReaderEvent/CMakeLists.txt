set (DepthReaderEvent_SOURCES
  main.cpp
  )

add_executable(DepthReaderEvent ${DepthReaderEvent_SOURCES})

target_link_libraries(DepthReaderEvent SenseKit SenseKitUL)
