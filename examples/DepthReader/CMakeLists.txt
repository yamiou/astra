set (DepthReader_SOURCES
  main.cpp
  )

add_executable(DepthReader ${DepthReader_SOURCES})

target_link_libraries(DepthReader SenseKit SenseKitSL)
