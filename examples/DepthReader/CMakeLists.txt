set (DepthReader_SOURCES
  main.c
  )

add_executable(DepthReader ${DepthReader_SOURCES})

target_link_libraries(DepthReader SenseKit SenseKitUL)
