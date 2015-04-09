set (HandReader_SOURCES
  main.c
  )

add_executable(HandReader ${HandReader_SOURCES})

target_link_libraries(HandReader SenseKit SenseKitUL)
