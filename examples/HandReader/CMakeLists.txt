set (HandReader_SOURCES
  main.c
  )

add_executable(HandReader ${HandReader_SOURCES})

include_directories(HandReader ${SENSEKIT_INCLUDE_DIR})

target_link_libraries(HandReader ${SENSEKIT_LIBRARIES})
