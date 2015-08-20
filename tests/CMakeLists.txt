set(PROJECT_NAME "test-harness")

include_directories(${CATCH_INCLUDE_DIR} ${ASTRA_INCLUDE_DIR})

set(${PROJECT_NAME}_SOURCES
  main.cpp
  stream-api/basic_tests.cpp
  stream-api/stream_tests.cpp
  )

add_executable(${PROJECT_NAME} ${${PROJECT_NAME}_SOURCES})
target_link_libraries(${PROJECT_NAME} Astra)
set_target_properties(${PROJECT_NAME} PROPERTIES FOLDER "tests")
set_target_properties(${PROJECT_NAME} PROPERTIES OUTPUT_NAME "astra-tests")
