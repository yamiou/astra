set(_projname "test-harness")

include_directories(${CATCH_INCLUDE_DIR} ${ASTRA_INCLUDE_DIR})

set(${_projname}_SOURCES
  main.cpp
  stream-api/basic_tests.cpp
  stream-api/stream_tests.cpp
#  c-api/DepthReaderPoll_tests.cpp
  )

add_executable(${_projname} ${${_projname}_SOURCES})
target_link_libraries(${_projname} ${ASTRA_LIBRARIES})

set_debug_working_dir(${_projname} "$(TargetDir)")

add_astra_sdk_dependency(${_projname})

set_target_properties(${_projname} PROPERTIES FOLDER "tests")
set_target_properties(${_projname} PROPERTIES OUTPUT_NAME "astra-tests")
