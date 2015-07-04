set(C_SOURCES
  sensekit_wrapper.h
  sensekit_wrapper.cpp
  )

add_library(sensekit_jni SHARED ${C_SOURCES})
target_link_libraries(sensekit_jni ${SENSEKIT_LIBRARIES})

install(TARGETS sensekit_jni
        RUNTIME DESTINATION bin/
        LIBRARY DESTINATION lib/
        ARCHIVE DESTINATION lib/
        )
