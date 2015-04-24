set (_projname "orbbec_skeleton")

set(${_projname}_SOURCES
  SkeletonPlugin.h
  SkeletonPlugin.cpp
  SkeletonStream.h
  SkeletonTracker.h
  SkeletonTracker.cpp
  )

add_library(${_projname} SHARED ${${_projname}_SOURCES})

target_link_libraries(${_projname} ${SENSEKIT_LIBRARIES})

include_directories(${SENSEKIT_INCLUDE_DIR})

set_target_properties(${_projname} PROPERTIES FOLDER "${SAMPLE_DIR_FOLDER}plugins")

add_sensekit_sdk_dependency(${_projname})