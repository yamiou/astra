set (_projname "orbbec_skeleton")

set(${_projname}_SOURCES
  SkeletonPlugin.h
  SkeletonPlugin.cpp
  skeletonstream.h
  SkeletonTracker.h
  SkeletonTracker.cpp
  )

add_library(${_projname} SHARED ${${_projname}_SOURCES})

target_link_libraries(${_projname} ${ASTRA_LIBRARIES})

include_directories(${ASTRA_INCLUDE_DIR})

set_target_properties(${_projname} PROPERTIES FOLDER "${SAMPLE_DIR_FOLDER}plugins")

add_astra_sdk_dependency(${_projname})