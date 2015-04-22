set (_projname "orbbec_skeleton")

set(${_projname}_SOURCES
  SkeletonPlugin.h
  SkeletonPlugin.cpp
  SkeletonStream.h
  SkeletonTracker.h
  SkeletonTracker.cpp
  )

add_library(${_projname} SHARED ${${_projname}_SOURCES})

set_target_properties(${_projname} PROPERTIES FOLDER "plugins")

target_link_libraries(${_projname} SenseKitAPI SenseKitUL)

include_directories(${_projname})

install(TARGETS ${_projname}
  RUNTIME DESTINATION bin/Plugins/
  LIBRARY DESTINATION bin/Plugins/
  )

install_pdb(${_projname} bin/Plugins/)
