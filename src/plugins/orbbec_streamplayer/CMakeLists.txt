set (_projname "orbbec_streamplayer")

set(${_projname}_SOURCES
  StreamPlayerPlugin.cpp
  StreamPlayerPlugin.h
  DepthStream.h
  PlaybackStreamSet.h
  PlaybackStream.h
)

add_library(${_projname} SHARED ${${_projname}_SOURCES})

set_target_properties(${_projname} PROPERTIES FOLDER "plugins")

target_link_libraries(${_projname} AstraAPI AstraUL FrameSerialization ClockUtil)

include_directories(${_projname})

#install(TARGETS ${_projname}
#  RUNTIME DESTINATION bin/Plugins/
#  LIBRARY DESTINATION lib/Plugins/
#  )

#install(FILES "${DF_FILES}"
#        DESTINATION bin/Plugins/
#  )

#install_pdb(${_projname} bin/Plugins/)
