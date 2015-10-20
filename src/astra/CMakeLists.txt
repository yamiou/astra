set (_projname "astra")

set(${_projname}_SOURCES
  ../../include/astra/vector.hpp
  ../../include/astra/vector2f.hpp
  ../../include/astra/vector2i.hpp
  ../../include/astra/vector3f.hpp
  ../../include/astra/vector3i.hpp
  ../../include/astra/astra.hpp
  ../../include/astra/capi/astra.h
  ../../include/astra/capi/astra_ctypes.h
  ../../include/astra/capi/streams/color_capi.h
  ../../include/astra/capi/streams/color_types.h
  ../../include/astra/capi/streams/depth_capi.h
  ../../include/astra/capi/streams/depth_parameters.h
  ../../include/astra/capi/streams/depth_types.h
  ../../include/astra/capi/streams/hand_capi.h
  ../../include/astra/capi/streams/hand_parameters.h
  ../../include/astra/capi/streams/hand_types.h
  ../../include/astra/capi/streams/image_capi.h
  ../../include/astra/capi/streams/image_parameters.h
  ../../include/astra/capi/streams/image_types.h
  ../../include/astra/capi/streams/infrared_capi.h
  ../../include/astra/capi/streams/infrared_types.h
  ../../include/astra/capi/streams/point_capi.h
  ../../include/astra/capi/streams/point_types.h
  ../../include/astra/capi/streams/skeleton_capi.h
  ../../include/astra/capi/streams/skeleton_types.h
  ../../include/astra/capi/streams/stream_types.h
  ../../include/astra/streams/astra_color.hpp
  ../../include/astra/streams/astra_debug_hand.hpp
  ../../include/astra/streams/astra_depth.hpp
  ../../include/astra/streams/astra_hand.hpp
  ../../include/astra/streams/astra_image.hpp
  ../../include/astra/streams/astra_infrared.hpp
  ../../include/astra/streams/astra_point.hpp
  ../../include/astra/streams/astra_skeleton.hpp
  astra_depth.cpp
  astra_color.cpp
  astra_infrared.cpp
  astra_image.cpp
  astra_hand.cpp
  astra_point.cpp
  astra_skeleton.cpp
  astra.cpp
  astra_generic_stream_api.hpp
  )

add_library(${_projname} SHARED ${${_projname}_SOURCES})

add_definitions(-DASTRA_BUILD_EX)

target_link_libraries(${_projname} astra_core_api Shiny)

include_directories(${_projname} ${SHINY_INCLUDE})

install_lib(${_projname})