set (_projname "FrameSerialization")

set (${_projname}_SOURCES
  ../../../include/common/serialization/FrameInputStream.h
  ../../../include/common/serialization/FrameOutputStream.h
  ../../../include/common/serialization/FrameStreamReader.h
  ../../../include/common/serialization/FrameStreamWriter.h
  FrameStreamReader.cpp
  pb_util.cpp
  pb_util.h
  ProtoFrameInputStream.h
  ProtoFrameInputStream.cpp
  ProtoFrameOutputStream.h
  ProtoFrameOutputStream.cpp
  ../../../include/common/serialization/StreamFileModels.h
  ../../../include/common/serialization/FrameStreamWriter.h
  FrameStreamWriter.cpp
)

find_package(Protobuf REQUIRED)

include_directories(${PROTOBUF_INCLUDE_DIRS})

protobuf_generate_cpp(PROTO_SRCS PROTO_HDRS "proto/StreamHeader.proto" "proto/FrameDescription.proto" "proto/Frame.proto")

FOREACH (_srcFile ${PROTO_SRCS})
  get_filename_component(_target ${_srcFile} NAME_WE)
  get_filename_component(_path ${_srcFile} DIRECTORY)

  add_custom_target(${_target} ALL
    COMMAND ${CMAKE_COMMAND} -E copy
      "${_path}/${_target}.pb.cc"
      "${CMAKE_CURRENT_LIST_DIR}/gensrc/"
    COMMAND ${CMAKE_COMMAND} -E copy
      "${_path}/${_target}.pb.h"
      "${CMAKE_CURRENT_LIST_DIR}/gensrc/")

set_target_properties(${_target} PROPERTIES FOLDER CMakeCopyTargets)

ENDFOREACH()

add_library(${_projname} ${${_projname}_SOURCES} ${PROTO_SRCS} ${PROTO_HDRS})

set_target_properties(${_projname} PROPERTIES FOLDER "${COMMON_DIR_FOLDER}serialization")

target_link_libraries(${_projname} ${PROTOBUF_LIBRARIES} ClockUtil)

set_debug_working_dir(${_projname} "$(TargetDir)")
