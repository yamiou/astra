find_package(GLUT REQUIRED)
find_package(OpenGL REQUIRED)

set (_projname "SimpleDepthViewer-GL")

set (${_projname}_SOURCES
  main.cpp
  utils.h
  SimpleViewer.h
  SimpleViewer.cpp
  )

add_executable(${_projname} ${${_projname}_SOURCES})

set_target_properties(${_projname} PROPERTIES FOLDER "${SAMPLE_DIR_FOLDER}gl")

include_directories(${OPENGL_INCLUDE_DIRS} ${GLUT_INCLUDE_DIR})

target_link_libraries(${_projname} ${SENSEKIT_LIBRARIES} ${OPENGL_LIBRARIES} ${GLUT_LIBRARY})

if (WIN32)
  SET (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SAFESEH:NO")
  SET (CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} /SAFESEH:NO")
  SET (CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} /SAFESEH:NO")
endif()

set_debug_working_dir(${_projname} "$(TargetDir)")

add_sensekit_sdk_dependency(${_projname})