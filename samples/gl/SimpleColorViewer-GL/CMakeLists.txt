find_package(GLUT REQUIRED)
find_package(OpenGL REQUIRED)

set (_projname "SimpleColorViewer-GL")

set (${_projname}_SOURCES
  main.cpp
  utils.h
  SimpleColorViewer.h
  SimpleColorViewer.cpp
  )

add_executable(${_projname} ${${_projname}_SOURCES})

set_target_properties(${_projname} PROPERTIES FOLDER "${SAMPLE_DIR_FOLDER}gl")

include_directories(${OPENGL_INCLUDE_DIRS} ${GLUT_INCLUDE_DIR})

target_link_libraries(${_projname} ${ASTRA_LIBRARIES} ${OPENGL_LIBRARIES} ${GLUT_LIBRARIES})

if (WIN32)
  SET (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SAFESEH:NO")
  SET (CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} /SAFESEH:NO")
  SET (CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} /SAFESEH:NO")
endif()

set_debug_working_dir(${_projname} "$(TargetDir)")

add_astra_sdk_dependency(${_projname})