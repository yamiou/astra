find_package(GLUT REQUIRED)
find_package(OpenGL REQUIRED)

set (SimpleViewer_SOURCES
  main.cpp
  SimpleViewer.cpp
  )

add_executable(SimpleViewer ${SimpleViewer_SOURCES})

target_link_libraries(SimpleViewer SenseKit ${OPENGL_LIBRARIES} ${GLUT_LIBRARIES})
