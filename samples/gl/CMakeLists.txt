if (NOT DEFINED GLUT_ROOT_PATH)
  set(GLUT_ROOT_PATH "$ENV{GLUT_ROOT_PATH}")
  message("Using env var for GLUT: $ENV{GLUT_ROOT_PATH}")
endif()

add_subdirectory(SimpleDepthViewer-GL)
add_subdirectory(SimpleColorViewer-GL)
