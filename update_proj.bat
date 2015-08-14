if not exist build mkdir build
cd build

REM Update your dependency directories below as necessary

cmake -G"Visual Studio 12" -DOpenCV_DIR="C:\libs\opencv-2.4.10\build" -DGLUT_ROOT_PATH="C:\libs\glut_windows" -DSFML_ROOT="C:\libs\SFML-2.2" -DPROTOBUF_SRC_ROOT_FOLDER="C:\libs\protobuf-2.6.1" ..
cd ..
