@echo off
if not exist build-vc12-x64 mkdir build-vc12-x64
cd build-vc12-x64

REM Update your dependency directories below as necessary

cmake -G"Visual Studio 12" -Ax64 -DOpenCV_DIR="D:/libs/opencv-2.4.11/build" -DSFML_ROOT="D:/libs/SFML-2.3.2-vc12-64" -DPROTOBUF_SRC_ROOT_FOLDER="C:/libs/protobuf-2.6.1" .. && goto :copy_files

REM only if cmake failed...
GOTO end

:copy_files
xcopy ..\samples-aux\thirdparty-bin\vs2013\copy_to_bin64_dir\*.* .\bin\Debug\ /E /Y
xcopy ..\samples-aux\thirdparty-bin\vs2013\copy_to_bin64_dir\*.* .\bin\Release\ /E /Y
xcopy ..\samples-aux\thirdparty-bin\vs2013\copy_to_bin64_dir\*.* .\bin\RelWithDebInfo\ /E /Y
echo Done!

:end
cd ..
