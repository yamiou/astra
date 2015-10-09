@echo off
if not exist build-x64 mkdir build-x64
cd build-x64

REM Update your dependency directories below as necessary

cmake -G"Visual Studio 12" -A x64 -DOpenCV_DIR="C:\libs\opencv-2.4.10\build-x64" -DSFML_ROOT="C:\libs\SFML-2.3.2-64" -DPROTOBUF_SRC_ROOT_FOLDER="C:\libs\protobuf-2.6.1" .. && goto :copy_files

REM only if cmake failed...
GOTO end

:copy_files
xcopy ..\samples-aux\vs2013\copy_to_bin64_dir\*.* .\bin\Debug\ /E /Y
xcopy ..\samples-aux\vs2013\copy_to_bin64_dir\*.* .\bin\Release\ /E /Y
xcopy ..\samples-aux\vs2013\copy_to_bin64_dir\*.* .\bin\RelWithDebInfo\ /E /Y
echo Done!

:end
cd ..