@echo off
if not exist build-vc14-x86 mkdir build-vc14-x86
cd build-vc14-x86

REM Update your dependency directories below as necessary

cmake -G"Visual Studio 14" -DASTRA_HAND=Off -DOpenCV_DIR="D:/libs/opencv-2.4.11/opencv/build" -DSFML_ROOT="D:/libs/SFML-2.3.2-vc14-32" -DPROTOBUF_SRC_ROOT_FOLDER="D:/libs/protobuf-2.6.1" .. && goto :copy_files

REM only if cmake failed...
GOTO end
:copy_files
echo Copying files
xcopy ..\samples-aux\thirdparty-bin\vs2015\copy_to_bin32_dir\*.* .\bin\Debug\ /E /Y
xcopy ..\samples-aux\thirdparty-bin\vs2015\copy_to_bin32_dir\*.* .\bin\Release\ /E /Y
xcopy ..\samples-aux\thirdparty-bin\vs2015\copy_to_bin32_dir\*.* .\bin\RelWithDebInfo\ /E /Y
echo Done!

:end
cd ..
