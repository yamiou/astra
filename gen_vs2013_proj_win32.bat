@ECHO OFF
SETLOCAL EnableExtensions EnableDelayedExpansion

REM Update your dependency directories below as necessary

SET SFMLPATH=C:\libs\SFML-2.3.2-vc12-32
SET PROTOPATH=C:\libs\protobuf-2.6.1

SET MSVCNAME=2013
SET MSVCVERSION=12
SET ARCH=x86
SET BUILDPATH=build\vs%MSVCVERSION%\%ARCH%
SET GENERATOR=Visual Studio %MSVCVERSION% %MSVCNAME%

SET ASTRADIR=%~dp1
SET EXTRAARGS=

PUSHD

CD %ASTRADIR%

IF NOT EXIST %BUILDPATH% (
   MD %BUILDPATH%  || GOTO :ERROR
)

CD %BUILDPATH%

cmake -G"%GENERATOR%" ^
      -DSFML_ROOT="%SFMLPATH%" ^
      -DPROTOBUF_SRC_ROOT_FOLDER="%PROTOPATH%" ^
      %EXTRAARGS% "%ASTRADIR%" || GOTO :ERROR

ECHO Copying files

FOR %%G IN (bin\Debug bin\Release bin\RelWithDebInfo bin\MinSizeRel) DO (
    xcopy %ASTRADIR%\samples-aux\thirdparty-bin\vs%MSVCVERSION%\copy_to_bin32_dir\*.* %%G /E /Y
) || GOTO :ERROR

ECHO Done!

:ERROR
ECHO Something went wrong...

:END
POPD
