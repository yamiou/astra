SenseKit SDK v0.1 for Windows
Copyright (c) 2015 Orbbec

ALPHA - NOT FOR PUBLIC DISTRIBUTION
4/12/2015

josh@orbbec3d.com

System requirements

Sample pre-requisites
==========

To compile the samples, you'll need
1) CMake 3.2.1 - http://www.cmake.org/download/ (Binary distributions, Windows (Win32 installer))
2) GLUT 3.7 - provided for convenience in the sensekit_win32_deps.zip under glut_windows
3) SFML 2.2 - http://www.sfml-dev.org/download/sfml/2.2/ Windows | Visual C++ 12 (2013) - 32-bit

Visual Studio 2013 is required to compile. 
If you want to run the samples and don't have VS2013 installed, 
you must install the Visual C++ Redistributable Packages for Visual Studio 2013 from
http://www.microsoft.com/en-us/download/details.aspx?id=40784
(Not required if VS2013 is installed.)

Tested on Windows 8.1, but would probably work on Windows 7 as well.

Building the samples
==========

Unzip the SFML-2.2-windows-vc12-32-bit.zip to a good location, for example C:\projects\SFML-2.2.
Edit your Environment Variables (e.g. http://winaero.com/blog/how-to-edit-environment-variables-quickly-in-windows-8-1-and-windows-8/)
and add a new variable named SFML_ROOT pointing to the directory you unzipped SFML to. 
(This should be the directory that contains SFML's readme.txt, bin directory, lib directory, etc.)

1) Use CMake-gui to auto-configure the project
2) Set GLUT_INCLUDE_DIR to the directory you unzipped glut_windows to, e.g. C:\projects\glut_windows
3) Set GLUT_glut_LIBRARY to the glut32.lib file, e.g. C:/projects/glut_windows/glut32.lib
4) Configure, then generate. 

Now you should be able to open sensekit-samples.sln from the build directory and compile the samples directly.
The samples will all be output to bin/Debug or bin/Release under the build directory, depending upon your 
build configuration.

Running the samples
==========
To run the samples, you'll need.
1) Unzip sensekit_win32_deps.zip
This zip includes:
* glut32.dll
* opencv_core2410.dll & opencv_core2410d.dll
* opencv_imgproc2410.dll & opencv_imgproc2410d.dll
* OpenNI.dll, OpenNI.ini, and OpenNI2 directory (the version compatible with Orbbec sensors)
* sfml-graphics-2.dll
* sfml-system-2.dll
* sfml-window-2.dll
* glut_windows/* (only needed for building the samples, see above)

After building the samples, copy all of these files (except the glut_windows directory) to 
your sample output folder (bin/Debug or bin/Release).

2) sensekit binaries, from this zip distribution. 
Copy the contents of the sensekitsdk bin/ folder to your sample output folder.
This should include:
* SenseKit.dll & .pdb
* SenseKitAPI.dll & .pdb
* SenseKitUL.dll & .pdb
* Plugins/DummyPlugin.dll & .pdb
* Plugins/OpenNIPlugin.dll & .pdb
* Plugins/orbbec_hands.dll & .pdb

The plugins should still be in a Plugins subfolder under your sample .exe files.
The .pdb files are not necessary for running the applications, but if you find a crash
then you can load the symbols in Visual Studio using these .pdb files and give a useful 
stack trace for reporting bugs.

You can exit most of the command line samples by pressing Control-C. 
They will catch this signal and exit cleanly. 
To exit samples with a GUI window, simply close the window.

Known issues
==========
1) The hand tracking produces data but there is currently a bug in the plugin that prevents 
good hand tracking. Don't bother trying to test it, except to see that it runs or to look at the API.

2) There isn't an error message if no sensor is found or plugged in. (But, it doesn't crash!) You can 
plug in a sensor after the program starts and it should detect it and continue.

3) The color sample will crash if the sensor has no color stream. 
It may or may not work for sensors that do have a color stream.

4) If a sample crashes or you stop debugging before it cleans itself up nicely, the sensor driver 
may be put into a weird state. Before the next run you may need to replug the sensor or turn the sensor 
off and then on first. (This is primarily an OpenNI driver issue.)

5) The DepthReaderCPP sample will automatically remove its event listener after 100 callbacks. This is
not a bug, but is testing part of the API.