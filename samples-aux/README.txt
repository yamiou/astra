SenseKit SDK v0.1.4 for Windows
Copyright (c) 2015 Orbbec

ALPHA - NOT FOR PUBLIC DISTRIBUTION

josh@orbbec3d.com

What's New
==========

v0.1.4 2015/04/24 Internal reorganization, bug fixes, consistency. Improved debug hand viewer visualizations. Initial re-work of coordinate mapper. Arranged files to support additional plugin development.

v0.1.3 2015/04/21 More fixes in VS2013 project generation - should be good now. Hand tracker now works, and there is now a debug hand viewer sample/tool. Push number keys (above the letters, not numpad) 1-5 to see different visualization modes: Depth, Velocity, Filtered velocity, Segmentation, Score. Various clean up of the SDK header files. Various bug fixes. Get/set parameter mechanism is working. Preliminary logging has been added. A log file is generated in the logs/ directory under the exe working directory.

v0.1.2 2015/04/15 Fixed absolute paths in VS2013 projects. Moved sample folders around. Fixed hand tracker so it gets data, but hand tracking plugin still is not working. Improved SFML samples. Added HandViewer-SFML, but it again hand plugin is not working yet. Various internal improvements to SDK and plugins. The pre-compiled sample binaries will be Release mode, so hopefully won't require VS2013 to be installed.

v0.1.1 2015/04/14 Fixed SimpleColorViewer. Added VS2013 projects (auto-generated via SDK CMake install process). Samples now includes dependencies and have a much better out-of-the-box experience. Fixed minor bugs. HandReader very broken. 

v0.1.0 2015/04/12 Initial internal release

Sample pre-requisites
==========

Visual Studio 2013 or later is required to compile on Windows. The VS 2013 Community version is a free download:
https://www.visualstudio.com/en-us/products/visual-studio-community-vs.aspx

The provided Visual Studio 2013 solution is already configured to run out of the box. 

For reference, some of the samples have dependencies, which are provided in this archive.
See sdk/samples/vs2013/thirdparty/
1) GLUT 3.7 
2) SFML 2.2 - From http://www.sfml-dev.org/download/sfml/2.2/ Windows | Visual C++ 12 (2013) - 32-bit

If you want to run the pre-compiled samples and don't have VS2013 installed, 
you must install the Visual C++ Redistributable Packages for Visual Studio 2013 from
http://www.microsoft.com/en-us/download/details.aspx?id=40784
(Not required if VS2013 is installed.)

Tested on Windows 8.1, but would probably work on Windows 7 as well.

Building the samples
==========

In the sdk/samples/vs2013 folder, open sensekit-samples.sln. Build solution. 

The samples compile to sdk/samples/vs2013/bin/Debug/ or Release/, depending upon your configuration.

Running the samples
==========
To run the samples:
1) Copy all the files from sdk/samples/vs2013/thirdparty/copy_to_bin_dir/ to 
your build folder (bin/Debug/ or bin/Release/).

2) Copy all the files from sdk/bin/ your build folder.

3) Run the samples!

The plugins should still be in a Plugins subfolder under your sample .exe files.

The .pdb files are not necessary for running the applications, but if you find a crash
then you can load the symbols in Visual Studio using these .pdb files and give a useful 
stack trace for reporting bugs.

You can exit most of the command line samples by pressing Control-C. 
They will catch this signal and exit cleanly. 
To exit samples with a GUI window, simply close the window.

Known issues
==========
1) Hand tracking works but at the moment only shows a single hand point. The hand point is also aliased
to the nearest 80x60 coordinate. This is all being investigated currently.

2) There isn't an error message if no sensor is found or plugged in. (But, it doesn't crash!) You can 
plug in a sensor after the program starts and it should detect it and continue. 
Repeated hotplugging (in and out) does not work though.

3) The color sample will crash if the sensor has no color stream. 
It may or may not work for sensors that do have a color stream.

4) If a sample crashes or you stop debugging before it cleans itself up nicely, the sensor driver 
may be put into a weird state. Before the next run you may need to replug the sensor or turn the sensor 
off and then on first. (This is primarily an OpenNI driver issue.)