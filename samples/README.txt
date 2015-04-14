SenseKit SDK v0.1.1 for Windows
Copyright (c) 2015 Orbbec

ALPHA - NOT FOR PUBLIC DISTRIBUTION

josh@orbbec3d.com

What's New
==========

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

If you want to run the samples and don't have VS2013 installed, 
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
1) The hand reader app is currently broken. Don't bother trying to test it, except to see that it 
runs or to look at the API.

2) There isn't an error message if no sensor is found or plugged in. (But, it doesn't crash!) You can 
plug in a sensor after the program starts and it should detect it and continue. 
Repeated hotplugging (in and out) does not work though.

3) The color sample will crash if the sensor has no color stream. 
It may or may not work for sensors that do have a color stream.

4) If a sample crashes or you stop debugging before it cleans itself up nicely, the sensor driver 
may be put into a weird state. Before the next run you may need to replug the sensor or turn the sensor 
off and then on first. (This is primarily an OpenNI driver issue.)

5) The DepthReaderCPP sample will automatically remove its event listener after 100 callbacks. This is
not a bug, but is testing part of the API.

6) The orbbec_hands plugin uses the debug version of opencv temporarily, so for now you need to have
VS2013 installed to run samples (which provides the debug runtime.)