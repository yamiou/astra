Astra SDK v0.2.1 Preview for Windows and Android
Copyright (c) 2015 Orbbec
www.orbbec3d.com

josh@orbbec3d.com

What's New
==========

v0.2.1 2015/07/06 Updated Android and Windows drivers for new sensor USB IDs. Add Android test app .apk.

v0.2.0 2015/07/03 First version ready for external use.

Sample pre-requisites
==========

Visual Studio 2013 or later is required to compile on Windows. The VS 2013 Community version is a free download:
https://www.visualstudio.com/en-us/products/visual-studio-community-vs.aspx

The provided Visual Studio 2013 solution is already configured to run out of the box.

For reference, some of the samples have dependencies, which are provided in this archive.
See sdk/samples/vs2013/thirdparty/
* SFML 2.2 - From http://www.sfml-dev.org/download/sfml/2.2/ Windows | Visual C++ 12 (2013) - 32-bit

If you want to run the pre-compiled samples and don't have VS2013 installed,
you must install the Visual C++ Redistributable Packages for Visual Studio 2013 from
http://www.microsoft.com/en-us/download/details.aspx?id=40784
(Not required if VS2013 is installed.)

Tested on Windows 8.1, Windows 7, and Android 4.4.2.

Pre-built samples
==========

Pre-built samples are included in the bin/ directory.
Simply plug in your sensor and then run any of the executable files in the bin/ directory.

We recommend starting with SimpleDepthViewer-SFML and SimpleHandViewer-SFML.
In the hand viewer, wave left and right at the sensor a few times to start hand tracking.

Building the samples
==========

In the sdk/samples/vs2013 folder, open astra-samples.sln. Build solution.

The samples compile to sdk/samples/vs2013/bin/Debug/ or Release/, depending upon your configuration.

Running the samples
==========
To run the samples:
1) Copy all the files from sdk/samples/vs2013/thirdparty/copy_to_bin_dir/ to
your build folder (bin/Debug/ or bin/Release/).

2) From the sdk/bin/ folder, copy Astra.dll, AstraAPI.dll, AstraUL.dll,
and the entire Plugin folder to your build folder.
The plugins should still be in a Plugins subfolder under your sample .exe files.

3) Run the samples!

The .pdb files are not necessary for running the applications, but if you find a crash
then you can load the symbols in Visual Studio using these .pdb files and give a useful
stack trace for reporting bugs.

You can exit most of the command line samples by pressing Control-C.
They will catch this signal and exit cleanly.
To exit samples with a GUI window, simply close the window.

Known issues
==========
1) There is no error message if no sensor is found or plugged in. (But, it doesn't crash!) You can
plug in a sensor after the program starts and it should detect it and continue.
Repeated hotplugging (in and out) does not work though.

2) If a sample crashes or you stop debugging before it cleans itself up nicely, the sensor driver
may be put into a weird state. Before the next run you may need to replug the sensor or turn the sensor
off and then on first. (This is primarily an OpenNI driver issue.)

3) If you close a sample's console window without cleanly exiting the program, it may crash. For console
samples, press control-c to exit cleanly. For GUI apps, press escape to exit cleanly.
