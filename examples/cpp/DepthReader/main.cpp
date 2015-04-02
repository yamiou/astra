// Orbbec (c) 2015

#include <SenseKit.h>
#include <SenseKitUL.h>
#include <cstdio>
#include <iostream>

#ifdef _WIN32

#include <windows.h>

#else //_WIN32 -> OS X, LINUX

#include <signal.h>
#include <stdlib.h>
#include <unistd.h>

#endif //_WIN32

static bool shouldContinue = true;

void signal_stop_processing(){
    printf("quitting...\n\n");
    shouldContinue = false;
}

#ifdef _WIN32

BOOL CtrlHandler(DWORD fdwCtrlType)
{
    switch (fdwCtrlType)
    {
        // Handle the CTRL-C signal.
    case CTRL_C_EVENT:
        signal_stop_processing();
        return(TRUE);
    default:
        return FALSE;
    }
}

#else //_WIN32 -> OSX, LINUX

static void signal_handler(int s) {
    signal_stop_processing();
}

#endif //_WIN32

int main(int argc, char** argv)
{
#ifdef _WIN32
    SetConsoleCtrlHandler((PHANDLER_ROUTINE)CtrlHandler, TRUE);

#else //_WIN32 -> OS X, LINUX

    struct sigaction sigIntHandler;

    sigIntHandler.sa_handler = &signal_handler;
    sigemptyset(&sigIntHandler.sa_mask);
    sigIntHandler.sa_flags = 0;

    sigaction(SIGINT, &sigIntHandler, NULL);

#endif //_WIN32

    sensekit_initialize();

    sensekit::Sensor sensor;
    sensekit::StreamReader reader = sensor.create_reader();

    reader.stream<sensekit::DepthStream>().start();

    do
    {
        sensekit_temp_update();

        sensekit::FrameRef frameRef = reader.get_latest_frame(30);

        sensekit::DepthFrame depthFrame = frameRef.get<sensekit::DepthFrame>();

        int width = depthFrame.getResolutionX();
        int height = depthFrame.getResolutionY();
        int frameIndex = depthFrame.getFrameIndex();

        size_t index = ((width * (height / 2)) + (width / 2));
        short middle = depthFrame.data()[index];
        std::cout << "index: " << frameIndex << " value: " << middle << std::endl;

        frameRef.release();

     } while (shouldContinue);

    sensekit_terminate();
}
