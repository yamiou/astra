// Orbbec (c) 2015

#include <SenseKit.h>
#include <SenseKitUL.h>
#include <cstdio>
#include <iostream>

#include "../../key_handler.h"

int main(int argc, char** argv)
{
    set_key_handler();

    sensekit_initialize();

    sensekit::Sensor sensor;
    sensekit::StreamReader reader = sensor.create_reader();

    reader.stream<sensekit::DepthStream>().start();

    do
    {
        sensekit_temp_update();

        sensekit::FrameRef frameRef = reader.get_latest_frame(30);

        sensekit::DepthFrame depthFrame = frameRef.get<sensekit::DepthFrame>();

        int width = depthFrame.get_resolutionX();
        int height = depthFrame.get_resolutionY();
        int frameIndex = depthFrame.get_frameIndex();

        size_t index = ((width * (height / 2)) + (width / 2));
        short middle = depthFrame.data()[index];
        std::cout << "index: " << frameIndex << " value: " << middle << std::endl;

        frameRef.release();

     } while (shouldContinue);

    sensekit_terminate();
}
