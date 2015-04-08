// Orbbec (c) 2015

#include <SenseKit.h>
#include <SenseKitUL.h>
#include <cstdio>
#include <iostream>

#include "../../key_handler.h"

void print_depth(sensekit::DepthFrame& depthFrame)
{
    if (depthFrame.is_valid())
    {
        int width = depthFrame.get_resolutionX();
        int height = depthFrame.get_resolutionY();
        int frameIndex = depthFrame.get_frameIndex();

        int16_t* buffer = new int16_t[depthFrame.length()];
        depthFrame.copy_to(buffer);

        size_t index = ((width * (height / 2)) + (width / 2));
        short middle = buffer[index];
        std::cout << "index: " << frameIndex << " value: " << middle << std::endl;

        delete[] buffer;
    }
}

void print_color(sensekit::ColorFrame& colorFrame)
{
    if (colorFrame.is_valid())
    {
        int width = colorFrame.get_resolutionX();
        int height = colorFrame.get_resolutionY();
        int frameIndex = colorFrame.get_frameIndex();

        uint8_t* buffer = new uint8_t[colorFrame.length()];
        colorFrame.copy_to(buffer);

        size_t index = ((width * (height / 2)) + (width / 2)) * colorFrame.get_bytesPerPixel();
        uint8_t r = buffer[index];
        uint8_t g = buffer[index + 1];
        uint8_t b = buffer[index + 2];

        std::cout << "index: " << frameIndex
            << " r: " << +r
            << " g: " << +g
            << " b: " << +b
            << std::endl;

        delete[] buffer;
    }
}

int main(int argc, char** argv)
{
    set_key_handler();

    sensekit_initialize();

    sensekit::Sensor sensor;
    sensekit::StreamReader reader = sensor.create_reader();

    reader.stream<sensekit::DepthStream>().start();
    //reader.stream<sensekit::ColorStream>().start();

    do
    {
        sensekit_temp_update();

        sensekit::FrameRef frameRef = reader.get_latest_frame(30);
        sensekit::FrameRef& frameRef2 = frameRef;
        sensekit::FrameRef& frameRef3 = frameRef2;
        sensekit::DepthFrame depthFrame = frameRef3.get<sensekit::DepthFrame>();
        print_depth(depthFrame);

        //sensekit::ColorFrame colorFrame = frameRef.get<sensekit::ColorFrame>();
        //print_color(colorFrame);


     } while (shouldContinue);

    sensekit_terminate();
}
