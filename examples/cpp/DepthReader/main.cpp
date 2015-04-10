// Orbbec (c) 2015

#include <SenseKit.h>
#include <SenseKitUL.h>
#include <cstdio>
#include <iostream>

#include "../../key_handler.h"

void print_depth(sensekit::DepthFrame& depthFrame,
                 const sensekit::CoordinateMapper& mapper)
{
    if (depthFrame.is_valid())
    {
        int width = depthFrame.get_resolutionX();
        int height = depthFrame.get_resolutionY();
        int frameIndex = depthFrame.get_frameIndex();

        int16_t* buffer = new int16_t[depthFrame.length()];
        depthFrame.copy_to(buffer);

        size_t index = ((width * (height / 2.0f)) + (width / 2.0f));
        short middle = buffer[index];

        float worldX, worldY, worldZ;
        float depthX, depthY, depthZ;
        mapper.convert_depth_to_world(width / 2.0f, height / 2.0f, middle, &worldX, &worldY, &worldZ);
        mapper.convert_world_to_depth(worldX, worldY, worldZ, &depthX, &depthY, &depthZ);

        std::cout << "depth index: " << frameIndex
                  << " value: " << middle
                  << " wX: " << worldX
                  << " wY: " << worldY
                  << " wZ: " << worldZ
                  << " dX: " << depthX
                  << " dY: " << depthY
                  << " dZ: " << depthZ
                  << std::endl;

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

        std::cout << "color index: " << frameIndex
            << " r: " << +r
            << " g: " << +g
            << " b: " << +b
            << std::endl;

        delete[] buffer;
    }
}

class SampleFrameListener : public sensekit::FrameReadyListener
{
    virtual void on_frame_ready(sensekit::StreamReader& reader,
                                 sensekit::Frame& frame) override
        {
            sensekit::DepthFrame depthFrame = frame.get<sensekit::DepthFrame>();
            print_depth(depthFrame,
                        reader.stream<sensekit::DepthStream>().get_coordinateMapper());

            sensekit::ColorFrame colorFrame = frame.get<sensekit::ColorFrame>();
            print_color(colorFrame);

            if (depthFrame.is_valid() && colorFrame.is_valid())
            {
                ++count;
            }
            else
            {
                std::cout << "invalid frame(s)" << std::endl;
            }

            if (count == 10)
            {
                std::cout << "removing listener" << std::endl;
                reader.removeListener(*this);
            }
        }

private:
    int count{0};
};

int main(int argc, char** argv)
{
    set_key_handler();

    sensekit::Sensor sensor;
    sensekit::StreamReader reader = sensor.create_reader();

    SampleFrameListener listener;

    reader.stream<sensekit::DepthStream>().start();
    reader.stream<sensekit::ColorStream>().start();

    reader.addListener(listener);
    int count = 0;
    do
    {
        sensekit_temp_update();
    } while (shouldContinue);
}
