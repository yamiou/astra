// Orbbec (c) 2015

#include <Sensekit/SenseKit.h>
#include <SensekitUL/SenseKitUL.h>
#include <cstdio>
#include <iostream>

#include <key_handler.h>

void print_color(sensekit::ColorFrame& colorFrame)
{
    if (colorFrame.is_valid())
    {
        int width = colorFrame.resolutionX();
        int height = colorFrame.resolutionY();
        int frameIndex = colorFrame.frameIndex();

        sensekit::RGBPixel* buffer = new sensekit::RGBPixel[colorFrame.numberOfPixels()];
        colorFrame.copy_to(buffer);

        size_t index = ((width * (height / 2.0f)) + (width / 2.0f));
        sensekit::RGBPixel middle = buffer[index];

        std::cout << "color frameIndex: " << frameIndex
                  << " r: " << static_cast<int>(middle.r)
                  << " g: " << static_cast<int>(middle.g)
                  << " b: " << static_cast<int>(middle.b)
                  << std::endl;

        delete[] buffer;
    }
}

class SampleFrameListener : public sensekit::FrameReadyListener
{
    virtual void on_frame_ready(sensekit::StreamReader& reader,
                                sensekit::Frame& frame) override
    {
        sensekit::ColorFrame colorFrame = frame.get<sensekit::ColorFrame>();

        if (colorFrame.is_valid())
        {
            print_color(colorFrame);
        }
    }
};

int main(int argc, char** argv)
{
    sensekit::SenseKit::initialize();

    set_key_handler();

    sensekit::Sensor sensor;
    sensekit::StreamReader reader = sensor.create_reader();

    SampleFrameListener listener;

    reader.stream<sensekit::ColorStream>().start();

    std::cout << "colorStream -- hFov: "
              << reader.stream<sensekit::ColorStream>().horizontalFieldOfView()
              << " vFov: "
              << reader.stream<sensekit::ColorStream>().verticalFieldOfView()
              << std::endl;


    reader.addListener(listener);

    do
    {
        sensekit_temp_update();
    } while (shouldContinue);

    reader.removeListener(listener);

    sensekit::SenseKit::terminate();
}
