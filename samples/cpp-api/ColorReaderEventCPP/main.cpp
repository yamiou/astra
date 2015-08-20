// Orbbec (c) 2015

#include <Astra/Astra.h>
#include <AstraUL/AstraUL.h>
#include <cstdio>
#include <iostream>

#include <key_handler.h>

void print_color(astra::ColorFrame& colorFrame)
{
    if (colorFrame.is_valid())
    {
        int width = colorFrame.resolutionX();
        int height = colorFrame.resolutionY();
        int frameIndex = colorFrame.frameIndex();

        astra::RGBPixel* buffer = new astra::RGBPixel[colorFrame.numberOfPixels()];
        colorFrame.copy_to(buffer);

        size_t index = ((width * (height / 2.0f)) + (width / 2.0f));
        astra::RGBPixel middle = buffer[index];

        std::cout << "color frameIndex: " << frameIndex
                  << " r: " << static_cast<int>(middle.r)
                  << " g: " << static_cast<int>(middle.g)
                  << " b: " << static_cast<int>(middle.b)
                  << std::endl;

        delete[] buffer;
    }
}

class SampleFrameListener : public astra::FrameReadyListener
{
    virtual void on_frame_ready(astra::StreamReader& reader,
                                astra::Frame& frame) override
    {
        astra::ColorFrame colorFrame = frame.get<astra::ColorFrame>();

        if (colorFrame.is_valid())
        {
            print_color(colorFrame);
        }
    }
};

int main(int argc, char** argv)
{
    astra::Astra::initialize();

    set_key_handler();

    astra::Sensor sensor;
    astra::StreamReader reader = sensor.create_reader();

    SampleFrameListener listener;

    reader.stream<astra::ColorStream>().start();

    std::cout << "colorStream -- hFov: "
              << reader.stream<astra::ColorStream>().horizontalFieldOfView()
              << " vFov: "
              << reader.stream<astra::ColorStream>().verticalFieldOfView()
              << std::endl;


    reader.addListener(listener);

    do
    {
        astra_temp_update();
    } while (shouldContinue);

    reader.removeListener(listener);

    astra::Astra::terminate();
}
