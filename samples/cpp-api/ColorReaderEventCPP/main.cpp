// Orbbec (c) 2015

#include <astra_core/astra_core.hpp>
#include <astra/astra.hpp>
#include <cstdio>
#include <iostream>

#include <key_handler.h>

void print_color(astra::colorframe& colorFrame)
{
    if (colorFrame.is_valid())
    {
        int width = colorFrame.resolutionX();
        int height = colorFrame.resolutionY();
        int frameIndex = colorFrame.frameIndex();

        astra::rgb_pixel* buffer = new astra::rgb_pixel[colorFrame.numberOfPixels()];
        colorFrame.copy_to(buffer);

        size_t index = ((width * (height / 2.0f)) + (width / 2.0f));
        astra::rgb_pixel middle = buffer[index];

        std::cout << "color frameIndex: " << frameIndex
                  << " r: " << static_cast<int>(middle.r)
                  << " g: " << static_cast<int>(middle.g)
                  << " b: " << static_cast<int>(middle.b)
                  << std::endl;

        delete[] buffer;
    }
}

class SampleFrameListener : public astra::frame_listener
{
    virtual void on_frame_ready(astra::stream_reader& reader,
                                astra::frame& frame) override
    {
        astra::colorframe colorFrame = frame.get<astra::colorframe>();

        if (colorFrame.is_valid())
        {
            print_color(colorFrame);
        }
    }
};

int main(int argc, char** argv)
{
    astra::initialize();

    set_key_handler();

    astra::streamset streamset;
    astra::stream_reader reader = streamset.create_reader();

    SampleFrameListener listener;

    reader.stream<astra::colorstream>().start();

    std::cout << "colorStream -- hFov: "
              << reader.stream<astra::colorstream>().horizontalFieldOfView()
              << " vFov: "
              << reader.stream<astra::colorstream>().verticalFieldOfView()
              << std::endl;

    reader.addListener(listener);

    do
    {
        astra_temp_update();
    } while (shouldContinue);

    reader.removeListener(listener);

    astra::terminate();
}
