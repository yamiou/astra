// Orbbec (c) 2015

#include <Astra/Astra.h>
#include <AstraUL/AstraUL.h>
#include <cstdio>
#include <iostream>

#include <key_handler.h>

void print_depth(astra::DepthFrame& depthFrame,
                 const astra::CoordinateMapper& mapper)
{
    if (depthFrame.is_valid())
    {
        int width = depthFrame.resolutionX();
        int height = depthFrame.resolutionY();
        int frameIndex = depthFrame.frameIndex();

        int16_t* buffer = new int16_t[depthFrame.numberOfPixels()];
        depthFrame.copy_to(buffer);

        size_t index = ((width * (height / 2.0f)) + (width / 2.0f));
        short middle = buffer[index];

        float worldX, worldY, worldZ;
        float depthX, depthY, depthZ;
        mapper.convert_depth_to_world(width / 2.0f, height / 2.0f, middle, &worldX, &worldY, &worldZ);
        mapper.convert_world_to_depth(worldX, worldY, worldZ, &depthX, &depthY, &depthZ);

        std::cout << "depth frameIndex: " << frameIndex
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

class SampleFrameListener : public astra::FrameReadyListener
{
    virtual void on_frame_ready(astra::StreamReader& reader,
                                 astra::Frame& frame) override
    {
        astra::DepthFrame depthFrame = frame.get<astra::DepthFrame>();

        if (depthFrame.is_valid())
        {
            print_depth(depthFrame,
                reader.stream<astra::DepthStream>().coordinateMapper());
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

    reader.stream<astra::DepthStream>().start();

    std::cout << "depthStream -- hFov: "
              << reader.stream<astra::DepthStream>().horizontalFieldOfView()
              << " vFov: "
              << reader.stream<astra::DepthStream>().verticalFieldOfView()
              << std::endl;

    reader.addListener(listener);

    do
    {
        astra_temp_update();
    } while (shouldContinue);

    reader.removeListener(listener);

    astra::Astra::terminate();
}
