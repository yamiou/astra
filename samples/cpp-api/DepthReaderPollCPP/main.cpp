// Orbbec (c) 2015

#include <Sensekit/SenseKit.h>
#include <SensekitUL/SenseKitUL.h>
#include <cstdio>
#include <iostream>

#include <key_handler.h>

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

class SampleFrameListener : public sensekit::FrameReadyListener
{
    virtual void on_frame_ready(sensekit::StreamReader& reader,
                                 sensekit::Frame& frame) override
        {
            sensekit::DepthFrame depthFrame = frame.get<sensekit::DepthFrame>();
            print_depth(depthFrame,
                        reader.stream<sensekit::DepthStream>().get_coordinateMapper());

            if (depthFrame.is_valid())
            {
                ++count;
            }
            else
            {
                std::cout << "invalid frame(s)" << std::endl;
            }

            if (count == 100)
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
    sensekit_initialize();
 
    set_key_handler();

    sensekit::Sensor sensor;
    sensekit::StreamReader reader = sensor.create_reader();

    SampleFrameListener listener;

    reader.stream<sensekit::DepthStream>().start();

    reader.addListener(listener);

    do
    {
        sensekit_temp_update();
    } while (shouldContinue);

    reader.removeListener(listener);
    
    sensekit_terminate();
}
