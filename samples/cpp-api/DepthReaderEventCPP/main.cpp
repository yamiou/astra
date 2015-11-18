// This file is part of the Orbbec Astra SDK [https://orbbec3d.com]
// Copyright (c) 2015 Orbbec 3D
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// Be excellent to each other.
#include <astra_core/astra_core.hpp>
#include <astra/astra.hpp>
#include <cstdio>
#include <chrono>
#include <iostream>
#include <iomanip>

#include <key_handler.h>

void print_depth(astra::depthframe& depthFrame,
                 const astra::coordinate_mapper& mapper)
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

class SampleFrameListener : public astra::frame_listener
{
    virtual void on_frame_ready(astra::stream_reader& reader,
                                 astra::frame& frame) override
    {
        astra::depthframe depthFrame = frame.get<astra::depthframe>();

        if (depthFrame.is_valid())
        {
            print_depth(depthFrame,
                reader.stream<astra::depthstream>().coordinateMapper());
            check_fps();
        }
    }

    void check_fps()
    {
        const double frameWeight = 0.2;

        auto newTimepoint = clock_type::now();
        auto frameDuration = std::chrono::duration_cast<duration_type>(newTimepoint - lastTimepoint_);

        frameDuration_ = frameDuration * frameWeight + frameDuration_ * (1 - frameWeight);
        lastTimepoint_ = newTimepoint;

        double fps = 1.0 / frameDuration_.count();

        auto precision = std::cout.precision();
        std::cout << std::fixed
                  << std::setprecision(1)
                  << fps << " fps ("
                  << std::setprecision(2)
                  << frameDuration.count() * 1000 << " ms)"
                  << std::setprecision(precision)
                  << std::endl;
    }

private:
    using duration_type = std::chrono::duration<double>;
    duration_type frameDuration_{0.0};

    using clock_type = std::chrono::system_clock;
    std::chrono::time_point<clock_type> lastTimepoint_;
};

int main(int argc, char** argv)
{
    astra::initialize();

    set_key_handler();

    astra::streamset streamset("device/default");
    astra::stream_reader reader = streamset.create_reader();

    SampleFrameListener listener;

    reader.stream<astra::depthstream>().start();

    std::cout << "depthStream -- hFov: "
              << reader.stream<astra::depthstream>().horizontalFieldOfView()
              << " vFov: "
              << reader.stream<astra::depthstream>().verticalFieldOfView()
              << std::endl;

    reader.add_listener(listener);

    do
    {
        astra_temp_update();
    } while (shouldContinue);

    reader.remove_listener(listener);

    astra::terminate();
}
