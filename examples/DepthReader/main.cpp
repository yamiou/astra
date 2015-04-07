// Orbbec (c) 2015

#include <SenseKit.h>
#include <SenseKitUL.h>
#include <cstdio>
#include <iostream>
#include "../key_handler.h"

void print_depth(sensekit_depthframe_t depthFrame)
{
    sensekit_depthframe_metadata_t metadata;
    int16_t* depthData;
    size_t depthLength;

    sensekit_depthframe_get_data_length(depthFrame, &depthLength);
    sensekit_depthframe_get_metadata(depthFrame, &metadata);

    depthData = (int16_t*)malloc(depthLength);
    sensekit_depthframe_copy_data(depthFrame, depthData);

    int width = metadata.width;
    int height = metadata.height;

    size_t index = ((width * (height / 2)) + (width / 2));
    short middle = depthData[index];

    free(depthData);

    uint32_t frameIndex;
    sensekit_depthframe_get_frameindex(depthFrame, &frameIndex);
    std::cout << "index: " << frameIndex << " value: " << middle << std::endl;
}

int main(int argc, char** argv)
{
    set_key_handler();

    sensekit_initialize();

    sensekit_streamset_t sensor;

    sensekit_streamset_open("localhost/device0", &sensor);

    sensekit_reader_t reader;
    sensekit_reader_create(sensor, &reader);

    sensekit_depthstream_t depthStream;
    sensekit_depth_stream_get(reader, &depthStream);

    sensekit_stream_start(depthStream);

    do
    {
        sensekit_temp_update();

        sensekit_reader_frame_t frame;
        sensekit_reader_open_frame(reader, 30, &frame);

        sensekit_depthframe_t depthFrame;
        sensekit_depth_frame_get(frame, &depthFrame);

        print_depth(depthFrame);

        sensekit_reader_close_frame(&frame);

    } while (shouldContinue);

    sensekit_reader_destroy(&reader);
    sensekit_streamset_close(&sensor);

    sensekit_terminate();
}
