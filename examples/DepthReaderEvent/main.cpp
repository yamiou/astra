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

    sensekit_depthframe_get_data_ptr(depthFrame, &depthData, &depthLength);
    sensekit_depthframe_get_metadata(depthFrame, &metadata);

    int width = metadata.width;
    int height = metadata.height;

    size_t index = ((width * (height / 2)) + (width / 2));
    short middle = depthData[index];

    uint32_t frameIndex;
    sensekit_depthframe_get_frameindex(depthFrame, &frameIndex);
    std::cout << "index: " << frameIndex << " value: " << middle << std::endl;
}

void frame_ready(sensekit_reader_t reader, sensekit_reader_frame_t frame)
{
    sensekit_depthframe_t depthFrame;
    sensekit_depth_frame_get(frame, &depthFrame);

    print_depth(depthFrame);
}

int main(int argc, char** argv)
{
    set_key_handler();

    sensekit_initialize();

    sensekit_streamset_t sensor;

    //client connects to daemon host, registers interest in certain sensor URI
    sensekit_streamset_open("localhost/device0", &sensor);

    sensekit_reader_t reader;
    sensekit_reader_create(sensor, &reader);

    //client -> daemon resolves stream type to plugin, notifies plugin client added
    //client service starts pulling (or daemon starts pushing) when data is available
    //client service stores latest frame until requested (via open or event)
    sensekit_depthstream_t depthStream;
    sensekit_depth_stream_get(reader, &depthStream);
    sensekit_stream_start(depthStream);

    sensekit_reader_callback_id_t callbackId;
    sensekit_reader_register_frame_ready_callback(reader, &frame_ready, &callbackId);

    do
    {
        sensekit_temp_update();

    } while (shouldContinue);

    sensekit_reader_unregister_frame_ready_callback(&callbackId);

    sensekit_reader_destroy(&reader);
    sensekit_streamset_close(&sensor);

    sensekit_terminate();
}
