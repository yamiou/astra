// Orbbec (c) 2015

#include <Astra/astra_capi.h>
#include <AstraUL/astraul_capi.h>
#include <stdio.h>
#include <key_handler.h>

void print_depth(astra_depthframe_t depthFrame)
{
    astra_image_metadata_t metadata;
    int16_t* depthData;
    size_t depthLength;

    astra_depthframe_get_data_ptr(depthFrame, &depthData, &depthLength);
    astra_depthframe_get_metadata(depthFrame, &metadata);

    int width = metadata.width;
    int height = metadata.height;

    size_t index = ((width * (height / 2)) + (width / 2));
    short middle = depthData[index];

    astra_frame_index_t frameIndex;
    astra_depthframe_get_frameindex(depthFrame, &frameIndex);

	printf("index:  %d  value:  %d \n", frameIndex, middle);
}

void frame_ready(void* clientTag, astra_reader_t reader, astra_reader_frame_t frame)
{
    astra_depthframe_t depthFrame;
    astra_frame_get_depthframe(frame, &depthFrame);

    print_depth(depthFrame);
}

int main(int argc, char* argv[])
{
    set_key_handler();

    astra_initialize();

    astra_streamsetconnection_t sensor;

    //client connects to daemon host, registers interest in certain sensor URI
    astra_streamset_open("device/default", &sensor);

    astra_reader_t reader;
    astra_reader_create(sensor, &reader);

    //client -> daemon resolves stream type to plugin, notifies plugin client added
    //client service starts pulling (or daemon starts pushing) when data is available
    //client service stores latest frame until requested (via open or event)
    astra_depthstream_t depthStream;
    astra_reader_get_depthstream(reader, &depthStream);
    astra_stream_start(depthStream);

    astra_reader_callback_id_t callbackId;
    astra_reader_register_frame_ready_callback(reader, &frame_ready, NULL, &callbackId);

    do
    {
        astra_temp_update();

    } while (shouldContinue);

    astra_reader_unregister_frame_ready_callback(&callbackId);

    astra_reader_destroy(&reader);
    astra_streamset_close(&sensor);

    astra_terminate();
}
