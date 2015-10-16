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

    astra_depthframe_get_data_byte_length(depthFrame, &depthLength);
    astra_depthframe_get_metadata(depthFrame, &metadata);

    depthData = (int16_t*)malloc(depthLength);
    astra_depthframe_copy_data(depthFrame, depthData);

    int width = metadata.width;
    int height = metadata.height;

    size_t index = ((width * (height / 2)) + (width / 2));
    short middle = depthData[index];

    free(depthData);

    astra_frame_index_t frameIndex;
    astra_depthframe_get_frameindex(depthFrame, &frameIndex);
    printf("index %d value %d\n", frameIndex, middle);
}

int main(int argc, char* argv[])
{
    set_key_handler();

    astra_initialize();

    astra_streamsetconnection_t sensor;

    astra_streamset_open("device/default", &sensor);

    astra_reader_t reader;
    astra_reader_create(sensor, &reader);

    astra_depthstream_t depthStream;
    astra_reader_get_depthstream(reader, &depthStream);

    float hFov, vFov;
    astra_depthstream_get_hfov(depthStream, &hFov);
    astra_depthstream_get_vfov(depthStream, &vFov);

    printf("depth sensor -- hFov: %f radians vFov: %f radians\n", hFov, vFov);

    astra_stream_start(depthStream);

    astra_frame_index_t lastFrameIndex = -1;

    do
    {
        astra_temp_update();

        astra_reader_frame_t frame;
        astra_status_t rc = astra_reader_open_frame(reader, 0, &frame);

        if (rc == ASTRA_STATUS_SUCCESS)
        {
            astra_depthframe_t depthFrame;
            astra_frame_get_depthframe(frame, &depthFrame);

            astra_frame_index_t newFrameIndex;
            astra_depthframe_get_frameindex(depthFrame, &newFrameIndex);

            if (lastFrameIndex == newFrameIndex)
            {
                printf("duplicate frame index: %d\n", lastFrameIndex);
            }
            lastFrameIndex = newFrameIndex;

            print_depth(depthFrame);

            astra_reader_close_frame(&frame);
        }

    } while (shouldContinue);

    astra_reader_destroy(&reader);
    astra_streamset_close(&sensor);

    astra_terminate();
}
