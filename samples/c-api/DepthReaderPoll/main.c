// Orbbec (c) 2015

#include <SenseKit/sensekit_capi.h>
#include <SenseKitUL/skul_capi.h>
#include <stdio.h>
#include <key_handler.h>

void print_depth(sensekit_depthframe_t depthFrame)
{
    sensekit_image_metadata_t metadata;

    int16_t* depthData;
    size_t depthLength;

    sensekit_depthframe_get_data_byte_length(depthFrame, &depthLength);
    sensekit_depthframe_get_metadata(depthFrame, &metadata);

    depthData = (int16_t*)malloc(depthLength);
    sensekit_depthframe_copy_data(depthFrame, depthData);

    int width = metadata.width;
    int height = metadata.height;

    size_t index = ((width * (height / 2)) + (width / 2));
    short middle = depthData[index];

    free(depthData);

    sensekit_frame_index_t frameIndex;
    sensekit_depthframe_get_frameindex(depthFrame, &frameIndex);
    printf("index %d value %d\n", frameIndex, middle);
}

int main(int argc, char* argv[])
{
    set_key_handler();

    sensekit_initialize();

    sensekit_streamsetconnection_t sensor;

    sensekit_streamset_open("device/sensor0", &sensor);

    sensekit_reader_t reader;
    sensekit_reader_create(sensor, &reader);

    sensekit_depthstream_t depthStream;
    sensekit_reader_get_depthstream(reader, &depthStream);

    float hFov, vFov;
    sensekit_depthstream_get_hfov(depthStream, &hFov);
    sensekit_depthstream_get_vfov(depthStream, &vFov);

    printf("depth sensor -- hFov: %f radians vFov: %f radians\n", hFov, vFov);

    sensekit_stream_start(depthStream);

    sensekit_frame_index_t lastFrameIndex = -1;

    do
    {
        sensekit_temp_update();

        sensekit_reader_frame_t frame;
        sensekit_status_t rc = sensekit_reader_open_frame(reader, 0, &frame);

        if (rc == SENSEKIT_STATUS_SUCCESS)
        {
            sensekit_depthframe_t depthFrame;
            sensekit_frame_get_depthframe(frame, &depthFrame);

            sensekit_frame_index_t newFrameIndex;
            sensekit_depthframe_get_frameindex(depthFrame, &newFrameIndex);

            if (lastFrameIndex == newFrameIndex)
            {
                printf("duplicate frame index: %d\n", lastFrameIndex);
            }
            lastFrameIndex = newFrameIndex;

            print_depth(depthFrame);

            sensekit_reader_close_frame(&frame);
        }

    } while (false); //(shouldContinue);

    sensekit_reader_destroy(&reader);
    sensekit_streamset_close(&sensor);

    sensekit_terminate();
}
