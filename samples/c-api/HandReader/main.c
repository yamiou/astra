// Orbbec (c) 2015

#include <SenseKit/sensekit_capi.h>
#include <SenseKitUL/skul_capi.h>
#include <stdio.h>
#include <key_handler.h>

void print_hand_frame(sensekit_handframe_t handFrame)
{
    sensekit_handpoint_t* handPoints;
    size_t handCount;

    sensekit_handframe_get_hand_count(handFrame, &handCount);

    handPoints = (sensekit_handpoint_t*)malloc(handCount * sizeof(sensekit_handpoint_t));
    sensekit_handframe_copy_hands(handFrame, handPoints);

    sensekit_frame_index_t frameIndex;
    sensekit_handframe_get_frameindex(handFrame, &frameIndex);

    int activeHandCount = 0;

    sensekit_handpoint_t* point = handPoints;

    int i;
    for (i = 0; i < handCount; i++)
    {
        if (point->status == HAND_STATUS_TRACKING)
        {
            ++activeHandCount;
        }
        ++point;
    }

    free(handPoints);

    printf("index %d active hand count %d\n", frameIndex, activeHandCount);
}

void runHandStream(sensekit_reader_t reader)
{
    do
    {
        sensekit_temp_update();

        sensekit_reader_frame_t frame;
        sensekit_status_t rc = sensekit_reader_open_frame(reader, 0, &frame);

        if (rc == SENSEKIT_STATUS_SUCCESS)
        {
            sensekit_handframe_t handFrame;
            sensekit_frame_get_handframe(frame, &handFrame);

            print_hand_frame(handFrame);

            sensekit_colorframe_t handDebugImageFrame;
            sensekit_frame_get_debug_handframe(frame, &handDebugImageFrame);

            sensekit_image_metadata_t metadata;
            sensekit_colorframe_get_metadata(handDebugImageFrame, &metadata);

            sensekit_reader_close_frame(&frame);
        }

    } while (shouldContinue);
}

int main(int argc, char* argv[])
{
    set_key_handler();

    sensekit_initialize();

    sensekit_streamsetconnection_t sensor;

    sensekit_streamset_open("device/default", &sensor);

    sensekit_reader_t reader;
    sensekit_reader_create(sensor, &reader);

    sensekit_handstream_t handStream;
    sensekit_reader_get_handstream(reader, &handStream);
    sensekit_stream_start(handStream);

    sensekit_colorstream_t handDebugImageStream;
    sensekit_reader_get_debug_handstream(reader, &handDebugImageStream);
    sensekit_stream_start(handDebugImageStream);

    runHandStream(reader);

    sensekit_reader_destroy(&reader);
    sensekit_streamset_close(&sensor);

    sensekit_terminate();
}
