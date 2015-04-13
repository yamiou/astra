// Orbbec (c) 2015

#include <SenseKit/sensekit_capi.h>
#include <SenseKitUL/skul_capi.h>
#include <stdio.h>
#include "../key_handler.h"

void print_hands(sensekit_handframe_t handFrame)
{
    sensekit_handpoint_t* handPoints;
    size_t numHands;

    sensekit_handframe_get_num_hands(handFrame, &numHands);

    handPoints = (sensekit_handpoint_t*)malloc(numHands * sizeof(sensekit_handpoint_t));
    sensekit_handframe_copy_hands(handFrame, handPoints);

    sensekit_frame_index_t frameIndex;
    sensekit_handframe_get_frameindex(handFrame, &frameIndex);

    int numActiveHands = 0;

    sensekit_handpoint_t* point = handPoints;
    for (int i = 0; i < numHands; i++)
    {
        if (point->status == HAND_STATUS_TRACKING)
        {
            ++numActiveHands;
        }
        ++point;
    }

    free(handPoints);

    printf("index %d activeHands %d\n", frameIndex, numActiveHands);
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
            sensekit_hand_get_frame(frame, &handFrame);

            print_hands(handFrame);

            sensekit_colorframe_t handDebugImageFrame;
            sensekit_hand_debug_image_get_frame(frame, &handDebugImageFrame);

            sensekit_colorframe_metadata_t metadata;
            sensekit_colorframe_get_metadata(handDebugImageFrame, &metadata);

            sensekit_reader_close_frame(&frame);
        }

    } while (shouldContinue);
}

int main(int argc, char* argv[])
{
    set_key_handler();

    sensekit_initialize();

    sensekit_streamset_t sensor;

    sensekit_streamset_open("localhost/device0", &sensor);

    sensekit_reader_t reader;
    sensekit_reader_create(sensor, &reader);

    sensekit_handstream_t handStream;
    sensekit_hand_get_stream(reader, &handStream);
    sensekit_stream_start(handStream);

    sensekit_colorstream_t handDebugImageStream;
    sensekit_hand_debug_image_get_stream(reader, &handDebugImageStream);
    sensekit_stream_start(handDebugImageStream);

    runHandStream(reader);

    sensekit_reader_destroy(&reader);
    sensekit_streamset_close(&sensor);

    sensekit_terminate();
}
