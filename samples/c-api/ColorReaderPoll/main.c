// Orbbec (c) 2015

#include <Astra/astra_capi.h>
#include <AstraUL/skul_capi.h>
#include <stdio.h>
#include <key_handler.h>

void print_color(astra_depthframe_t colorFrame)
{
	astra_image_metadata_t metadata;
	uint8_t* colorData;
	size_t colorLength;

	astra_colorframe_get_data_ptr(colorFrame, &colorData, &colorLength);
	astra_colorframe_get_metadata(colorFrame, &metadata);

	int width = metadata.width;
	int height = metadata.height;
	size_t index = ((width * (height / 2)) + (width / 2));

	astra_frame_index_t frameIndex;
	astra_colorframe_get_frameindex(colorFrame, &frameIndex);

	colorData = colorData + index * sizeof(uint8_t) * 3;
	uint8_t r = *colorData++;
	uint8_t g = *colorData++;
	uint8_t b = *colorData++;

	printf("color frameIndex: %d  r: %d    g: %d    b: %d \n", frameIndex, r, g, b);	
}

int main(int argc, char* argv[])
{
    set_key_handler();

    astra_initialize();

    astra_streamsetconnection_t sensor;

    astra_streamset_open("device/default", &sensor);

    astra_reader_t reader;
    astra_reader_create(sensor, &reader);

	astra_depthstream_t colorStream;
	astra_reader_get_colorstream(reader, &colorStream);

	astra_stream_start(colorStream);

    astra_frame_index_t lastFrameIndex = -1;

    do
    {
        astra_temp_update();

        astra_reader_frame_t frame;
        astra_status_t rc = astra_reader_open_frame(reader, 0, &frame);

        if (rc == ASTRA_STATUS_SUCCESS)
        {
			astra_colorframe_t colorFrame;
			astra_frame_get_colorframe(frame, &colorFrame);

            astra_frame_index_t newFrameIndex;
			astra_colorframe_get_frameindex(colorFrame, &newFrameIndex);

            if (lastFrameIndex == newFrameIndex)
            {
                printf("duplicate frame index: %d\n", lastFrameIndex);
            }
            lastFrameIndex = newFrameIndex;

			print_color(colorFrame);

            astra_reader_close_frame(&frame);
        }

    } while (shouldContinue);

    astra_reader_destroy(&reader);
    astra_streamset_close(&sensor);

    astra_terminate();
}
