// Orbbec (c) 2015

#include <Astra/astra_capi.h>
#include <AstraUL/skul_capi.h>
#include <stdio.h>
#include <key_handler.h>

void print_color(astra_colorframe_t colorFrame)
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

void frame_ready(void* clientTag, astra_reader_t reader, astra_reader_frame_t frame)
{
	astra_colorframe_t colorFrame;
	astra_frame_get_colorframe(frame, &colorFrame);

	print_color(colorFrame);
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
	astra_colorstream_t colorStream;
	astra_reader_get_colorstream(reader, &colorStream);
	astra_stream_start(colorStream);

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
