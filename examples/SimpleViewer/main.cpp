// Orbbec (c) 2015

#include <SenseKit.h>

int main(int argc, char** argv)
{
	sensekit_sensor_t* sensor;
	sensekit_status_t status = SENSEKIT_STATUS_SUCCESS;

	status = sensekit_open_sensor("local", &sensor);

	sensekit_depthstream_t* depthStream;
	status = sensekit_depth_open(sensor, &depthStream);


	status = sensekit_depth_close(&depthStream);

	status = sensekit_close_sensor(&sensor);
}
