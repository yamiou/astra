#include "SenseKit-private.h"
#include "SenseKit-openni-private.h"

SENSEKIT_API sensekit_status_t sensekit_open_sensor(char* connection_string, /*out*/ sensekit_sensor_t** sensor)
{
	if (NULL == connection_string)
		return SENSEKIT_STATUS_INVALID_PARAMETER;

	*sensor = _create_openni_sensekit_sensor();

	return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API sensekit_status_t sensekit_close_sensor(sensekit_sensor_t * sensor)
{
	if (NULL == sensor)
		return SENSEKIT_STATUS_INVALID_PARAMETER;

	_destroy_openni_sensekit_sensor(sensor);

	return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API sensekit_status_t sensekit_depth_open(sensekit_sensor_t* sensor, sensekit_depthstream_t** stream)
{
	if (NULL == sensor)
		return SENSEKIT_STATUS_INVALID_PARAMETER;

	*stream = _open_openni_depthstream(sensor);

	return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API sensekit_status_t sensekit_depth_close(sensekit_depthstream_t** stream)
{
	if (NULL == stream)
		return SENSEKIT_STATUS_INVALID_PARAMETER;

	_close_openni_depthstream(*stream);
	stream = NULL;

	return SENSEKIT_STATUS_SUCCESS;
}