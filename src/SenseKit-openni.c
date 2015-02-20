#include "SenseKit-openni-private.h"
#include "SenseKit-private.h"

void _openni_initialize(sensekit_sensor_adapter_t* adapter)
{
	adapter->handle = _create_openni_adapter();
}

void _openni_destroy(sensekit_sensor_adapter_t* adapter)
{
	if (adapter->handle != NULL)
	{
		_destroy_openni_adapter(adapter->handle);
	}
}

static sensekit_sensor_adapter_t _default_openni_sensor_adapter = {
	NULL,
	_openni_initialize,
	_openni_destroy
};

sensekit_sensor_adapter_t * _create_openni_sensor_adapter()
{
	sensekit_sensor_adapter_t* adapter = (sensekit_sensor_adapter_t*)malloc(sizeof(sensekit_sensor_adapter_t));
	memcpy(adapter, (void*)&_default_openni_sensor_adapter, sizeof(sensekit_sensor_adapter_t));
	adapter->handle = _create_openni_adapter();

	return adapter;
}

void _destroy_openni_sensor_adapter(sensekit_sensor_adapter_t * adapter)
{
	adapter->destroy(adapter);
	adapter->handle = NULL;

	free(adapter);
}

sensekit_sensor_t * _create_openni_sensekit_sensor(void)
{
	sensekit_sensor_t* sensor;
	sensor = (sensekit_sensor_t*)malloc(sizeof(sensekit_sensor_t));
	sensor->adapter = _create_openni_sensor_adapter();

	return sensor;
}

void _destroy_openni_sensekit_sensor(sensekit_sensor_t * sensor)
{
	if (sensor == NULL)
	{
		return;
	}

	_destroy_openni_sensor_adapter(sensor->adapter);
	sensor->adapter = NULL;

	free(sensor);
}

sensekit_depthstream_t * _open_openni_depthstream(sensekit_sensor_t * sensor)
{
	return NULL;
}

void _close_openni_depthstream(sensekit_depthstream_t * depthstream)
{

}
