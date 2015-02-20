#ifndef SENSEKIT_OPENNI_H
#define SENSEKIT_OPENNI_H

#include <SenseKit.h>
#include "SenseKit-adapter-private.h"

sensor_handle_t* _create_openni_adapter();
void _destroy_openni_adapter(sensor_handle_t* handle);

sensekit_sensor_t * _create_openni_sensekit_sensor(void);
void _destroy_openni_sensekit_sensor(sensekit_sensor_t * sensor);

sensekit_depthstream_t * _open_openni_depthstream(sensekit_sensor_t * sensor);
void _close_openni_depthstream(sensekit_depthstream_t * depthstream);

#endif // SENSEKIT_OPENNI_H
