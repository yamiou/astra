#include "SenseKit-private.h"
#include "Context.h"

static sensekit::Context g_Context;

SENSEKIT_BEGIN_DECLS

SENSEKIT_API sensekit_status_t sensekit_open_sensor(char* connection_string, /*out*/ sensekit_sensor_t** sensor)
{
    return g_Context.open_sensor(connection_string, sensor);
}

SENSEKIT_API sensekit_status_t sensekit_close_sensor(sensekit_sensor_t** sensor)
{
    return g_Context.close_sensor(sensor);
}

SENSEKIT_API sensekit_status_t sensekit_depth_open(sensekit_sensor_t* sensor, sensekit_depthstream_t** stream)
{
    if (NULL == sensor)
            return SENSEKIT_STATUS_INVALID_PARAMETER;

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API sensekit_status_t sensekit_depth_close(sensekit_depthstream_t** stream)
{
    if (NULL == stream)
            return SENSEKIT_STATUS_INVALID_PARAMETER;

    stream = NULL;

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_END_DECLS
