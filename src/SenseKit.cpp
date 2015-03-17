#include "SenseKit-private.h"
#include "PluginContext.h"

static sensekit::PluginContext g_Context;

SENSEKIT_BEGIN_DECLS

SENSEKIT_API void sensekit_initialize()
{
    g_Context.initialize();
}

SENSEKIT_API void sensekit_terminate()
{
    g_Context.terminate();
}

SENSEKIT_API sensekit_status_t sensekit_open_sensor(const char* connection_string, /*out*/ sensekit_sensor_t** sensor)
{
    return g_Context.open_sensor(connection_string, sensor);
}

SENSEKIT_API sensekit_status_t sensekit_close_sensor(sensekit_sensor_t** sensor)
{
    return g_Context.close_sensor(sensor);
}

SENSEKIT_API sensekit_status_t sensekit_depth_open(sensekit_sensor_t* sensor, sensekit_depthstream_t** stream)
{
    g_Context.open_depth_stream(sensor, stream);

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API sensekit_status_t sensekit_depth_close(sensekit_depthstream_t** stream)
{
    g_Context.close_depth_stream(stream);

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API sensekit_status_t sensekit_depth_frame_open(sensekit_depthstream_t* stream, int timeout, sensekit_depthframe_t*& frame)
{

    g_Context.open_depth_frame(stream, timeout, frame);

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API sensekit_status_t sensekit_depth_frame_close(sensekit_depthframe_t*& frame)
{
    g_Context.close_depth_frame(frame);
    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API sensekit_status_t sensekit_temp_update()
{
    return g_Context.temp_update();
}

SENSEKIT_END_DECLS
