#include "SenseKit-private.h"
#include "SenseKitContext.h"

static sensekit::SenseKitContext g_Context;

SENSEKIT_BEGIN_DECLS

SENSEKIT_API void sensekit_initialize()
{
    g_Context.initialize();
}

SENSEKIT_API void sensekit_terminate()
{
    g_Context.terminate();
}

SENSEKIT_API sensekit_status_t sensekit_open_streamset(const char* connection_string, /*out*/ sensekit_streamset_t** streamset)
{
    return g_Context.open_streamset(connection_string, streamset);
}

SENSEKIT_API sensekit_status_t sensekit_close_streamset(sensekit_streamset_t** streamset)
{
    return g_Context.close_streamset(streamset);
}

SENSEKIT_API sensekit_status_t sensekit_stream_open(sensekit_streamset_t* streamset, sensekit_stream_t** stream)
{
    g_Context.open_stream(streamset, stream);

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API sensekit_status_t sensekit_stream_close(sensekit_stream_t** stream)
{
    g_Context.close_stream(stream);

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API sensekit_status_t sensekit_stream_frame_open(sensekit_stream_t* stream, int timeout, sensekit_frame_t** frame)
{
    g_Context.open_frame(stream, timeout, *frame);

    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API sensekit_status_t sensekit_stream_frame_close(sensekit_frame_t** frame)
{
    g_Context.close_frame(*frame);
    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_API sensekit_status_t sensekit_temp_update()
{
    return g_Context.temp_update();
}

SENSEKIT_END_DECLS
