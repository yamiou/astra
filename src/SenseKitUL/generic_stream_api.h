#ifndef GENERIC_STREAM_API_H
#define GENERIC_STREAM_API_H

#include <SenseKit/sensekit_capi.h>
#include <SenseKit/Plugins/plugin_capi.h>
#include <cassert>
#include <string.h>

template<typename TFrameWrapperType, typename TFrameType>
sensekit_status_t sensekit_generic_frame_get(sensekit_reader_frame_t readerFrame,
                                             sensekit_stream_type_t type,
                                             sensekit_stream_subtype_t subtype,
                                             TFrameType** frame)
{
    sensekit_frame_ref_t* frameRef;
    sensekit_reader_get_frame(readerFrame, type, subtype, &frameRef);

    TFrameWrapperType* wrapper = reinterpret_cast<TFrameWrapperType*>(frameRef->frame->data);
    *frame = reinterpret_cast<TFrameType*>(&(wrapper->frame));
    (*frame)->frameRef = frameRef;

    return SENSEKIT_STATUS_SUCCESS;
}

template<typename TFrameType>
sensekit_status_t sensekit_generic_frame_get(sensekit_reader_frame_t readerFrame,
                                             sensekit_stream_type_t type,
                                             sensekit_stream_subtype_t subtype,
                                             TFrameType** frame)
{
    sensekit_frame_ref_t* frameRef;
    sensekit_reader_get_frame(readerFrame, type, subtype, &frameRef);

    *frame = reinterpret_cast<TFrameType*>(frameRef->frame->data);
    (*frame)->frameRef = frameRef;

    return SENSEKIT_STATUS_SUCCESS;
}

template<typename TFrameType>
sensekit_status_t sensekit_generic_frame_get_frameindex(TFrameType* frame,
                                                        sensekit_frame_index_t* index)
{
    *index = frame->frameRef->frame->frameIndex;

    return SENSEKIT_STATUS_SUCCESS;
}


inline sensekit_status_t sensekit_stream_get_parameter_fixed(sensekit_streamconnection_t connection,
                                                             sensekit_parameter_id parameterId,
                                                             size_t byteLength,
                                                             sensekit_parameter_data_t* data)
{
    sensekit_result_token_t token;
    size_t paramSize;
    sensekit_status_t rc = sensekit_stream_get_parameter(connection,
                                                         parameterId,
                                                         &paramSize,
                                                         &token);

    if (rc != SENSEKIT_STATUS_SUCCESS)
    {
        memset(data, 0, byteLength);
        return rc;
    }

    assert(paramSize == byteLength);

    return sensekit_stream_get_result(connection,
                                      token,
                                      byteLength,
                                      data);
}

#endif /* GENERIC_STREAM_API_H */
