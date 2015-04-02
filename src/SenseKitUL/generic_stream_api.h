#ifndef GENERIC_STREAM_API_H
#define GENERIC_STREAM_API_H

#include "sensekit_core.h"

template<typename TStreamType>
sensekit_status_t sensekit_generic_stream_get(sensekit_reader_t* reader,
                                              TStreamType** stream,
                                              sensekit_stream_type_t type,
                                              sensekit_stream_subtype_t subType)
{
    sensekit_streamconnection_t* connection;
    sensekit_reader_get_stream(reader, type, subType, &connection);

    *stream = reinterpret_cast<TStreamType*>(connection);

    return SENSEKIT_STATUS_SUCCESS;
}

template<typename TStreamType>
sensekit_status_t sensekit_generic_stream_start(TStreamType* stream)
{
    sensekit_streamconnection_t* connection = reinterpret_cast<sensekit_streamconnection_t*>(stream);
    return sensekit_stream_start(connection);
}

template<typename TStreamType>
sensekit_status_t sensekit_generic_stream_stop(TStreamType* stream)
{
    sensekit_streamconnection_t* connection = reinterpret_cast<sensekit_streamconnection_t*>(stream);
    return sensekit_stream_stop(connection);
}

template<typename TFrameWrapperType, typename TStreamType, typename TFrameType>
sensekit_status_t sensekit_generic_frame_open(TStreamType* stream, int timeout, TFrameType** frame)
{
    sensekit_frame_ref_t* frameRef;
    sensekit_streamconnection_t* connection = reinterpret_cast<sensekit_streamconnection_t*>(stream);

    sensekit_stream_frame_open(connection, timeout, &frameRef);

    TFrameWrapperType* wrapper = reinterpret_cast<TFrameWrapperType*>(frameRef->frame->data);
    *frame = reinterpret_cast<TFrameType*>(&(wrapper->frame));
    (*frame)->frameRef = frameRef;

    return SENSEKIT_STATUS_SUCCESS;
}

template<typename TStreamType, typename TFrameType>
sensekit_status_t sensekit_generic_frame_open(TStreamType* stream, int timeout, TFrameType** frame)
{
    sensekit_frame_ref_t* frameRef;
    sensekit_streamconnection_t* connection = reinterpret_cast<sensekit_streamconnection_t*>(stream);

    sensekit_stream_frame_open(connection, timeout, &frameRef);

    *frame = reinterpret_cast<TFrameType*>(frameRef->frame->data);
    (*frame)->frameRef = frameRef;

    return SENSEKIT_STATUS_SUCCESS;
}

template<typename TFrameType>
sensekit_status_t sensekit_generic_frame_close(TFrameType** frame)
{
    sensekit_frame_ref_t* sk_frameRef = (*frame)->frameRef;

    sensekit_stream_frame_close(&sk_frameRef);

    *frame = nullptr;

    return SENSEKIT_STATUS_SUCCESS;
}


#endif /* GENERIC_STREAM_API_H */
