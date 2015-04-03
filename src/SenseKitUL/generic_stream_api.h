#ifndef GENERIC_STREAM_API_H
#define GENERIC_STREAM_API_H

#include "sensekit_core.h"

template<typename TStreamType>
sensekit_status_t sensekit_generic_stream_get(sensekit_reader_t* reader,
                                              sensekit_stream_type_t type,
                                              sensekit_stream_subtype_t subType,
                                              TStreamType** stream)
{
    sensekit_streamconnection_t* connection;
    sensekit_reader_get_stream(reader, type, subType, &connection);

    *stream = reinterpret_cast<TStreamType*>(connection);

    return SENSEKIT_STATUS_SUCCESS;
}

template<typename TFrameWrapperType, typename TFrameType>
sensekit_status_t sensekit_generic_frame_get(sensekit_reader_frame_t* readerFrame,
                                             sensekit_stream_type_t type,
                                             sensekit_stream_subtype_t subType,
                                             TFrameType** frame)
{
    sensekit_frame_ref_t* frameRef;
    sensekit_reader_get_frame(readerFrame, type, subType, &frameRef);

    TFrameWrapperType* wrapper = reinterpret_cast<TFrameWrapperType*>(frameRef->frame->data);
    *frame = reinterpret_cast<TFrameType*>(&(wrapper->frame));
    (*frame)->frameRef = frameRef;

    return SENSEKIT_STATUS_SUCCESS;
}

template<typename TFrameType>
sensekit_status_t sensekit_generic_frame_get(sensekit_reader_frame_t* readerFrame,
                                             sensekit_stream_type_t type,
                                             sensekit_stream_subtype_t subType,
                                             TFrameType** frame)
{
    sensekit_frame_ref_t* frameRef;
    sensekit_reader_get_frame(readerFrame, type, subType, &frameRef);

    *frame = reinterpret_cast<TFrameType*>(frameRef->frame->data);
    (*frame)->frameRef = frameRef;

    return SENSEKIT_STATUS_SUCCESS;
}


#endif /* GENERIC_STREAM_API_H */
