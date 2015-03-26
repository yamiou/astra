#ifndef GENERIC_STREAM_API_H
#define GENERIC_STREAM_API_H

#include "sensekit_types.h"

template<typename TStreamType>
sensekit_status_t sensekit_generic_stream_open(sensekit_streamset_t* streamset, TStreamType** stream,
                                      sensekit_stream_type_t type, sensekit_stream_subtype_t subType)
{
    sensekit_streamconnection_t* stream_connection;
    sensekit_stream_open(streamset, type, subType, &stream_connection);

    *stream = reinterpret_cast<TStreamType*>(stream_connection);

    return SENSEKIT_STATUS_SUCCESS;
}

template<typename TStreamType>
sensekit_status_t sensekit_generic_stream_closes(TStreamType** stream)
{
    sensekit_streamconnection_t* sk_stream_connection = (sensekit_streamconnection_t*)(*stream);

    sensekit_stream_close(&sk_stream_connection);

    *stream = reinterpret_cast<TStreamType*>(sk_stream_connection);
    return SENSEKIT_STATUS_SUCCESS;
}

template<typename TFrameWrapperType, typename TStreamType, typename TFrameType>
sensekit_status_t sensekit_generic_frame_open(TStreamType* stream, int timeout, TFrameType** frame)
{
    sensekit_frame_ref_t* frameRef;
    sensekit_streamconnection_t* sk_stream_connection = (sensekit_streamconnection_t*)(stream);

    sensekit_stream_frame_open(sk_stream_connection, timeout, &frameRef);

    //SOOON...
    //sensekit_depthframe_header_t* header = (sensekit_depthframe_header_t*)(sk_frame->data);
    //interrogate header, optionally decompress, etc...

    //if (header->compressed)
    //{
    //*frame = codec_decompress((sensekit_depthframe_compressed_t*)(sk_frame->data));
    //}
    //else
    //{
    TFrameWrapperType* wrapper = reinterpret_cast<TFrameWrapperType*>(frameRef->frame->data);
    *frame = reinterpret_cast<TFrameType*>(&(wrapper->frame));
    (*frame)->frameRef = frameRef;
    //}
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
