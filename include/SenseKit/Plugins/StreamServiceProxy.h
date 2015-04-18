/* THIS FILE AUTO-GENERATED FROM StreamServiceProxy.h.lpp. DO NOT EDIT. */
#ifndef STREAMSERVICEPROXY_H
#define STREAMSERVICEPROXY_H

#include "StreamServiceProxyBase.h"

namespace sensekit {

    class StreamServiceProxy : public StreamServiceProxyBase
    {
    public:

        sensekit_status_t initialize()
        {
            return StreamServiceProxyBase::initialize(streamService);
        }

        sensekit_status_t terminate()
        {
            return StreamServiceProxyBase::terminate(streamService);
        }

        sensekit_status_t streamset_open(const char* connectionString,
                                         sensekit_streamset_t* streamSet)
        {
            return StreamServiceProxyBase::streamset_open(streamService, connectionString, streamSet);
        }

        sensekit_status_t streamset_close(sensekit_streamset_t* streamSet)
        {
            return StreamServiceProxyBase::streamset_close(streamService, streamSet);
        }

        char* get_status_string(sensekit_status_t status)
        {
            return StreamServiceProxyBase::get_status_string(streamService, status);
        }

        sensekit_status_t reader_create(sensekit_streamset_t streamSet,
                                        sensekit_reader_t* reader)
        {
            return StreamServiceProxyBase::reader_create(streamService, streamSet, reader);
        }

        sensekit_status_t reader_destroy(sensekit_reader_t* reader)
        {
            return StreamServiceProxyBase::reader_destroy(streamService, reader);
        }

        sensekit_status_t reader_get_stream(sensekit_reader_t reader,
                                            sensekit_stream_type_t type,
                                            sensekit_stream_subtype_t subtype,
                                            sensekit_streamconnection_t* connection)
        {
            return StreamServiceProxyBase::reader_get_stream(streamService, reader, type, subtype, connection);
        }

        sensekit_status_t stream_get_description(sensekit_streamconnection_t connection,
                                                 sensekit_stream_desc_t* description)
        {
            return StreamServiceProxyBase::stream_get_description(streamService, connection, description);
        }

        sensekit_status_t stream_start(sensekit_streamconnection_t connection)
        {
            return StreamServiceProxyBase::stream_start(streamService, connection);
        }

        sensekit_status_t stream_stop(sensekit_streamconnection_t connection)
        {
            return StreamServiceProxyBase::stream_stop(streamService, connection);
        }

        sensekit_status_t reader_open_frame(sensekit_reader_t reader,
                                            int timeoutMillis,
                                            sensekit_reader_frame_t* frame)
        {
            return StreamServiceProxyBase::reader_open_frame(streamService, reader, timeoutMillis, frame);
        }

        sensekit_status_t reader_close_frame(sensekit_reader_frame_t* frame)
        {
            return StreamServiceProxyBase::reader_close_frame(streamService, frame);
        }

        sensekit_status_t reader_register_frame_ready_callback(sensekit_reader_t reader,
                                                               sensekit_frame_ready_callback_t callback,
                                                               void* clientTag,
                                                               sensekit_reader_callback_id_t* callbackId)
        {
            return StreamServiceProxyBase::reader_register_frame_ready_callback(streamService, reader, callback, clientTag, callbackId);
        }

        sensekit_status_t reader_unregister_frame_ready_callback(sensekit_reader_callback_id_t* callbackId)
        {
            return StreamServiceProxyBase::reader_unregister_frame_ready_callback(streamService, callbackId);
        }

        sensekit_status_t reader_get_frame(sensekit_reader_frame_t frame,
                                           sensekit_stream_type_t type,
                                           sensekit_stream_subtype_t subtype,
                                           sensekit_frame_ref_t** frameRef)
        {
            return StreamServiceProxyBase::reader_get_frame(streamService, frame, type, subtype, frameRef);
        }

        sensekit_status_t stream_set_parameter(sensekit_streamconnection_t connection,
                                               sensekit_parameter_id parameterId,
                                               size_t inByteLength,
                                               sensekit_parameter_data_t inData)
        {
            return StreamServiceProxyBase::stream_set_parameter(streamService, connection, parameterId, inByteLength, inData);
        }

        sensekit_status_t stream_get_parameter(sensekit_streamconnection_t connection,
                                               sensekit_parameter_id parameterId,
                                               size_t* resultByteLength,
                                               sensekit_result_token_t* token)
        {
            return StreamServiceProxyBase::stream_get_parameter(streamService, connection, parameterId, resultByteLength, token);
        }

        sensekit_status_t stream_get_result(sensekit_streamconnection_t connection,
                                            sensekit_result_token_t token,
                                            size_t dataByteLength,
                                            sensekit_parameter_data_t dataDestination)
        {
            return StreamServiceProxyBase::stream_get_result(streamService, connection, token, dataByteLength, dataDestination);
        }

        sensekit_status_t stream_invoke(sensekit_streamconnection_t connection,
                                        sensekit_command_id commandId,
                                        size_t inByteLength,
                                        sensekit_parameter_data_t inData,
                                        size_t* resultByteLength,
                                        sensekit_result_token_t* token)
        {
            return StreamServiceProxyBase::stream_invoke(streamService, connection, commandId, inByteLength, inData, resultByteLength, token);
        }

        sensekit_status_t temp_update()
        {
            return StreamServiceProxyBase::temp_update(streamService);
        }
    };
}

#endif /* STREAMSERVICEPROXY_H */
