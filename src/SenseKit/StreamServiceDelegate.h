/* THIS FILE AUTO-GENERATED FROM StreamServiceDelegate.h.lpp. DO NOT EDIT. */
#ifndef STREAMSERVICEDELEGATE_H
#define STREAMSERVICEDELEGATE_H

#include <SenseKit/sensekit_types.h>
#include "SenseKitContext.h"
#include <iostream>

namespace sensekit {

    class StreamServiceDelegate
    {
    public:

        static sensekit_status_t initialize(void* streamService)
        {
            return static_cast<SenseKitContext*>(streamService)->initialize();
        }

        static sensekit_status_t terminate(void* streamService)
        {
            return static_cast<SenseKitContext*>(streamService)->terminate();
        }

        static sensekit_status_t streamset_open(void* streamService,
                                                const char* connectionString,
                                                sensekit_streamset_t* streamSet)
        {
            return static_cast<SenseKitContext*>(streamService)->streamset_open(connectionString, *streamSet);
        }

        static sensekit_status_t streamset_close(void* streamService,
                                                 sensekit_streamset_t* streamSet)
        {
            return static_cast<SenseKitContext*>(streamService)->streamset_close(*streamSet);
        }

        static char* get_status_string(void* streamService,
                                       sensekit_status_t status)
        {
            return static_cast<SenseKitContext*>(streamService)->get_status_string(status);
        }

        static sensekit_status_t reader_create(void* streamService,
                                               sensekit_streamset_t streamSet,
                                               sensekit_reader_t* reader)
        {
            return static_cast<SenseKitContext*>(streamService)->reader_create(streamSet, *reader);
        }

        static sensekit_status_t reader_destroy(void* streamService,
                                                sensekit_reader_t* reader)
        {
            return static_cast<SenseKitContext*>(streamService)->reader_destroy(*reader);
        }

        static sensekit_status_t reader_get_stream(void* streamService,
                                                   sensekit_reader_t reader,
                                                   sensekit_stream_type_t type,
                                                   sensekit_stream_subtype_t subtype,
                                                   sensekit_streamconnection_t* connection)
        {
            return static_cast<SenseKitContext*>(streamService)->reader_get_stream(reader, type, subtype, *connection);
        }

        static sensekit_status_t stream_get_description(void* streamService,
                                                        sensekit_streamconnection_t connection,
                                                        sensekit_stream_desc_t* description)
        {
            return static_cast<SenseKitContext*>(streamService)->stream_get_description(connection, description);
        }

        static sensekit_status_t stream_start(void* streamService,
                                              sensekit_streamconnection_t connection)
        {
            return static_cast<SenseKitContext*>(streamService)->stream_start(connection);
        }

        static sensekit_status_t stream_stop(void* streamService,
                                             sensekit_streamconnection_t connection)
        {
            return static_cast<SenseKitContext*>(streamService)->stream_stop(connection);
        }

        static sensekit_status_t reader_open_frame(void* streamService,
                                                   sensekit_reader_t reader,
                                                   int timeoutMillis,
                                                   sensekit_reader_frame_t* frame)
        {
            return static_cast<SenseKitContext*>(streamService)->reader_open_frame(reader, timeoutMillis, *frame);
        }

        static sensekit_status_t reader_close_frame(void* streamService,
                                                    sensekit_reader_frame_t* frame)
        {
            return static_cast<SenseKitContext*>(streamService)->reader_close_frame(*frame);
        }

        static sensekit_status_t reader_register_frame_ready_callback(void* streamService,
                                                                      sensekit_reader_t reader,
                                                                      sensekit_frame_ready_callback_t callback,
                                                                      void* clientTag,
                                                                      sensekit_reader_callback_id_t* callbackId)
        {
            return static_cast<SenseKitContext*>(streamService)->reader_register_frame_ready_callback(reader, callback, clientTag, *callbackId);
        }

        static sensekit_status_t reader_unregister_frame_ready_callback(void* streamService,
                                                                        sensekit_reader_callback_id_t* callbackId)
        {
            return static_cast<SenseKitContext*>(streamService)->reader_unregister_frame_ready_callback(*callbackId);
        }

        static sensekit_status_t reader_get_frame(void* streamService,
                                                  sensekit_reader_frame_t frame,
                                                  sensekit_stream_type_t type,
                                                  sensekit_stream_subtype_t subtype,
                                                  sensekit_frame_ref_t** frameRef)
        {
            return static_cast<SenseKitContext*>(streamService)->reader_get_frame(frame, type, subtype, *frameRef);
        }

        static sensekit_status_t stream_set_parameter(void* streamService,
                                                      sensekit_streamconnection_t connection,
                                                      sensekit_parameter_id parameterId,
                                                      size_t inByteLength,
                                                      sensekit_parameter_data_t inData)
        {
            return static_cast<SenseKitContext*>(streamService)->stream_set_parameter(connection, parameterId, inByteLength, inData);
        }

        static sensekit_status_t stream_get_parameter(void* streamService,
                                                      sensekit_streamconnection_t connection,
                                                      sensekit_parameter_id parameterId,
                                                      size_t* resultByteLength,
                                                      sensekit_result_token_t* token)
        {
            return static_cast<SenseKitContext*>(streamService)->stream_get_parameter(connection, parameterId, *resultByteLength, *token);
        }

        static sensekit_status_t stream_get_result(void* streamService,
                                                   sensekit_streamconnection_t connection,
                                                   sensekit_result_token_t token,
                                                   size_t dataByteLength,
                                                   sensekit_parameter_data_t dataDestination)
        {
            return static_cast<SenseKitContext*>(streamService)->stream_get_result(connection, token, dataByteLength, dataDestination);
        }

        static sensekit_status_t stream_invoke(void* streamService,
                                               sensekit_streamconnection_t connection,
                                               sensekit_command_id commandId,
                                               size_t inByteLength,
                                               sensekit_parameter_data_t inData,
                                               size_t* resultByteLength,
                                               sensekit_result_token_t* token)
        {
            return static_cast<SenseKitContext*>(streamService)->stream_invoke(connection, commandId, inByteLength, inData, *resultByteLength, *token);
        }

        static sensekit_status_t temp_update(void* streamService)
        {
            return static_cast<SenseKitContext*>(streamService)->temp_update();
        }
    };
}

#endif /* STREAMSERVICEDELEGATE_H */
