/* THIS FILE AUTO-GENERATED FROM sensekit_capi.h.lpp. DO NOT EDIT. */
#ifndef SENSEKIT_CAPI_H
#define SENSEKIT_CAPI_H

#include "SenseKit/sensekit_defines.h"
#include "SenseKit/sensekit_types.h"

SENSEKIT_BEGIN_DECLS

SENSEKIT_API sensekit_status_t sensekit_initialize();

SENSEKIT_API sensekit_status_t sensekit_terminate();

SENSEKIT_API sensekit_status_t sensekit_streamset_open(const char* connectionString,
                                                       sensekit_streamset_t* streamSet);

SENSEKIT_API sensekit_status_t sensekit_streamset_close(sensekit_streamset_t* streamSet);

SENSEKIT_API char* sensekit_get_status_string(sensekit_status_t status);

SENSEKIT_API sensekit_status_t sensekit_reader_create(sensekit_streamset_t streamSet,
                                                      sensekit_reader_t* reader);

SENSEKIT_API sensekit_status_t sensekit_reader_destroy(sensekit_reader_t* reader);

SENSEKIT_API sensekit_status_t sensekit_reader_get_stream(sensekit_reader_t reader,
                                                          sensekit_stream_type_t type,
                                                          sensekit_stream_subtype_t subtype,
                                                          sensekit_streamconnection_t* connection);

SENSEKIT_API sensekit_status_t sensekit_stream_get_description(sensekit_streamconnection_t connection,
                                                               sensekit_stream_desc_t* description);

SENSEKIT_API sensekit_status_t sensekit_stream_start(sensekit_streamconnection_t connection);

SENSEKIT_API sensekit_status_t sensekit_stream_stop(sensekit_streamconnection_t connection);

SENSEKIT_API sensekit_status_t sensekit_reader_open_frame(sensekit_reader_t reader,
                                                          int timeoutMillis,
                                                          sensekit_reader_frame_t* frame);

SENSEKIT_API sensekit_status_t sensekit_reader_close_frame(sensekit_reader_frame_t* frame);

SENSEKIT_API sensekit_status_t sensekit_reader_register_frame_ready_callback(sensekit_reader_t reader,
                                                                             sensekit_frame_ready_callback_t callback,
                                                                             void* clientTag,
                                                                             sensekit_reader_callback_id_t* callbackId);

SENSEKIT_API sensekit_status_t sensekit_reader_unregister_frame_ready_callback(sensekit_reader_callback_id_t* callbackId);

SENSEKIT_API sensekit_status_t sensekit_reader_get_frame(sensekit_reader_frame_t frame,
                                                         sensekit_stream_type_t type,
                                                         sensekit_stream_subtype_t subtype,
                                                         sensekit_frame_ref_t** frameRef);

SENSEKIT_API sensekit_status_t sensekit_stream_set_parameter(sensekit_streamconnection_t connection,
                                                             sensekit_parameter_id parameterId,
                                                             size_t inByteLength,
                                                             sensekit_parameter_data_t inData);

SENSEKIT_API sensekit_status_t sensekit_stream_get_parameter(sensekit_streamconnection_t connection,
                                                             sensekit_parameter_id parameterId,
                                                             size_t* resultByteLength,
                                                             sensekit_result_token_t* token);

SENSEKIT_API sensekit_status_t sensekit_stream_get_result(sensekit_streamconnection_t connection,
                                                          sensekit_result_token_t token,
                                                          size_t dataByteLength,
                                                          sensekit_parameter_data_t dataDestination);

SENSEKIT_API sensekit_status_t sensekit_stream_invoke(sensekit_streamconnection_t connection,
                                                      sensekit_command_id commandId,
                                                      size_t inByteLength,
                                                      sensekit_parameter_data_t inData,
                                                      size_t* resultByteLength,
                                                      sensekit_result_token_t* token);

SENSEKIT_API sensekit_status_t sensekit_temp_update();

SENSEKIT_END_DECLS

#endif /* SENSEKIT_CAPI_H */
