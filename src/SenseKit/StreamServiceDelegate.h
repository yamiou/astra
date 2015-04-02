#ifndef STREAMSERVICEDELEGATE_H
#define STREAMSERVICEDELEGATE_H

#include <sensekit_core.h>
#include "SenseKitContext.h"

namespace sensekit {

    class StreamServiceDelegate
    {
    public:
        static sensekit_status_t open_streamset(void* service, const char* uri, sensekit_streamset_t** streamSet)
            {
                return static_cast<SenseKitContext*>(service)->open_streamset(uri, *streamSet);
            }

        static sensekit_status_t close_streamset(void* service, sensekit_streamset_t** streamSet)
            {
                return static_cast<SenseKitContext*>(service)->close_streamset(*streamSet);
            }

        static sensekit_status_t create_reader(void* service,
                                               sensekit_streamset_t* streamSet,
                                               sensekit_reader_t** reader)
            {
                return static_cast<SenseKitContext*>(service)->create_reader(streamSet, *reader);
            }

        static sensekit_status_t destroy_reader(void* service, sensekit_reader_t** reader)
            {
                return static_cast<SenseKitContext*>(service)->destroy_reader(*reader);
            }

        static sensekit_status_t get_stream(void* service,
                                            sensekit_reader_t* reader,
                                            sensekit_stream_type_t type,
                                            sensekit_stream_subtype_t subType,
                                            sensekit_streamconnection_t** connection)
            {
                return static_cast<SenseKitContext*>(service)->get_stream(reader, type, subType, *connection);
            }

        static sensekit_status_t start_stream(void* service, sensekit_streamconnection_t* connection)
            {
                return static_cast<SenseKitContext*>(service)->start_stream(connection);
            }

        static sensekit_status_t stop_stream(void* service, sensekit_streamconnection_t* connection)
            {
                return static_cast<SenseKitContext*>(service)->stop_stream(connection);
            }

        static sensekit_status_t open_frame(void* service,
                                            sensekit_streamconnection_t* connection,
                                            int timeoutMillis,
                                            sensekit_frame_ref_t** frameRef)
            {
                return static_cast<SenseKitContext*>(service)->open_frame(connection, timeoutMillis, *frameRef);
            }

        static sensekit_status_t close_frame(void* service, sensekit_frame_ref_t** frameRef)
            {
                return static_cast<SenseKitContext*>(service)->close_frame(*frameRef);
            }

        static sensekit_status_t temp_update(void* service)
            {
                return static_cast<SenseKitContext*>(service)->temp_update();
            }

        static sensekit_status_t set_parameter(void* service,
                                               sensekit_streamconnection_t* connection,
                                               sensekit_parameter_id parameterId,
                                               size_t byteLength,
                                               sensekit_parameter_data_t* data)
            {
                return static_cast<SenseKitContext*>(service)->set_parameter(connection, parameterId, byteLength, data);
            }

        static sensekit_status_t get_parameter_size(void* service,
                                                    sensekit_streamconnection_t* connection,
                                                    sensekit_parameter_id parameterId, size_t* byteLength)
            {
                return static_cast<SenseKitContext*>(service)->get_parameter_size(connection,
                                                                                  parameterId,
                                                                                  *byteLength);
            }

        static sensekit_status_t get_parameter_data(void* service,
                                                    sensekit_streamconnection_t* connection,
                                                    sensekit_parameter_id parameterId,
                                                    size_t byteLength,
                                                    sensekit_parameter_data_t* data)
            {
                return static_cast<SenseKitContext*>(service)->get_parameter_data(connection,
                                                                                  parameterId,
                                                                                  byteLength,
                                                                                  data);
            }
    };
}

#endif /* STREAMSERVICEDELEGATE_H */
