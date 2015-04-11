#ifndef STREAMCALLBACKTARGET_H
#define STREAMCALLBACKTARGET_H

#include <sensekit_types.h>
#include "plugin_api.h"

namespace sensekit {

    class StreamCallbackListener
    {
    public:
        virtual ~StreamCallbackListener() = default;
    private:

        static void set_parameter_thunk(void* instance,
                                        sensekit_streamconnection_t connection,
                                        sensekit_parameter_id id,
                                        size_t byteLength,
                                        sensekit_parameter_data_t* data)
            {
                static_cast<StreamCallbackListener*>(instance)->set_parameter(connection, id, byteLength, data);
            }

        static void get_parameter_size_thunk(void* instance,
                                             sensekit_streamconnection_t connection,
                                             sensekit_parameter_id id,
                                             size_t* byteLength)
            {
                static_cast<StreamCallbackListener*>(instance)->get_parameter_size(connection, id, *byteLength);
            }

        static void get_parameter_data_thunk(void* instance,
                                             sensekit_streamconnection_t connection,
                                             sensekit_parameter_id id,
                                             size_t byteLength,
                                             sensekit_parameter_data_t* data)
            {
                static_cast<StreamCallbackListener*>(instance)->get_parameter_data(connection, id, byteLength, data);
            }

        static void connection_added_thunk(void* instance,
                                           sensekit_stream_t stream,
                                           sensekit_streamconnection_t connection)
            {
                static_cast<StreamCallbackListener*>(instance)->connection_added(stream, connection);
            }

        static void connection_removed_thunk(void* instance,
                                             sensekit_stream_t stream,
                                             sensekit_bin_t bin,
                                             sensekit_streamconnection_t connection)
            {
                static_cast<StreamCallbackListener*>(instance)->connection_removed(stream, bin, connection);
            }

        virtual void set_parameter(sensekit_streamconnection_t connection,
                                   sensekit_parameter_id id,
                                   size_t byteLength,
                                   sensekit_parameter_data_t* data) {}

        virtual void get_parameter_size(sensekit_streamconnection_t connection,
                                        sensekit_parameter_id id,
                                        size_t& byteLength) {}

        virtual void get_parameter_data(sensekit_streamconnection_t connection,
                                        sensekit_parameter_id id,
                                        size_t byteLength,
                                        sensekit_parameter_data_t* data) {}

        virtual void connection_added(sensekit_stream_t stream, sensekit_streamconnection_t connection) {}
        virtual void connection_removed(sensekit_stream_t stream, sensekit_bin_t bin, sensekit_streamconnection_t connection) {}

        friend stream_callbacks_t create_plugin_callbacks(StreamCallbackListener* context);
    };

    inline stream_callbacks_t create_plugin_callbacks(StreamCallbackListener* context)
    {
        stream_callbacks_t callbacks;

        callbacks.context = context;
        callbacks.connectionAddedCallback = &StreamCallbackListener::connection_added_thunk;
        callbacks.connectionRemovedCallback = &StreamCallbackListener::connection_removed_thunk;
        callbacks.getParameterDataCallback = &StreamCallbackListener::get_parameter_data_thunk;
        callbacks.getParameterSizeCallback = &StreamCallbackListener::get_parameter_size_thunk;
        callbacks.setParameterCallback = &StreamCallbackListener::set_parameter_thunk;

        return callbacks;
    }
}

#endif /* STREAMCALLBACKTARGET_H */