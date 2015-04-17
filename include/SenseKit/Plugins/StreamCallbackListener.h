#ifndef STREAMCALLBACKTARGET_H
#define STREAMCALLBACKTARGET_H

#include <SenseKit/sensekit_types.h>
#include <SenseKit/Plugins/plugin_callbacks.h>

namespace sensekit {

    class StreamCallbackListener
    {
    public:
        virtual ~StreamCallbackListener() = default;
    private:

        static void set_parameter_thunk(void* instance,
                                        sensekit_streamconnection_t connection,
                                        sensekit_parameter_id id,
                                        size_t inByteLength,
                                        sensekit_parameter_data_t inData)
        {
            static_cast<StreamCallbackListener*>(instance)->set_parameter(connection, id, inByteLength, inData);
        }

        static void get_parameter_thunk(void* instance,
                                        sensekit_streamconnection_t connection,
                                        sensekit_parameter_id id,
                                        sensekit_parameter_bin_t* parameterBin)
        {
            static_cast<StreamCallbackListener*>(instance)->get_parameter(connection, 
                                                                          id, 
                                                                          *parameterBin);
        }

        static void invoke_thunk(void* instance,
                                 sensekit_streamconnection_t connection,
                                 sensekit_command_id commandId,
                                 size_t inByteLength,
                                 sensekit_parameter_data_t inData,
                                 sensekit_parameter_bin_t* parameterBin)
        {
            static_cast<StreamCallbackListener*>(instance)->invoke(connection, 
                                                                   commandId, 
                                                                   inByteLength, 
                                                                   inData,
                                                                   *parameterBin);
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
                                   size_t inByteLength,
                                   sensekit_parameter_data_t inData) {}

        virtual void get_parameter(sensekit_streamconnection_t connection,
                                   sensekit_parameter_id id,
                                   sensekit_parameter_bin_t& parameterBin) {}

        virtual void invoke(sensekit_streamconnection_t connection,
                            sensekit_command_id commandId,
                            size_t inByteLength,
                            sensekit_parameter_data_t inData,
                            sensekit_parameter_bin_t& parameterBin) {}

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
        callbacks.getParameterCallback = &StreamCallbackListener::get_parameter_thunk;
        callbacks.invokeCallback = &StreamCallbackListener::invoke_thunk;
        callbacks.setParameterCallback = &StreamCallbackListener::set_parameter_thunk;

        return callbacks;
    }
}

#endif /* STREAMCALLBACKTARGET_H */