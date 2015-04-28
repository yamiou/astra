#ifndef STYLIZEDDEPTHSTREAM_H
#define STYLIZEDDEPTHSTREAM_H

#include <SenseKit/Plugins/PluginKit.h>
#include <SenseKit/sensekit_types.h>
#include <SenseKit/Plugins/plugin_capi.h>
#include <SenseKitUL/skul_ctypes.h>
#include <SenseKitUL/Plugins/stream_types.h>

namespace sensekit { namespace plugins { namespace depth {

    class StylizedDepthStream : public MultiBinStream<sensekit_imageframe_wrapper_t,
                                                      uint8_t>
    {
    public:
        StylizedDepthStream(PluginServiceProxy& pluginService,
                            Sensor streamSet,
                            sensekit_stream_t sourceStream)
            : MultiBinStream(pluginService,
                             streamSet,
                             StreamDescription(SENSEKIT_STREAM_STYLIZED_DEPTH,
                                               DEFAULT_SUBTYPE))
        {}

        virtual void on_connection_added(sensekit_streamconnection_t connection)
        {
            /*
               if !bin create default bin
               link connection to bin
            */
        }

        virtual void on_connection_removed(sensekit_bin_t bin,
                                           sensekit_streamconnection_t connection) { }

        virtual void on_set_parameter(sensekit_streamconnection_t connection,
                                      sensekit_parameter_id id,
                                      size_t inByteLength,
                                      sensekit_parameter_data_t inData) {}

        virtual void on_get_parameter(sensekit_streamconnection_t connection,
                                      sensekit_parameter_id id,
                                      sensekit_parameter_bin_t& parameterBin) {}

        virtual void on_invoke(sensekit_streamconnection_t connection,
                               sensekit_command_id commandId,
                               size_t inByteLength,
                               sensekit_parameter_data_t inData,
                               sensekit_parameter_bin_t& parameterBin) {};

    };

}}}

#endif /* STYLIZEDDEPTHSTREAM_H */
