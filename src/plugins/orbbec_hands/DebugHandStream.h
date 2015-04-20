#ifndef DEBUGHANDSTREAM_H
#define DEBUGHANDSTREAM_H

#include <SenseKit/Plugins/SingleBinStream.h>
#include <SenseKitUL/StreamTypes.h>
#include <SenseKitUL/Plugins/stream_types.h>

namespace sensekit { namespace plugins { namespace hands {
    
    class DebugHandStream : public SingleBinStream<sensekit_imageframe_wrapper_t, uint8_t>

    {
    public:
        DebugHandStream(PluginServiceProxy& pluginService,
                        Sensor& streamSet,
                        uint32_t width,
                        uint32_t height,
                        uint32_t bytesPerPixel)
            : SingleBinStream(pluginService,
                                streamSet,
                                StreamDescription(SENSEKIT_STREAM_DEBUG_HAND,
                                                  DEFAULT_SUBTYPE),
                                width * height * bytesPerPixel)
            { }
    protected:
        virtual void set_parameter(sensekit_streamconnection_t connection,
                                    sensekit_parameter_id id,
                                    size_t inByteLength,
                                    sensekit_parameter_data_t inData) override;

        virtual void get_parameter(sensekit_streamconnection_t connection,
                                    sensekit_parameter_id id,
                                    sensekit_parameter_bin_t& parameterBin) override;

        virtual void invoke(sensekit_streamconnection_t connection,
                            sensekit_command_id commandId,
                            size_t inByteLength,
                            sensekit_parameter_data_t inData,
                            sensekit_parameter_bin_t& parameterBin) override;

    private: 
    };
} } }

#endif /* DEBUGHANDSTREAM_H */
