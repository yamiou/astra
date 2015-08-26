#ifndef STYLIZEDDEPTHSTREAM_H
#define STYLIZEDDEPTHSTREAM_H

#include <Astra/Plugins/PluginKit.h>
#include <Astra/astra_types.h>
#include <Astra/Plugins/plugin_capi.h>
#include <AstraUL/skul_ctypes.h>
#include <AstraUL/Plugins/stream_types.h>
#include <AstraUL/streams/Depth.h>

namespace astra { namespace plugins { namespace depth {

    class StylizedDepthStream
        : public SingleBinStream<astra_imageframe_wrapper_t>,
          public astra::FrameReadyListener
    {
    public:

        StylizedDepthStream(PluginServiceProxy& pluginService,
                            astra_streamset_t streamSet,
                            astra_stream_t sourceStream)
            : SingleBinStream(pluginService,
                              streamSet,
                              StreamDescription(ASTRA_STREAM_STYLIZED_DEPTH,
                                                DEFAULT_SUBTYPE), 0),
              m_sourceStream(sourceStream),
              m_sensor(get_uri_for_streamset(pluginService, streamSet)),
              m_reader(m_sensor.create_reader())
        {
            m_depthStream = m_reader.stream<astra::DepthStream>();
            m_depthStream.start();

            m_reader.addListener(*this);
        }

        astra_stream_t get_source_stream() { return m_sourceStream; }

        virtual void on_frame_ready(astra::StreamReader& reader, astra::Frame& frame) override
        {

        }

        virtual void on_connection_added(astra_streamconnection_t connection) override
        {
            /*
              if !bin create default bin
              link connection to bin
            */
        }

        virtual void on_connection_removed(astra_bin_t bin,
                                           astra_streamconnection_t connection) override { }

        virtual void on_set_parameter(astra_streamconnection_t connection,
                                      astra_parameter_id id,
                                      size_t inByteLength,
                                      astra_parameter_data_t inData) override {}

        virtual void on_get_parameter(astra_streamconnection_t connection,
                                      astra_parameter_id id,
                                      astra_parameter_bin_t& parameterBin) override {}

        virtual void on_invoke(astra_streamconnection_t connection,
                               astra_command_id commandId,
                               size_t inByteLength,
                               astra_parameter_data_t inData,
                               astra_parameter_bin_t& parameterBin) override {};

    private:
        DepthStream m_depthStream{nullptr};
        astra_stream_t m_sourceStream{nullptr};
        Sensor m_sensor;
        StreamReader m_reader;
    };

}}}

#endif /* STYLIZEDDEPTHSTREAM_H */
