#ifndef OPENNIPLUGIN_H
#define OPENNIPLUGIN_H

#include <Plugins/plugin_api.h>
#include <sensekit_capi.h>
#include <OpenNI.h>
#include <streams/depth_types.h>
#include <streams/color_types.h>
#include "../../SenseKitUL/SenseKitUL_internal.h"
#include <SenseKit/Plugins/StreamCallbackListener.h>

namespace sensekit
{
    namespace openni
    {
        class OpenNIPlugin : public PluginBase, StreamCallbackListener
        {
        public:
            OpenNIPlugin(PluginServiceProxy* pluginService);

            virtual ~OpenNIPlugin();
            virtual void temp_update() override;

            OpenNIPlugin(const OpenNIPlugin&) = delete;
            OpenNIPlugin& operator=(const OpenNIPlugin&) = delete;

        private:

            virtual void set_parameter(sensekit_streamconnection_t connection,
                               sensekit_parameter_id id,
                               size_t byteLength,
                               sensekit_parameter_data_t* data) override;

            virtual void get_parameter_size(sensekit_streamconnection_t connection,
                                    sensekit_parameter_id id,
                                    size_t& byteLength) override;

            virtual void get_parameter_data(sensekit_streamconnection_t connection,
                                    sensekit_parameter_id id,
                                    size_t byteLength,
                                    sensekit_parameter_data_t* data) override;

            virtual void connection_added(sensekit_streamconnection_t connection) override;
            virtual void connection_removed(sensekit_streamconnection_t connection) override;

            sensekit_status_t open_sensor_streams();
            sensekit_status_t close_sensor_streams();

            void set_new_depth_buffer(sensekit_frame_t* nextBuffer);
            void set_new_color_buffer(sensekit_frame_t* nextBuffer);

            sensekit_status_t read_next_depth_frame(sensekit_depthframe_wrapper_t* frame);
            sensekit_status_t read_next_color_frame(sensekit_colorframe_wrapper_t* frame);

            ::openni::Device m_device;
            ::openni::VideoStream m_depthStream;
            ::openni::VideoStream m_colorStream;
            ::openni::VideoMode m_colorMode;
            ::openni::VideoMode m_depthMode;
            ::openni::DeviceInfo m_deviceInfo;

            unsigned m_colorBufferLength{0};
            unsigned m_depthBufferLength{0};

            sensekit_frame_index_t m_frameIndex{ 0 };

            sensekit_frame_t* m_currentColorBuffer{nullptr};
            sensekit_frame_t* m_currentDepthBuffer{nullptr};

            sensekit_bin_t m_depthBinHandle{nullptr};
            sensekit_bin_t m_colorBinHandle{nullptr};

            sensekit_depthframe_wrapper_t* m_currentDepthFrame{nullptr};
            sensekit_colorframe_wrapper_t* m_currentColorFrame{nullptr};

            sensekit_stream_t m_depthHandle{nullptr};
            sensekit_stream_t m_colorHandle{nullptr};
            sensekit_streamset_t m_streamSetHandle{nullptr};
        };
    }
}

#endif /* OPENNIPLUGIN_H */