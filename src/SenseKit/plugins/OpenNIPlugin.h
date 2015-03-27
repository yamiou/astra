#ifndef OPENNIPLUGIN_H
#define OPENNIPLUGIN_H

#include "PluginBase.h"
#include <OpenNI.h>
#include <streams/depth_types.h>
#include <streams/color_types.h>
#include "../../SenseKitUL/SenseKitUL_internal.h"

namespace sensekit
{
    namespace openni
    {
        class OpenNIPlugin : public sensekit::PluginBase
        {
        public:
            OpenNIPlugin(StreamServiceProxy* streamService, PluginServiceProxy* pluginService)
                : sensekit::PluginBase(streamService, pluginService) {}

            virtual ~OpenNIPlugin() = default;
            virtual void temp_update() override;

            // disable copying
            OpenNIPlugin(const OpenNIPlugin&) = delete;
            OpenNIPlugin& operator=(const OpenNIPlugin&) = delete;

        protected:
            virtual void on_initialize() override;
            virtual void on_cleanup() override;

        private:
            static void set_parameter_thunk(void* instance, sensekit_streamconnection_t* streamConnection, sensekit_parameter_id, size_t, sensekit_parameter_data_t*);
            static void get_parameter_size_thunk(void* instance, sensekit_streamconnection_t* streamConnection, sensekit_parameter_id id, size_t* byteLength);
            static void get_parameter_data_thunk(void* instance, sensekit_streamconnection_t* streamConnection, sensekit_parameter_id id, size_t byteLength, sensekit_parameter_data_t* data);

            void set_parameter(sensekit_streamconnection_t* streamConnection, sensekit_parameter_id, size_t, sensekit_parameter_data_t*);
            void get_parameter_size(sensekit_streamconnection_t* streamConnection, sensekit_parameter_id id, size_t& byteLength);
            void get_parameter_data(sensekit_streamconnection_t* streamConnection, sensekit_parameter_id id, size_t byteLength, sensekit_parameter_data_t* data);

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

            int m_frameIndex{ 0 };

            sensekit_frame_t* m_currentColorBuffer{nullptr};
            sensekit_frame_t* m_currentDepthBuffer{nullptr};

            StreamBinId m_depthBinId{0};
            StreamBinId m_colorBinId{0};

            sensekit_depthframe_wrapper_t* m_currentDepthFrame{nullptr};
            sensekit_colorframe_wrapper_t* m_currentColorFrame{nullptr};

            StreamHandle* m_depthHandle{nullptr};
            StreamHandle* m_colorHandle{nullptr};
            StreamSetHandle* m_streamSetHandle{nullptr};
        };
    }
}

#endif /* OPENNIPLUGIN_H */