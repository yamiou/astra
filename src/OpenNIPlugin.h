#ifndef OPENNIPLUGIN_H
#define OPENNIPLUGIN_H

#include "PluginBase.h"
#include <OpenNI.h>

namespace sensekit
{
    namespace openni
    {
        class OpenNIPlugin : public sensekit::PluginBase
        {
        public:
            OpenNIPlugin();
            ~OpenNIPlugin();

            virtual void temp_update() override;

        protected:
            virtual void on_initialize() override;
            virtual void on_cleanup() override;

        private:
            sensekit_status_t open_depth_stream();
            sensekit_status_t close_depth_stream();
            void set_new_buffer(buffer* nextBuffer);
            sensekit_status_t read_next_depth_frame(sensekit_depthframe_t* frame);

            ::openni::Device m_device;
            ::openni::VideoStream m_depthStream;
            ::openni::DeviceInfo m_deviceInfo;

            int m_frameIndex{ 0 };

            buffer* m_currentBuffer{nullptr};
            sensekit_depthframe_t* m_currentFrame{nullptr};
        };
    }
}

#endif /* OPENNIPLUGIN_H */