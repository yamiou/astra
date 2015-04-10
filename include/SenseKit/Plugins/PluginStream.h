#ifndef PLUGINSTREAM_H
#define PLUGINSTREAM_H
#include <sensekit_types.h>
#include <Plugins/plugin_api.h>
#include <system_error>
#include "StreamCallbackListener.h"

namespace sensekit {
    namespace plugins {

        template < typename TFrameWrapper >
        class PluginStream : StreamCallbackListener
        {
        public:
            PluginStream(PluginServiceProxy& pluginService, 
                         Sensor& streamset, 
                         StreamDescription description,
                         size_t binByteLength) :
                m_pluginService(pluginService),
                m_streamset(streamset),
                m_description(description),
                m_binByteLength(binByteLength)
            {
                create_stream(description);
            }

            virtual ~PluginStream()
            {
                m_pluginService.destroy_stream(m_streamHandle);
            }

            inline TFrameWrapper* try_lock_frame(sensekit_frame_index_t frameIndex)
            {
                if (m_currentFrame == nullptr)
                {
                    return nullptr;
                }
                m_currentFrame->frameIndex = frameIndex;
                return static_cast<TFrameWrapper*>(m_currentFrame->data);
            }

            inline void unlock_frame(TFrameWrapper*& frame)
            {
                //TODO verify frame is the same from m_currentFrame
                m_pluginService.cycle_bin_buffers(m_binHandle, &m_currentFrame);
                frame = nullptr;
            }

            inline StreamDescription get_description() { return m_description; }

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

            void create_stream(StreamDescription& description)
            {
                stream_callbacks_t pluginCallbacks = create_plugin_callbacks(this);

                sensekit_stream_desc_t desc = description.get_desc_t();
                m_pluginService.create_stream(m_streamset.get_handle(), desc, pluginCallbacks, &m_streamHandle);
            }

            PluginServiceProxy& m_pluginService;
            Sensor& m_streamset;
            StreamDescription m_description;
            sensekit_stream_t m_streamHandle{ nullptr };
            sensekit_bin_t m_binHandle{ nullptr };
            sensekit_frame_t* m_currentFrame{ nullptr };
            size_t m_binByteLength { 0 };
        };

        template < typename TFrameWrapper >
        inline void PluginStream<TFrameWrapper>::set_parameter(sensekit_streamconnection_t streamConnection,
            sensekit_parameter_id id,
            size_t byteLength,
            sensekit_parameter_data_t* data)
        {
        }

        template < typename TFrameWrapper >
        inline void PluginStream<TFrameWrapper>::get_parameter_size(sensekit_streamconnection_t streamConnection,
            sensekit_parameter_id id,
            size_t& byteLength)
        {
        }

        template < typename TFrameWrapper >
        inline void PluginStream<TFrameWrapper>::get_parameter_data(sensekit_streamconnection_t streamConnection,
            sensekit_parameter_id id,
            size_t byteLength,
            sensekit_parameter_data_t* data)
        {
        }

        template < typename TFrameWrapper >
        inline void PluginStream<TFrameWrapper>::connection_added(sensekit_streamconnection_t connection)
        {
            if (m_binHandle == nullptr)
            {
                
                m_pluginService.create_stream_bin(m_streamHandle, m_binByteLength, &m_binHandle, &m_currentFrame);
            }
            m_pluginService.link_connection_to_bin(connection, m_binHandle);
        }

        template < typename TFrameWrapper >
        inline void PluginStream<TFrameWrapper>::connection_removed(sensekit_streamconnection_t connection)
        {
            m_pluginService.link_connection_to_bin(connection, nullptr);
            //don't destroy bin if other connections are linked assigned to it
            bool hasConnections;
            m_pluginService.bin_has_connections(m_binHandle, &hasConnections);
            if (!hasConnections)
            {
                m_pluginService.destroy_stream_bin(m_streamHandle, &m_binHandle, &m_currentFrame);
            }
        }
    }
}

#endif /* PLUGINSTREAM_H */