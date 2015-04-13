#ifndef PLUGINSTREAM_H
#define PLUGINSTREAM_H

#include <SenseKit/Plugins/plugin_api.h>
#include <SenseKit/SenseKit.h>
#include <system_error>
#include "StreamCallbackListener.h"

namespace sensekit { namespace plugins {

        class StreamBase : public StreamCallbackListener
        {
        public:

            StreamBase(PluginServiceProxy& pluginService,
                       Sensor& streamSet,
                       StreamDescription description) :
                m_pluginService(pluginService),
                m_streamSet(streamSet),
                m_description(description)
                {
                    create_stream(description);
                }

            virtual ~StreamBase()
                {
                    m_pluginService.destroy_stream(m_streamHandle);
                }

            inline const StreamDescription& get_description() { return m_description; }

        private:
            virtual void set_parameter(sensekit_streamconnection_t connection,
                                       sensekit_parameter_id id,
                                       size_t byteLength,
                                       sensekit_parameter_data_t* data) final;

            virtual void get_parameter_size(sensekit_streamconnection_t connection,
                                            sensekit_parameter_id id,
                                            size_t& byteLength) override;

            virtual void get_parameter_data(sensekit_streamconnection_t connection,
                                            sensekit_parameter_id id,
                                            size_t byteLength,
                                            sensekit_parameter_data_t* data) final;

            virtual void connection_added(sensekit_stream_t stream,
                                          sensekit_streamconnection_t connection) final;

            virtual void on_connection_added(sensekit_streamconnection_t connection) { }

            virtual void connection_removed(sensekit_stream_t stream,
                                            sensekit_bin_t bin,
                                            sensekit_streamconnection_t connection) final;

            virtual void on_connection_removed(sensekit_bin_t bin,
                                               sensekit_streamconnection_t connection) { }

            virtual void on_new_buffer(sensekit_frame_t* newBuffer) { }

            void create_stream(StreamDescription& description)
                {
                    assert(m_streamHandle == nullptr);

                    stream_callbacks_t pluginCallbacks = create_plugin_callbacks(this);

                    sensekit_stream_desc_t desc = description.get_desc_t();

                    m_pluginService.create_stream(m_streamSet.get_handle(),
                                                  desc,
                                                  pluginCallbacks,
                                                  & m_streamHandle);
                }

            PluginServiceProxy& m_pluginService;
            Sensor& m_streamSet;
            StreamDescription m_description;
            sensekit_stream_t m_streamHandle{nullptr};

        protected:
            void create_bin(size_t binSize, sensekit_bin_t& binHandle, sensekit_frame_t*& buffer)
                {
                    m_pluginService.create_stream_bin(m_streamHandle,
                                                      binSize,
                                                      &binHandle,
                                                      &buffer);
                }

            void cycle_bin(sensekit_bin_t binHandle, sensekit_frame_t*& buffer)
                {
                    m_pluginService.cycle_bin_buffers(binHandle, &buffer);
                }

            void link_connection_to_bin(sensekit_streamconnection_t connection, sensekit_bin_t bin)
                {
                    m_pluginService.link_connection_to_bin(connection, bin);
                }

            void destroy_bin(sensekit_bin_t& binHandle, sensekit_frame_t*& buffer)
                {
                    m_pluginService.destroy_stream_bin(m_streamHandle, &binHandle, &buffer);
                }

            bool bin_has_connections(sensekit_bin_t binHandle)
                {
                    bool hasConnections = false;
                    m_pluginService.bin_has_connections(binHandle, &hasConnections);

                    return hasConnections;
                }
        };

        inline void StreamBase::set_parameter(sensekit_streamconnection_t streamConnection,
                                              sensekit_parameter_id id,
                                              size_t byteLength,
                                              sensekit_parameter_data_t* data)
        { }

        inline void StreamBase::get_parameter_size(sensekit_streamconnection_t streamConnection,
                                                   sensekit_parameter_id id,
                                                   size_t& byteLength)
        { }

        inline void StreamBase::get_parameter_data(sensekit_streamconnection_t streamConnection,
                                                   sensekit_parameter_id id,
                                                   size_t byteLength,
                                                   sensekit_parameter_data_t* data)
        { }

        inline void StreamBase::connection_added(sensekit_stream_t stream,
                                                 sensekit_streamconnection_t connection)
        {
            assert(stream == m_streamHandle);
            on_connection_added(connection);
        }

        inline void StreamBase::connection_removed(sensekit_stream_t stream,
                                                   sensekit_bin_t bin,
                                                   sensekit_streamconnection_t connection)
        {
            assert(stream == m_streamHandle);
            on_connection_removed(bin, connection);
        }


        template<typename TFrameType, typename TBlockType>
        class SingleBinStream : public StreamBase
        {
        public:

            SingleBinStream(PluginServiceProxy& pluginService,
                            Sensor& streamSet,
                            StreamDescription description,
                            size_t bufferSize)
                : StreamBase(pluginService,
                             streamSet,
                             description)
                {
                    create_bin(sizeof(TFrameType) + bufferSize, m_binHandle, m_frame);
                }

            using frame_type = TFrameType;
            using block_type = TBlockType;

            bool has_connections()
                {
                    if (m_binHandle == nullptr)
                        return false;

                    return bin_has_connections(m_binHandle);
                }

            frame_type* begin_write()
                {
                    if (m_locked)
                        return reinterpret_cast<TFrameType*>(m_frame->data);

                    m_locked = true;
                    return reinterpret_cast<TFrameType*>(m_frame->data);
                }

            void end_write()
                {
                    if (!m_locked)
                        return;

                    cycle_bin(m_binHandle, m_frame);
                    m_locked = false;
                }

        private:
            bool m_locked{false};
            sensekit_bin_t m_binHandle{nullptr};
            sensekit_frame_t* m_frame{nullptr};

        };
    }}

#endif /* PLUGINSTREAM_H */