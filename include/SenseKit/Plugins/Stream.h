#ifndef PLUGINSTREAM_H
#define PLUGINSTREAM_H

#include <SenseKit/SenseKit.h>
#include <SenseKit/Plugins/PluginServiceProxy.h>
#include <SenseKit/Plugins/StreamCallbackListener.h>
#include <system_error>
#include <cassert>

#include <iostream>
using std::cout;
using std::endl;

namespace sensekit { namespace plugins {

        class Stream : public StreamCallbackListener
        {
        public:

            Stream(PluginServiceProxy& pluginService,
                   Sensor& streamSet,
                   StreamDescription description) :
                m_pluginService(pluginService),
                m_streamSet(streamSet),
                m_description(description)
                {
                    create_stream(description);
                }

            virtual ~Stream()
                {
                    cout << "destroying sensekit stream of type " << m_description.get_type() << endl;
                    m_pluginService.destroy_stream(m_streamHandle);
                }

            inline const StreamDescription& get_description() { return m_description; }

        private:
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

                    cout << "creating stream of type: " << m_description.get_type() << endl;
                    stream_callbacks_t pluginCallbacks = create_plugin_callbacks(this);

                    sensekit_stream_desc_t desc = description.get_desc_t();

                    m_pluginService.create_stream(m_streamSet.get_handle(),
                                                  desc,
                                                  pluginCallbacks,
                                                  &m_streamHandle);
                }

            PluginServiceProxy& m_pluginService;
            Sensor& m_streamSet;
            StreamDescription m_description;
            sensekit_stream_t m_streamHandle{nullptr};

        protected:
            void create_bin(size_t binSize, sensekit_bin_t& binHandle, sensekit_frame_t*& buffer)
                {
                    cout << "creating bin -- "
                         << " handle: " << m_streamHandle
                         << " type: " << m_description.get_type()
                         << " size: " << binSize << endl;

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
                    if (bin != nullptr)
                    {
                    cout << "linking connection to bin -- "
                         << " handle: " << m_streamHandle
                         << " type: " << m_description.get_type()
                         << " conn: " << connection
                         << " bin: " << bin << endl;
                    }
                    else
                    {
                        cout << "unlinking connection -- "
                             << " handle: " << m_streamHandle
                             << " type: " << m_description.get_type()
                             << " conn: " << connection << endl;
                    }

                    m_pluginService.link_connection_to_bin(connection, bin);
                }

            void destroy_bin(sensekit_bin_t& binHandle, sensekit_frame_t*& buffer)
                {
                    cout << "destroying bin -- "
                         << " handle: " << m_streamHandle
                         << " type: " << m_description.get_type()
                         << " bin: " << binHandle << endl;

                    m_pluginService.destroy_stream_bin(m_streamHandle, &binHandle, &buffer);
                }

            bool bin_has_connections(sensekit_bin_t binHandle)
                {
                    bool hasConnections = false;
                    m_pluginService.bin_has_connections(binHandle, &hasConnections);

                    return hasConnections;
                }
        };

        inline void Stream::connection_added(sensekit_stream_t stream,
                                             sensekit_streamconnection_t connection)
        {
            assert(stream == m_streamHandle);
            on_connection_added(connection);
        }

        inline void Stream::connection_removed(sensekit_stream_t stream,
                                               sensekit_bin_t bin,
                                               sensekit_streamconnection_t connection)
        {
            assert(stream == m_streamHandle);
            on_connection_removed(bin, connection);
        }
    }}

#endif /* PLUGINSTREAM_H */