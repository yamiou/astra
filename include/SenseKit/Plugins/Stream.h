#ifndef PLUGINSTREAM_H
#define PLUGINSTREAM_H

#include <SenseKit/SenseKit.h>
#include <SenseKit/Plugins/PluginServiceProxy.h>
#include <SenseKit/Plugins/StreamCallbackListener.h>
#include <SenseKit/Plugins/PluginLogger.h>
#include <system_error>
#include <cassert>

namespace sensekit { namespace plugins {

    class Stream : public StreamCallbackListener
    {
    public:

        Stream(PluginServiceProxy& pluginService,
               Sensor& streamSet,
               StreamDescription description) :
            m_pluginService(pluginService),
            m_streamSet(streamSet),
            m_description(description),
            m_logger(pluginService)
        {
            create_stream(description);
        }

        virtual ~Stream()
        {
            m_logger.info("destroying sensekit stream of type: %d", m_description.get_type());
            m_pluginService.destroy_stream(m_streamHandle);
        }

        inline const StreamDescription& get_description() { return m_description; }

    protected:
        inline sensekit::plugins::PluginLogger& get_logger() { return m_logger; }

    private:
        sensekit::plugins::PluginLogger m_logger;
        virtual void connection_added(sensekit_stream_t stream,
                                      sensekit_streamconnection_t connection) override final;

        virtual void on_connection_added(sensekit_streamconnection_t connection) { }

        virtual void connection_removed(sensekit_stream_t stream,
                                        sensekit_bin_t bin,
                                        sensekit_streamconnection_t connection) override final;

        virtual void on_connection_removed(sensekit_bin_t bin,
                                           sensekit_streamconnection_t connection) { }

        virtual void on_new_buffer(sensekit_frame_t* newBuffer) { }

        void create_stream(StreamDescription& description)
        {
            assert(m_streamHandle == nullptr);

            m_logger.info("creating stream of type: %d", m_description.get_type());
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
        PluginServiceProxy& get_pluginService() const { return m_pluginService; }

        void create_bin(size_t binSize, sensekit_bin_t& binHandle, sensekit_frame_t*& buffer)
        {
            m_logger.info("creating bin -- handle: %x stream: %x type: %d size: %u",
                 binHandle,
                 m_streamHandle,
                 m_description.get_type(),
                 binSize);

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
                m_logger.info("linking connection to bin -- stream: %x type: %d conn: %x bin: %x",
                     m_streamHandle,
                     m_description.get_type(),
                     connection,
                     bin);
            }
            else
            {
                m_logger.info("linking connection to bin -- stream: %x type: %d conn: %x",
                     m_streamHandle,
                     m_description.get_type(),
                     connection);
            }

            m_pluginService.link_connection_to_bin(connection, bin);
        }

        void destroy_bin(sensekit_bin_t& binHandle, sensekit_frame_t*& buffer)
        {
            m_logger.info("destroying bin -- %d stream: %x type: %d size: %u",
                 binHandle,
                 m_streamHandle,
                 m_description.get_type());

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