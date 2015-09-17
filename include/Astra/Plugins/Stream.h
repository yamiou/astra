#ifndef PLUGINSTREAM_H
#define PLUGINSTREAM_H

#include <Astra/Astra.h>
#include <Astra/Plugins/PluginLogger.h>
#include <Astra/Plugins/PluginServiceProxy.h>
#include <Astra/Plugins/StreamCallbackListener.h>
#include <system_error>
#include <cassert>
#include <unordered_set>

namespace astra { namespace plugins {

    class StreamHandleHash
    {
    public:
        std::size_t operator()(const astra_stream_t streamHandle) const
        {
            return std::hash<astra_stream_t>()(streamHandle);
        }
    };

    class StreamHandleEqualTo
    {
    public:
        std::size_t operator()(const astra_stream_t& lhs,
                               const astra_stream_t& rhs) const
        {
            return lhs == rhs;
        }
    };

    class Stream : public StreamCallbackListener
    {
    public:
        Stream(PluginServiceProxy& pluginService,
               astra_streamset_t streamSet,
               StreamDescription description)
            : m_pluginService(pluginService),
              m_streamSet(streamSet),
              m_description(description),
              m_inhibitCallbacks(true)
        {
            create_stream(description);
        }

        virtual ~Stream()
        {
            m_pluginService.destroy_stream(m_streamHandle);
        }

        inline const StreamDescription& description() { return m_description; }
        inline astra_stream_t get_handle() { return m_streamHandle; }

    protected:
        inline PluginServiceProxy& pluginService() const { return m_pluginService; }
        inline void enable_callbacks();

    private:
        virtual void connection_added(astra_stream_t stream,
                                      astra_streamconnection_t connection) override final;

        virtual void connection_removed(astra_stream_t stream,
                                        astra_bin_t bin,
                                        astra_streamconnection_t connection) override final;

        virtual void connection_started(astra_stream_t stream,
                                        astra_streamconnection_t connection) override final;

        virtual void connection_stopped(astra_stream_t stream,
                                        astra_streamconnection_t connection) override final;

        virtual void set_parameter(astra_streamconnection_t connection,
                                   astra_parameter_id id,
                                   size_t inByteLength,
                                   astra_parameter_data_t inData) override final;

        virtual void get_parameter(astra_streamconnection_t connection,
                                   astra_parameter_id id,
                                   astra_parameter_bin_t& parameterBin) override final;

        virtual void invoke(astra_streamconnection_t connection,
                            astra_command_id commandId,
                            size_t inByteLength,
                            astra_parameter_data_t inData,
                            astra_parameter_bin_t& parameterBin) override final;

        virtual void on_connection_added(astra_streamconnection_t connection) {}

        virtual void on_connection_removed(astra_bin_t bin,
                                           astra_streamconnection_t connection) {}

        virtual void on_connection_started(astra_streamconnection_t connection) {}

        virtual void on_connection_stopped(astra_streamconnection_t connection) {}

        virtual void on_set_parameter(astra_streamconnection_t connection,
                                      astra_parameter_id id,
                                      size_t inByteLength,
                                      astra_parameter_data_t inData) {}

        virtual void on_get_parameter(astra_streamconnection_t connection,
                                      astra_parameter_id id,
                                      astra_parameter_bin_t& parameterBin) {}

        virtual void on_invoke(astra_streamconnection_t connection,
                               astra_command_id commandId,
                               size_t inByteLength,
                               astra_parameter_data_t inData,
                               astra_parameter_bin_t& parameterBin) {};

        void create_stream(StreamDescription& description)
        {
            assert(m_streamHandle == nullptr);

            stream_callbacks_t pluginCallbacks = create_plugin_callbacks(this);

            astra_stream_desc_t* desc = static_cast<astra_stream_desc_t*>(m_description);
            m_pluginService.create_stream(m_streamSet,
                                          *desc,
                                          pluginCallbacks,
                                          &m_streamHandle);
        }

        PluginServiceProxy& m_pluginService;
        astra_streamset_t m_streamSet{nullptr};
        StreamDescription m_description;
        astra_stream_t m_streamHandle{nullptr};

        bool m_inhibitCallbacks;
        std::unordered_set<astra_streamconnection_t> m_savedConnections;
    };

    inline void Stream::set_parameter(astra_streamconnection_t connection,
                                      astra_parameter_id id,
                                      size_t inByteLength,
                                      astra_parameter_data_t inData)
    {
        on_set_parameter(connection, id, inByteLength, inData);
    }

    inline void Stream::get_parameter(astra_streamconnection_t connection,
                                      astra_parameter_id id,
                                      astra_parameter_bin_t& parameterBin)
    {
        on_get_parameter(connection, id, parameterBin);
    }

    inline void Stream::invoke(astra_streamconnection_t connection,
                               astra_command_id commandId,
                               size_t inByteLength,
                               astra_parameter_data_t inData,
                               astra_parameter_bin_t& parameterBin)
    {
        on_invoke(connection, commandId, inByteLength, inData, parameterBin);
    }

    inline void Stream::connection_added(astra_stream_t stream,
                                         astra_streamconnection_t connection)
    {
        if (m_inhibitCallbacks)
        {
            LOG_INFO("astra.plugins.Stream", "Saving a connection_added for later");
            m_savedConnections.insert(connection);
        }
        else
        {
            assert(stream == m_streamHandle);
            on_connection_added(connection);
        }
    }

    inline void Stream::connection_removed(astra_stream_t stream,
                                           astra_bin_t bin,
                                           astra_streamconnection_t connection)
    {
        if (m_inhibitCallbacks)
        {
            m_savedConnections.erase(connection);
        }
        else
        {
            assert(stream == m_streamHandle);
            on_connection_removed(bin, connection);
        }
    }

    inline void Stream::connection_started(astra_stream_t stream,
                                           astra_streamconnection_t connection)
    {
        assert(stream == m_streamHandle);
        on_connection_started(connection);
    }

    inline void Stream::connection_stopped(astra_stream_t stream,
                                           astra_streamconnection_t connection)
    {
        assert(stream == m_streamHandle);
        on_connection_stopped(connection);
    }

    inline void Stream::enable_callbacks()
    {
        if (!m_inhibitCallbacks)
        {
            return;
        }

        m_inhibitCallbacks = false;

        if (m_savedConnections.size() > 0)
        {
            LOG_INFO("astra.plugins.Stream", "Flushing saved connection_added connections");
            for (auto connection : m_savedConnections)
            {
                on_connection_added(connection);
            }
            m_savedConnections.clear();
        }
    }
}}

#endif /* PLUGINSTREAM_H */
