#ifndef PLUGINSTREAM_H
#define PLUGINSTREAM_H

#include <SenseKit/SenseKit.h>
#include <SenseKit/Plugins/PluginServiceProxy.h>
#include <SenseKit/Plugins/StreamCallbackListener.h>
#include <SenseKit/Plugins/PluginLogger.h>
#include <system_error>
#include <cassert>
#include <unordered_set>

namespace sensekit { namespace plugins {

    class StreamHandleHash
    {
    public:
        std::size_t operator()(const sensekit_stream_t streamHandle) const
        {
            return std::hash<sensekit_stream_t>()(streamHandle);
        }
    };

    class StreamHandleEqualTo
    {
    public:
        std::size_t operator()(const sensekit_stream_t& lhs,
                               const sensekit_stream_t& rhs) const
        {
            return lhs == rhs;
        }
    };

    class Stream : public StreamCallbackListener
    {
    public:
        Stream(PluginServiceProxy& pluginService,
               Sensor streamSet,
               StreamDescription description) :
            m_logger(pluginService, "PluginStream"),
            m_pluginService(pluginService),
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

        inline const StreamDescription& get_description() { return m_description; }
        inline sensekit_stream_t get_handle() { return m_streamHandle; }

    protected:
        inline sensekit::plugins::PluginLogger& get_logger() { return m_logger; }
        inline PluginServiceProxy& get_pluginService() const { return m_pluginService; }

        inline void enable_callbacks();
    private:
        sensekit::plugins::PluginLogger m_logger;

        virtual void connection_added(sensekit_stream_t stream,
                                      sensekit_streamconnection_t connection) override final;

        virtual void connection_removed(sensekit_stream_t stream,
                                        sensekit_bin_t bin,
                                        sensekit_streamconnection_t connection) override final;

        virtual void set_parameter(sensekit_streamconnection_t connection,
                                   sensekit_parameter_id id,
                                   size_t inByteLength,
                                   sensekit_parameter_data_t inData) override final;

        virtual void get_parameter(sensekit_streamconnection_t connection,
                                   sensekit_parameter_id id,
                                   sensekit_parameter_bin_t& parameterBin) override final;

        virtual void invoke(sensekit_streamconnection_t connection,
                            sensekit_command_id commandId,
                            size_t inByteLength,
                            sensekit_parameter_data_t inData,
                            sensekit_parameter_bin_t& parameterBin) override final;

        virtual void on_connection_added(sensekit_streamconnection_t connection) { }

        virtual void on_connection_removed(sensekit_bin_t bin,
                                           sensekit_streamconnection_t connection) { }

        virtual void on_set_parameter(sensekit_streamconnection_t connection,
                                      sensekit_parameter_id id,
                                      size_t inByteLength,
                                      sensekit_parameter_data_t inData) {}

        virtual void on_get_parameter(sensekit_streamconnection_t connection,
                                      sensekit_parameter_id id,
                                      sensekit_parameter_bin_t& parameterBin) {}

        virtual void on_invoke(sensekit_streamconnection_t connection,
                               sensekit_command_id commandId,
                               size_t inByteLength,
                               sensekit_parameter_data_t inData,
                               sensekit_parameter_bin_t& parameterBin) {};

        void create_stream(StreamDescription& description)
        {
            assert(m_streamHandle == nullptr);

            stream_callbacks_t pluginCallbacks = create_plugin_callbacks(this);

            sensekit_stream_desc_t desc = description.get_desc_t();

            m_pluginService.create_stream(m_streamSet.get_handle(),
                                          desc,
                                          pluginCallbacks,
                                          &m_streamHandle);
        }

        PluginServiceProxy& m_pluginService;
        Sensor m_streamSet;
        StreamDescription m_description;
        sensekit_stream_t m_streamHandle{nullptr};
        bool m_inhibitCallbacks;
        std::unordered_set<sensekit_streamconnection_t> m_savedConnections;
    };

    inline void Stream::set_parameter(sensekit_streamconnection_t connection,
                                      sensekit_parameter_id id,
                                      size_t inByteLength,
                                      sensekit_parameter_data_t inData)
    {
        on_set_parameter(connection, id, inByteLength, inData);
    }

    inline void Stream::get_parameter(sensekit_streamconnection_t connection,
                                      sensekit_parameter_id id,
                                      sensekit_parameter_bin_t& parameterBin)
    {
        on_get_parameter(connection, id, parameterBin);
    }

    inline void Stream::invoke(sensekit_streamconnection_t connection,
                               sensekit_command_id commandId,
                               size_t inByteLength,
                               sensekit_parameter_data_t inData,
                               sensekit_parameter_bin_t& parameterBin)
    {
        on_invoke(connection, commandId, inByteLength, inData, parameterBin);
    }

    inline void Stream::connection_added(sensekit_stream_t stream,
                                         sensekit_streamconnection_t connection)
    {
        if (m_inhibitCallbacks)
        {
            m_logger.info("Saving a connection_added for later");
            m_savedConnections.insert(connection);
        }
        else
        {
            assert(stream == m_streamHandle);
            on_connection_added(connection);
        }
    }

    inline void Stream::connection_removed(sensekit_stream_t stream,
                                           sensekit_bin_t bin,
                                           sensekit_streamconnection_t connection)
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

    inline void Stream::enable_callbacks()
    {
        if (!m_inhibitCallbacks)
        {
            return;
        }
        m_inhibitCallbacks = false;

        if (m_savedConnections.size() > 0)
        {
            m_logger.info("Flushing saved connection_added connections");
            for (auto connection : m_savedConnections)
            {
                on_connection_added(connection);
            }
            m_savedConnections.clear();
        }
    }
}}

#endif /* PLUGINSTREAM_H */
