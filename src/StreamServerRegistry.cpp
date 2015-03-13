#include "StreamServerRegistry.h"
#include "StreamServer.h"

namespace sensekit {

    StreamServerRegistry::~StreamServerRegistry()
    {
        terminate();
    }

    void StreamServerRegistry::initialize()
    {
        if (m_initialized)
            return;

        for(auto* server : m_streamServers)
        {
            server->initialize();
        }

        m_initialized = true;
    }

    void StreamServerRegistry::terminate()
    {
        if (!m_initialized)
            return;

        for(auto* server : m_streamServers)
        {
            server->terminate();
        }

        m_initialized = false;
    }

    bool StreamServerRegistry::register_stream_server(StreamServer* server)
    {
        if (server == nullptr)
            return false;

        if (is_server_registered(server))
            return false;

        m_streamServers.push_back(server);

        if (is_initialized())
            server->initialize();

        raise_serverRegisteredSignal(server);

        return true;
    }

    bool StreamServerRegistry::unregister_stream_server(StreamServer* server)
    {
        if (server == nullptr)
            return false;

        if (!is_server_registered(server))
            return false;

        auto it = std::find(m_streamServers.begin(), m_streamServers.end(), server);
        m_streamServers.erase(it);

        raise_serverUnregisteredSignal(server);

        return true;
    }

    bool StreamServerRegistry::is_server_registered(sensekit::StreamServer *server)
    {
        if (server == nullptr)
            return false;

        auto it = std::find(m_streamServers.begin(), m_streamServers.end(), server);
        return it != m_streamServers.end();
    }
}
