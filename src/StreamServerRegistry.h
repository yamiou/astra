#ifndef STREAMSERVERREGISTRY_H
#define STREAMSERVERREGISTRY_H

#include <vector>
#include "Signal.h"

namespace sensekit {

    class StreamServer;

    class StreamServerRegistry
    {

    public:
        StreamServerRegistry()
            : m_initialized(false) {}

        ~StreamServerRegistry();

        void initialize();
        void terminate();

        bool register_stream_server(StreamServer* server);
        bool unregister_stream_server(StreamServer* server);
        bool is_server_registered(StreamServer* server);

        bool is_initialized() const { return m_initialized; }

        Signal<StreamServer*>  get_serverRegisteredSignal() { return m_serverRegisteredSignal; }
        Signal<StreamServer*> get_serverUnregisteredSignal() { return m_serverUnregisteredSignal; }

    private:

        void raise_serverRegisteredSignal(StreamServer* server)
            {
                m_serverRegisteredSignal.raise(server);
            }

        void raise_serverUnregisteredSignal(StreamServer* server)
            {
                m_serverUnregisteredSignal.raise(server);
            }

        using StreamServerList = std::vector<StreamServer*>;

        bool m_initialized;
        StreamServerList m_streamServers;

        Signal<StreamServer*> m_serverRegisteredSignal;
        Signal<StreamServer*> m_serverUnregisteredSignal;
    };
}


#endif /* STREAMSERVERREGISTRY_H */
