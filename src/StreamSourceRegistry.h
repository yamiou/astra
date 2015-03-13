#ifndef STREAMSOURCEREGISTRY_H
#define STREAMSOURCEREGISTRY_H

#include <vector>
#include "Signal.h"

namespace sensekit {

    class StreamSource;

    class StreamSourceRegistry
    {
    public:
        StreamSourceRegistry() {}
        ~StreamSourceRegistry();

        void initialize();
        void terminate();

        bool register_stream_source(StreamSource* source);
        bool unregister_stream_source(StreamSource* source);
        bool is_source_registered(StreamSource* source);

        bool is_initialized() const { return m_initialized; }

        Signal<StreamSource*>  get_sourceRegisteredSignal() { return m_sourceRegisteredSignal; }
        Signal<StreamSource*> get_sourceUnregisteredSignal() { return m_sourceUnregisteredSignal; }

    private:

        void raise_sourceRegisteredSignal(StreamSource* source)
            {
                m_sourceRegisteredSignal.raise(source);
            }

        void raise_sourceUnregisteredSignal(StreamSource* source)
            {
                m_sourceUnregisteredSignal.raise(source);
            }

        using StreamSourceList = std::vector<StreamSource*>;

        bool m_initialized;
        StreamSourceList m_streamSources;

        Signal<StreamSource*> m_sourceRegisteredSignal;
        Signal<StreamSource*> m_sourceUnregisteredSignal;
    };
}

#endif /* STREAMSOURCEREGISTRY_H */
