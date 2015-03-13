#include "StreamSourceRegistry.h"
#include "StreamSource.h"

namespace sensekit {

    StreamSourceRegistry::~StreamSourceRegistry()
    {
        terminate();
    }

    void StreamSourceRegistry::initialize()
    {
        if (m_initialized)
            return;

        for(auto* source : m_streamSources)
        {
            source->initialize();
        }

        m_initialized = true;
    }

    void StreamSourceRegistry::terminate()
    {
        if (!m_initialized)
            return;

        for(auto* source : m_streamSources)
        {
            source->terminate();
        }

        m_initialized = false;
    }

    bool StreamSourceRegistry::register_stream_source(StreamSource* source)
    {
        if (source == nullptr)
            return false;

        if (is_source_registered(source))
            return false;

        m_streamSources.push_back(source);

        if (is_initialized())
            source->initialize();

        raise_sourceRegisteredSignal(source);

        return true;
    }

    bool StreamSourceRegistry::unregister_stream_source(StreamSource* source)
    {
        if (source == nullptr)
            return false;

        if (!is_source_registered(source))
            return false;

        auto it = std::find(m_streamSources.begin(), m_streamSources.end(), source);
        m_streamSources.erase(it);

        raise_sourceUnregisteredSignal(source);

        return true;
    }

    bool StreamSourceRegistry::is_source_registered(sensekit::StreamSource *source)
    {
        if (source == nullptr)
            return false;

        auto it = std::find(m_streamSources.begin(), m_streamSources.end(), source);
        return it != m_streamSources.end();
    }
}
