#include "StreamServer.h"
#include <algorithm>

namespace sensekit {

    StreamServer::StreamServer()
        : m_initialized(false)
    { }

    StreamServer::~StreamServer()
    {
        terminate();
    }

    void StreamServer::initialize()
    {
        if (m_initialized)
            return;

        on_initialize();

        m_initialized = true;
    }

    void StreamServer::terminate()
    {
        if (!m_initialized)
            return;

        on_terminate();
        clear_sources();

        m_initialized = false;
    }

    void StreamServer::clear_sources()
    {
        m_sourceList.clear();
    }

    bool StreamServer::source_exists(StreamSource* source)
    {
        if (source == nullptr)
            return false;

        auto it = std::find(m_sourceList.begin(), m_sourceList.end(), source);

        return it != m_sourceList.end();
    }

    bool StreamServer::add_stream_source(StreamSource* source)
    {
        if (source == nullptr)
            return false;

        if (source_exists(source))
            return false;

        m_sourceList.push_back(source);

        return true;
    }

    bool StreamServer::remove_stream_source(StreamSource* source)
    {
        if (source == nullptr)
            return false;

        if (!source_exists(source))
            return false;

        auto it = std::find(m_sourceList.begin(), m_sourceList.end(), source);
        m_sourceList.erase(it);

        return true;
    }
}
