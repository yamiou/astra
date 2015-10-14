#ifndef ASTRA_STREAMSET_CONNECTION_H
#define ASTRA_STREAMSET_CONNECTION_H

#include "astra_registry.hpp"
#include <Astra/astra_types.h>
#include "astra_stream_reader.hpp"
#include <vector>
#include <memory>
#include "astra_logger.hpp"

namespace astra {

    class streamset;

    class streamset_connection : public tracked_instance<streamset_connection>
    {
    public:
        streamset_connection(streamset* streamSet)
            : m_streamSet(streamSet)
        {}

        ~streamset_connection()
        {
            LOG_TRACE("streamset_connection", "destroying streamset_connection: %p", this);
        }

        streamset_connection& operator=(const streamset_connection& rhs) = delete;
        streamset_connection(const streamset_connection& conn) = delete;

        streamset* get_streamSet() { return m_streamSet; }

        stream_reader* create_reader();
        bool destroy_reader(stream_reader* reader);

        bool is_connected() { return m_streamSet != nullptr; }

        static streamset_connection* get_ptr(astra_streamsetconnection_t conn)
        {
            return registry::get<streamset_connection>(conn);
        }

        astra_streamsetconnection_t get_handle()
        {
            return reinterpret_cast<astra_streamsetconnection_t>(this);
        }

    private:
        streamset* m_streamSet{nullptr};

        using ReaderPtr = std::unique_ptr<stream_reader>;
        using ReaderList = std::vector<ReaderPtr>;

        ReaderList m_readers;
    };
}


#endif /* ASTRA_STREAMSET_CONNECTION_H */
