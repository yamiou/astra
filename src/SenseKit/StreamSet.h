#ifndef STREAMSET_H
#define STREAMSET_H

#include "Stream.h"
#include "Logger.h"

#include <unordered_set>
#include <vector>
#include <memory>
#include <string>
#include <cassert>

namespace sensekit {

    class StreamSetConnection;

    class StreamSet
    {
    public:
        StreamSet(std::string uri);
        ~StreamSet();

        StreamSet& operator=(const StreamSet& rhs) = delete;
        StreamSet(const StreamSet& streamSet) = delete;

        StreamSetConnection* add_new_connection();
        void link_existing_connection(StreamSetConnection* connection);
        void disconnect_streamset_connection(StreamSetConnection* connection);

        StreamConnection* create_stream_connection(const sensekit_stream_desc_t& desc);

        bool destroy_stream_connection(StreamConnection* connection);

        void claim()
        {
            assert(m_isClaimed != true);
            m_isClaimed = true;
        }

        bool is_claimed()
        {
            return m_isClaimed;
        }

        //plugins only below

        Stream* register_stream(sensekit_stream_desc_t desc, stream_callbacks_t callbacks);
        Stream* register_orphan_stream(sensekit_stream_desc_t desc);

        void destroy_stream(Stream* stream);

        bool is_member(sensekit_stream_t stream) const;

        sensekit_stream_t find_stream_by_type_subtype(sensekit_stream_type_t type,
                                                      sensekit_stream_subtype_t subtype) const;

        const std::string& get_uri() { return m_uri; }

        sensekit_streamset_t get_handle()
            { return reinterpret_cast<sensekit_streamset_t>(this); }

        void visit_streams(std::function<void(Stream*)> visitorMethod);

        static StreamSet* get_ptr(sensekit_streamset_t handle)
            { return reinterpret_cast<StreamSet*>(handle); }

    private:
        Stream* find_stream_by_type_subtype_impl(sensekit_stream_type_t type,
                                                 sensekit_stream_subtype_t subtype) const;

        using StreamCollection = std::unordered_set<Stream*>;
        StreamCollection m_streamCollection;

        Logger m_logger;
        bool m_isClaimed{false};

        std::string m_uri;

        using StreamSetConnectionPtr = std::unique_ptr<StreamSetConnection>;
        using StreamSetConnectionList = std::vector<StreamSetConnectionPtr>;
        StreamSetConnectionList m_connections;
    };
}

#endif /* STREAMSET_H */
