#ifndef ASTRA_STREAMSET_H
#define ASTRA_STREAMSET_H

#include "astra_stream.hpp"
#include "astra_logger.hpp"
#include "astra_signal.hpp"
#include "astra_stream_registered_event_args.hpp"
#include "astra_stream_unregistering_event_args.hpp"
#include <unordered_set>
#include <functional>
#include <vector>
#include <memory>
#include <string>
#include <cassert>
#include "astra_streamset_connection.hpp"

namespace astra {

    class streamset
    {
    public:
        using VisitorFunc = std::function<void(stream*)>;

        streamset(std::string uri);

        streamset& operator=(const streamset& rhs) = delete;
        streamset(const streamset& streamSet) = delete;

        streamset_connection* add_new_connection();
        void disconnect_streamset_connection(streamset_connection* connection);

        stream_connection* create_stream_connection(const astra_stream_desc_t& desc);
        bool destroy_stream_connection(stream_connection* connection);

        void claim();
        bool is_claimed() const;

        //plugins only below

        stream* register_stream(astra_stream_desc_t desc, stream_callbacks_t callbacks);
        stream* register_orphan_stream(astra_stream_desc_t desc);

        void destroy_stream(stream* stream);

        bool is_member(astra_stream_t stream) const;

        astra_stream_t find_stream_by_type_subtype(astra_stream_type_t type,
                                                      astra_stream_subtype_t subtype) const;

        const std::string& get_uri() { return m_uri; }

        astra_streamset_t get_handle() { return reinterpret_cast<astra_streamset_t>(this); }

        void visit_streams(VisitorFunc visitorMethod);

        static streamset* get_ptr(astra_streamset_t handle) { return reinterpret_cast<streamset*>(handle); }

        astra_callback_id_t register_for_stream_registered_event(StreamRegisteredCallback callback);
        astra_callback_id_t register_for_stream_unregistering_event(StreamUnregisteringCallback callback);
        void unregister_for_stream_registered_event(astra_callback_id_t callbackId);
        void unregister_for_stream_unregistering_event(astra_callback_id_t callbackId);

    private:
        stream* find_stream_by_type_subtype_impl(astra_stream_type_t type,
                                                 astra_stream_subtype_t subtype) const;

        using StreamCollection = std::unordered_set<stream*>;
        StreamCollection m_streamCollection;

        using streamset_connectionPtr = std::unique_ptr<streamset_connection>;
        using streamset_connectionList = std::vector<streamset_connectionPtr>;
        streamset_connectionList m_connections;

        bool m_isClaimed{false};

        std::string m_uri;

        signal<stream_registered_event_args> m_streamRegisteredSignal;
        signal<stream_unregistering_event_args> m_streamUnregisteringSignal;
    };
}

#endif /* ASTRA_STREAMSET_H */
