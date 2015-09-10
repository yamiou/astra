#ifndef STREAMSET_H
#define STREAMSET_H

#include "Stream.h"
#include "Logger.h"
#include "Signal.h"
#include "StreamRegisteredEventArgs.h"
#include "StreamUnregisteringEventArgs.h"
#include <unordered_set>
#include <functional>
#include <vector>
#include <memory>
#include <string>
#include <cassert>
#include "StreamSetConnection.h"

namespace astra {

    class StreamSet
    {
    public:
        using VisitorFunc = std::function<void(Stream*)>;

        StreamSet(std::string uri);

        StreamSet& operator=(const StreamSet& rhs) = delete;
        StreamSet(const StreamSet& streamSet) = delete;

        StreamSetConnection* add_new_connection();
        void disconnect_streamset_connection(StreamSetConnection* connection);

        StreamConnection* create_stream_connection(const astra_stream_desc_t& desc);
        bool destroy_stream_connection(StreamConnection* connection);

        void claim();
        bool is_claimed() const;

        //plugins only below

        Stream* register_stream(astra_stream_desc_t desc, stream_callbacks_t callbacks);
        Stream* register_orphan_stream(astra_stream_desc_t desc);

        void destroy_stream(Stream* stream);

        bool is_member(astra_stream_t stream) const;

        astra_stream_t find_stream_by_type_subtype(astra_stream_type_t type,
                                                      astra_stream_subtype_t subtype) const;

        const std::string& get_uri() { return m_uri; }

        astra_streamset_t get_handle() { return reinterpret_cast<astra_streamset_t>(this); }

        void visit_streams(VisitorFunc visitorMethod);

        static StreamSet* get_ptr(astra_streamset_t handle) { return reinterpret_cast<StreamSet*>(handle); }

        astra_callback_id_t register_for_stream_registered_event(StreamRegisteredCallback callback);
        astra_callback_id_t register_for_stream_unregistering_event(StreamUnregisteringCallback callback);
        void unregister_for_stream_registered_event(astra_callback_id_t callbackId);
        void unregister_for_stream_unregistering_event(astra_callback_id_t callbackId);

    private:
        Stream* find_stream_by_type_subtype_impl(astra_stream_type_t type,
                                                 astra_stream_subtype_t subtype) const;

        using StreamCollection = std::unordered_set<Stream*>;
        StreamCollection m_streamCollection;

        bool m_isClaimed{false};

        std::string m_uri;

        using StreamSetConnectionPtr = std::unique_ptr<StreamSetConnection>;
        using StreamSetConnectionList = std::vector<StreamSetConnectionPtr>;
        StreamSetConnectionList m_connections;

        Signal<StreamRegisteredEventArgs> m_streamRegisteredSignal;
        Signal<StreamUnregisteringEventArgs> m_streamUnregisteringSignal;
    };
}

#endif /* STREAMSET_H */
