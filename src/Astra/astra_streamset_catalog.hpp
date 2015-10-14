#ifndef ASTRA_STREAMSET_CATALOG_H
#define ASTRA_STREAMSET_CATALOG_H

#include <map>
#include "astra_streamset.hpp"
#include "astra_stream_registered_event_args.hpp"
#include "astra_stream_unregistering_event_args.hpp"
#include <string>
#include <vector>
#include <memory>

namespace astra {

    class streamset_catalog
    {
    public:
        ~streamset_catalog();

        streamset_connection& open_set_connection(std::string uri);
        void close_set_connection(streamset_connection* connection);
        streamset& get_or_add(std::string uri, bool claim = false);
        streamset* find_streamset_for_stream(stream* stream);

        void clear();
        void visit_sets(std::function<void(streamset*)> visitorMethod);
        void destroy_set(streamset* set);

        astra_callback_id_t register_for_stream_registered_event(StreamRegisteredCallback callback)
        {
            astra_callback_id_t id =  m_streamRegisteredSignal += callback;

            visit_sets(
                [&callback] (streamset* set)
                {
                    set->visit_streams(
                        [&callback, &set] (stream* stream)
                        {
                            callback(stream_registered_event_args(set, stream, stream->get_description()));
                        });
                });

            return id;
        }

        astra_callback_id_t register_for_stream_unregistering_event(StreamUnregisteringCallback callback)
        {
            return m_streamUnregisteringSignal += callback;
        }

        void unregister_for_stream_registered_event(astra_callback_id_t callbackId)
        {
            m_streamRegisteredSignal -= callbackId;
        }

        void unregister_form_stream_unregistering_event(astra_callback_id_t callbackId)
        {
            m_streamUnregisteringSignal -= callbackId;
        }

    private:
        using streamset_ptr = std::unique_ptr<streamset>;

        struct streamset_entry
        {
            streamset_ptr streamSet;
            astra_callback_id_t addingId;
            astra_callback_id_t removingId;

            streamset_entry(streamset_ptr setPtr, astra_callback_id_t addingId, astra_callback_id_t removingId)
                : streamSet(std::move(setPtr)), addingId(addingId), removingId(removingId)
            { }

            ~streamset_entry()
            {
                streamSet->unregister_for_stream_registered_event(addingId);
                streamSet->unregister_for_stream_unregistering_event(removingId);
            }
        };

        using streamset_entry_ptr = std::unique_ptr<streamset_entry>;
        using streamsetMap = std::map<std::string, streamset_entry_ptr>;

        streamsetMap m_streamSets;

        void on_stream_registered(stream_registered_event_args args);
        void on_stream_unregistering(stream_unregistering_event_args args);

        signal<stream_registered_event_args> m_streamRegisteredSignal;
        signal<stream_unregistering_event_args> m_streamUnregisteringSignal;
    };
}

#endif /* ASTRA_STREAMSET_CATALOG_H */
