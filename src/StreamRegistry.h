#ifndef STREAMREGISTRY_H
#define STREAMREGISTRY_H

#include <map>
#include "Signal.h"

namespace sensekit {

    class Stream;

    struct context_id
    {
        int dummy;
    };

    class StreamRegistry
    {

    public:

        StreamRegistry();
        ~StreamRegistry();

        bool register_stream(context_id contextId, Stream* stream);
        bool unregister_stream(Stream* stream);
        bool is_stream_registered(Stream* stream);

        // template<typename F>
        // size_t register_callback(F&& callback)
        //     {
        //         return m_streamRegisteredSignal+= callback;
        //     }

        Signal<Stream*> get_streamRegisteredSignal() { return m_streamRegisteredSignal; }
        Signal<Stream*> get_streamUnregisteredSignal() { return m_streamUnregisteredSignal; }

    private:
        void raise_registered(Stream* stream)
            {
                m_streamRegisteredSignal.raise(stream);
            }

        void raise_unregistered(Stream* stream)
            {
                m_streamUnregisteredSignal.raise(stream);
            }

        using StreamContextMap = std::multimap<context_id, Stream*>;

        StreamContextMap m_streamContextMap;

        Signal<Stream*> m_streamRegisteredSignal;
        Signal<Stream*> m_streamUnregisteredSignal;
    };
}


#endif /* STREAMREGISTRY_H */
