#ifndef STREAMCONTEXTFACTORY_H
#define STREAMCONTEXTFACTORY_H

#include <atomic>

namespace sensekit {

    using StreamContextId = unsigned;
    using StreamContext = StreamContextId;

    class StreamContextFactory
    {
    public:
        StreamContext create()
            {
                StreamContextId id = m_nextContextId++;
                return StreamContext(id);
            }

    private:
        std::atomic<StreamContextId> m_nextContextId{0};
    };



}

#endif /* STREAMCONTEXTFACTORY_H */
