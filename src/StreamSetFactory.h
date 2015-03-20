#ifndef STREAMSETFACTORY_H
#define STREAMSETFACTORY_H

#include <atomic>
#include "StreamSet.h"

namespace sensekit {

    class StreamSetFactory
    {
    public:
        StreamSet* create()
            {
                StreamSetId id = m_nextSetId++;
                return new StreamSet(id);
            }

    private:
        std::atomic<StreamSetId> m_nextSetId{0};
    };



}

#endif /* STREAMSETFACTORY_H */
