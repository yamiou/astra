#ifndef HANDDEBUG_H
#define HANDDEBUG_H

#include <SenseKit/SenseKit.h>
#include <stdexcept>
#include <SenseKitUL/StreamTypes.h>
#include "hand_capi.h"
#include <SenseKitUL/Vectorx.h>

namespace sensekit {
    
    class HandDebugStream : public DataStream
    {
    public:
        explicit HandDebugStream(sensekit_streamconnection_t connection)
            : DataStream(connection)
            { }

        static const sensekit_stream_type_t id = SENSEKIT_STREAM_HAND_DEBUG_IMAGE;
    };
}

#endif /* HANDDEBUG_H */
