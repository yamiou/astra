#include "SenseKitContext.h"
#include "SenseKit-private.h"
#include <iostream>

#include "OpenNIPlugin.h"
#include "StreamConnection.h"
#include "StreamBin.h"

using std::cout;
using std::endl;

namespace sensekit {

    sensekit_status_t SenseKitContext::initialize()
    {
        //later this would involve dlopen and fptables and other stuff
        m_plugin = new sensekit::openni::OpenNIPlugin(*this, m_pluginService);
        m_plugin->initialize();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::terminate()
    {
        if (nullptr != m_plugin)
        {
            m_plugin->cleanup();
            delete m_plugin;
            m_plugin = nullptr;
        }

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::open_streamset(const char* uri, sensekit_streamset_t*& streamset)
    {
        //do nothing for now
        //would connect to the daemon and register interest in the uri

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::close_streamset(sensekit_streamset_t*& streamset)
    {
        //reverse the nothing we did above
        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::open_stream(sensekit_streamset_t *streamset, sensekit_stream_t*& stream)
    {
        //trollolol nothing to do for now
        //would find the depth plugin for the context(streamset) and call client added event, and
        //then the plugin would create a bin if necessary and assign the client to the bin
        Stream* str = new Stream(0, 0, 0);

        StreamConnection* stream_connection = new StreamConnection(str);

        stream = (sensekit_stream_t*)stream_connection;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::close_stream(sensekit_stream_t*& stream)
    {
        StreamConnection* stream_connection = (StreamConnection*)(stream);

        const Stream* str = stream_connection->get_stream();
        //TODO stream bookkeeping and lifetime would be elsewhere
        delete str;
        delete stream_connection;

        stream = nullptr;
        //would find the depth plugin and call client removed event, and the plugin might destroy a bin
        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::open_frame(sensekit_stream_t *stream, int timeout, sensekit_frame_t*& frame)
    {
        //we got the actual frame in the temp_update call.
        //for real, we would do some type of double buffer swap on the client side, copy the latest frame (if newer) from the daemon
        //the daemon might reference count this

        StreamConnection* stream_connection = (StreamConnection*)(stream);

        const Stream* str = stream_connection->get_stream();

        StreamBin* bin = stream_connection->get_bin();
        if (bin != nullptr)
        {
            frame = bin->get_front_buffer();
        }

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::close_frame(sensekit_frame_t*& frame)
    {
        //later, would decrement the reference count
        //TODO: how does the daemon recover from a client crashing while a frame is open? (reference count does go to 0)

        frame = nullptr;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::temp_update()
    {
        m_plugin->temp_update();

        return SENSEKIT_STATUS_SUCCESS;
    }
}
