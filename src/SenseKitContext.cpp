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
        if (m_plugin != nullptr)
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

    sensekit_status_t SenseKitContext::open_stream(sensekit_streamset_t* streamset, sensekit_stream_t*& stream)
    {
        //trollolol nothing to do for now
        //would find the depth plugin for the context(streamset) and call client added event, and
        //then the plugin would create a bin if necessary and assign the client to the bin
        Stream* str = m_rootSet.get_stream_by_id(0);
        StreamConnection* stream_connection = str->open();

        stream = (sensekit_stream_t*)stream_connection;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::close_stream(sensekit_stream_t*& stream)
    {
        StreamConnection* stream_connection = (StreamConnection*)(stream);

        Stream* str = stream_connection->get_stream();
        //TODO stream bookkeeping and lifetime would be elsewhere
        str->close(stream_connection);

        stream = nullptr;
        //would find the depth plugin and call client removed event, and the plugin might destroy a bin

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::open_frame(sensekit_stream_t* stream, int timeout, sensekit_frame_ref_t*& frameRef)
    {
        //we got the actual frame in the temp_update call.
        //for real, we would do some type of double buffer swap on the client side, copy the latest frame (if newer) from the daemon
        //the daemon might reference count this

        StreamConnection* streamConnection = reinterpret_cast<StreamConnection*>(stream);

        StreamBin* bin = streamConnection->get_bin();
        if (bin != nullptr)
        {
            frameRef = new sensekit_frame_ref_t;
            frameRef->frame = bin->lock_front_buffer();
            frameRef->stream = stream;
        }
        else
        {
            frameRef = nullptr;
        }

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::close_frame(sensekit_frame_ref_t*& frameRef)
    {
        //later, would decrement the reference count
        //TODO: how does the daemon recover from a client crashing while a frame is open? (reference count does go to 0)

        StreamConnection* streamConnection = reinterpret_cast<StreamConnection*>(frameRef->stream);

        StreamBin* bin = streamConnection->get_bin();
        if (bin != nullptr)
        {
            bin->unlock_front_buffer();
        }

        delete frameRef;

        frameRef = nullptr;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::temp_update()
    {
        m_plugin->temp_update();
        return SENSEKIT_STATUS_SUCCESS;
    }
}
