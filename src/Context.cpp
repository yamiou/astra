#include "Context.h"
#include "SenseKit-private.h"
#include <iostream>

#include "OpenNIPlugin.h"

using std::cout;
using std::endl;

namespace sensekit {

    sensekit_status_t Context::initialize()
    {
        m_pluginService = new PluginService();

        //later this would involve dlopen and fptables and other stuff
        m_plugin = new sensekit::openni::OpenNIPlugin();
        m_plugin->initialize(this, m_pluginService);

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t Context::terminate()
    {
        if (nullptr != m_plugin)
        {
            m_plugin->cleanup();
            delete m_plugin;
            m_plugin = nullptr;
        }

        if (nullptr != m_pluginService)
        {
            delete m_pluginService;
            m_pluginService = nullptr;
        }

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t Context::open_sensor(const char *uri, sensekit_sensor_t **sensor)
    {
        //do nothing for now
        //would connect to the daemon and register interest in the uri

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t Context::close_sensor(sensekit_sensor_t **sensor)
    {
        //reverse the nothing we did above
        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t Context::open_depth_stream(sensekit_sensor_t *sensor, sensekit_depthstream_t **stream)
    {
        //trollolol nothing to do for now
        //would find the depth plugin for the context(sensor) and call client added event, and
        //then the plugin would create a bin if necessary and assign the client to the bin
        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t Context::close_depth_stream(sensekit_depthstream_t **stream)
    {
        //would find the depth plugin and call client removed event, and the plugin might destroy a bin
        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t Context::open_depth_frame(sensekit_depthstream_t *stream, int timeout, sensekit_depthframe_t*& frame)
    {
        //we got the actual frame in the temp_update call.
        //for real, we would do some type of double buffer swap on the client side, copy the latest frame (if newer) from the daemon
        //the daemon might reference count this
        frame = m_currentFrame;
        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t Context::close_depth_frame(sensekit_depthframe_t*& frame)
    {
        //later, would decrement the reference count
        //TODO: how does the daemon recover from a client crashing while a frame is open? (reference count does go to 0)

        frame = nullptr;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t Context::temp_update()
    {
        m_plugin->temp_update();

        bin_id id = nullptr;
        buffer* buf;
        //get the bin front buffer for this client, with a dummy bin id
        //later, we would have bookkeeping for client/streams to bin, and the bin storage would be outside pluginservice
        m_pluginService->get_bin(id, buf);

        //this part would be in the depth client side wrapper, since it knows about depth stuff
        m_currentFrame = static_cast<sensekit_depthframe_t*>(buf->data);

        return SENSEKIT_STATUS_SUCCESS;
    }
}
