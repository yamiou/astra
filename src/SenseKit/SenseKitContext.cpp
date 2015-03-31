#include "SenseKitContext.h"
#include <iostream>

#include "StreamConnection.h"
#include "StreamBin.h"
#include "StreamServiceDelegate.h"
#include <Plugins/StreamServiceProxyBase.h>
#include <Plugins/PluginServiceProxyBase.h>
#include "Core/shared_library.h"
#include "Core/libs.h"

using std::cout;
using std::endl;

namespace sensekit {

    StreamServiceProxyBase* create_stream_proxy(SenseKitContext* context)
    {
        StreamServiceProxyBase* proxy = new StreamServiceProxyBase;

        proxy->open_streamset = &StreamServiceDelegate::open_streamset;
        proxy->close_streamset = &StreamServiceDelegate::close_streamset;
        proxy->open_stream = &StreamServiceDelegate::open_stream;
        proxy->close_stream = &StreamServiceDelegate::close_stream;
        proxy->open_frame = &StreamServiceDelegate::open_frame;
        proxy->close_frame = &StreamServiceDelegate::close_frame;
        proxy->set_parameter = &StreamServiceDelegate::set_parameter;
        proxy->get_parameter_data = &StreamServiceDelegate::get_parameter_data;
        proxy->get_parameter_size = &StreamServiceDelegate::get_parameter_size;
        proxy->temp_update = &StreamServiceDelegate::temp_update;
        proxy->streamService = context;

        return proxy;
    }

    sensekit_status_t SenseKitContext::initialize()
    {
        m_pluginServiceProxy = m_pluginService.create_proxy();
        m_streamServiceProxy = create_stream_proxy(this);

        //TODO: OMG ERROR HANDLING
        LibHandle libHandle = nullptr;

        for(auto lib : get_libs())
        {
            cout << lib << endl;
            os_load_library((PLUGIN_DIRECTORY + lib).c_str(), libHandle);

            PluginFuncs pluginFuncs;
            os_get_proc_address(libHandle, SK_STRINGIFY(sensekit_plugin_initialize), (FarProc&)pluginFuncs.initialize);
            os_get_proc_address(libHandle, SK_STRINGIFY(sensekit_plugin_terminate), (FarProc&)pluginFuncs.terminate);
            os_get_proc_address(libHandle, SK_STRINGIFY(sensekit_plugin_update), (FarProc&)pluginFuncs.update);

            if (pluginFuncs.isValid())
            {
                pluginFuncs.initialize(m_pluginServiceProxy);
                m_pluginList.push_back(pluginFuncs);
            }
            else
            {
                os_free_library(libHandle);
            }
        }

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::terminate()
    {
        for(auto pluginFuncs : m_pluginList)
        {
            pluginFuncs.terminate();
        }

        if (m_pluginServiceProxy)
            delete m_pluginServiceProxy;

        if (m_streamServiceProxy)
            delete m_streamServiceProxy;

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

    sensekit_status_t SenseKitContext::open_stream(sensekit_streamset_t* streamSet,
                                                   sensekit_stream_type_t type,
                                                   sensekit_stream_subtype_t subType,
                                                   sensekit_streamconnection_t*& streamConnection)
    {
        StreamConnection* connection = get_rootSet().open_stream_connection(
            static_cast<StreamType>(type),
            static_cast<StreamSubtype>(subType));

        if (connection)
        {
            streamConnection = reinterpret_cast<sensekit_streamconnection_t*>(connection);
            return SENSEKIT_STATUS_SUCCESS;
        }
        else
        {
            streamConnection = nullptr;
            return SENSEKIT_STATUS_INVALID_PARAMETER;
        }
    }

    sensekit_status_t SenseKitContext::close_stream(sensekit_streamconnection_t*& streamConnection)
    {
        StreamConnection* connection = reinterpret_cast<StreamConnection*>(streamConnection);

        Stream* stream = connection->get_stream();
        //TODO stream bookkeeping and lifetime would be elsewhere
        stream->close(connection);

        streamConnection = nullptr;
        //would find the depth plugin and call client removed event, and the plugin might destroy a bin

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::open_frame(sensekit_streamconnection_t* streamConnection,
                                                  int timeout,
                                                  sensekit_frame_ref_t*& frameRef)
    {
        //we got the actual frame in the temp_update call.
        //for real, we would do some type of double buffer swap on the client side,
        //copy the latest frame (if newer) from the daemon
        //the daemon might reference count this

        StreamConnection* connection = reinterpret_cast<StreamConnection*>(streamConnection);

        StreamBin* bin = connection->get_bin();
        if (bin)
        {
            frameRef = new sensekit_frame_ref_t;
            frameRef->frame = bin->lock_front_buffer();
            frameRef->streamConnection = streamConnection;
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

        StreamConnection* connection = reinterpret_cast<StreamConnection*>(frameRef->streamConnection);

        StreamBin* bin = connection->get_bin();
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
        for(auto plinfo : m_pluginList)
        {
            if (plinfo.update)
                plinfo.update();
        }

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::set_parameter(sensekit_streamconnection_t* streamConnection,
                                                     sensekit_parameter_id parameterId,
                                                     size_t byteLength,
                                                     sensekit_parameter_data_t* data)
    {
        StreamConnection* connection = reinterpret_cast<StreamConnection*>(streamConnection);
        Stream* stream = connection->get_stream();

        stream->set_parameter(connection, parameterId, byteLength, data);

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::get_parameter_size(sensekit_streamconnection_t* streamConnection,
                                                          sensekit_parameter_id parameterId,
                                                          size_t& byteLength)
    {
        StreamConnection* connection = reinterpret_cast<StreamConnection*>(streamConnection);
        Stream* stream = connection->get_stream();

        stream->get_parameter_size(connection, parameterId, byteLength);

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::get_parameter_data(sensekit_streamconnection_t* streamConnection,
                                                          sensekit_parameter_id parameterId,
                                                          size_t byteLength,
                                                          sensekit_parameter_data_t* data)
    {
        StreamConnection* connection = reinterpret_cast<StreamConnection*>(streamConnection);
        Stream* stream = connection->get_stream();

        stream->get_parameter_data(connection, parameterId, byteLength, data);

        return SENSEKIT_STATUS_SUCCESS;
    }
}
