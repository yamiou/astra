#include "SenseKitContext.h"
#include <iostream>
#include <cassert>
#include "StreamConnection.h"
#include "StreamBin.h"
#include "StreamServiceDelegate.h"
#include <Plugins/StreamServiceProxyBase.h>
#include <Plugins/PluginServiceProxyBase.h>
#include "Core/shared_library.h"
#include "Core/libs.h"
#include "StreamReader.h"

using std::cout;
using std::endl;

namespace sensekit {

    StreamServiceProxyBase* create_stream_proxy(SenseKitContext* context)
    {
        StreamServiceProxyBase* proxy = new StreamServiceProxyBase;

        proxy->open_streamset = &StreamServiceDelegate::open_streamset;
        proxy->close_streamset = &StreamServiceDelegate::close_streamset;
        proxy->create_reader = &StreamServiceDelegate::create_reader;
        proxy->destroy_reader = &StreamServiceDelegate::destroy_reader;
        proxy->get_stream = &StreamServiceDelegate::get_stream;
        proxy->start_stream = &StreamServiceDelegate::start_stream;
        proxy->stop_stream = &StreamServiceDelegate::stop_stream;
        proxy->open_frame = &StreamServiceDelegate::open_frame;
        proxy->close_frame = &StreamServiceDelegate::close_frame;
        proxy->get_frame =  &StreamServiceDelegate::get_frame;
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

    sensekit_status_t SenseKitContext::open_streamset(const char* uri, sensekit_streamset_t*& streamSet)
    {
        streamSet = reinterpret_cast<sensekit_streamset_t*>(&get_rootSet());

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::close_streamset(sensekit_streamset_t*& streamSet)
    {
        streamSet = nullptr;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::create_reader(sensekit_streamset_t* streamSet,
                                                     sensekit_reader_t*& reader)
    {
        assert(streamSet != nullptr);

        StreamSet* actualSet = reinterpret_cast<StreamSet*>(streamSet);
        reader = reinterpret_cast<sensekit_reader_t*>(new StreamReader(*actualSet));

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::destroy_reader(sensekit_reader_t*& reader)
    {

        assert(reader != nullptr);

        StreamReader* actualReader = reinterpret_cast<StreamReader*>(reader);
        if (actualReader != nullptr)
        {
            delete actualReader;
            reader = nullptr;
        }

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::get_stream(sensekit_reader_t* reader,
                                                  sensekit_stream_type_t type,
                                                  sensekit_stream_subtype_t subType,
                                                  sensekit_streamconnection_t*& connection)
    {
        assert(reader != nullptr);

        StreamReader* actualReader = reinterpret_cast<StreamReader*>(reader);

        sensekit_stream_desc_t desc;
        desc.type = type;
        desc.subType = subType;

        connection = actualReader->get_stream(desc);

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::start_stream(sensekit_streamconnection_t* connection)
    {
        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::stop_stream(sensekit_streamconnection_t* connection)
    {
        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::open_frame(sensekit_reader_t* reader,
                                                  int timeoutMillis,
                                                  sensekit_reader_frame_t*& frame)
    {
        assert(reader != nullptr);

        StreamReader* actualReader = reinterpret_cast<StreamReader*>(reader);

        //we got the actual frame in the temp_update call.
        //for real, we would do some type of double buffer swap on the client side,
        //copy the latest frame (if newer) from the daemon
        //the daemon might reference count this
        actualReader->lock();
        frame = reader;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::close_frame(sensekit_reader_frame_t*& frame)
    {
        assert(frame != nullptr);

        StreamReader* actualReader = reinterpret_cast<StreamReader*>(frame);
        actualReader->unlock();

        frame = nullptr;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::get_frame(sensekit_reader_frame_t* frame,
                                                 sensekit_stream_type_t type,
                                                 sensekit_stream_subtype_t subType,
                                                 sensekit_frame_ref_t*& frameRef)
    {
        assert(frame != nullptr);

        StreamReader* actualReader = reinterpret_cast<StreamReader*>(frame);

        sensekit_stream_desc_t desc;
        desc.type = type;
        desc.subType = subType;

        frameRef = actualReader->get_subframe(desc);

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

    sensekit_status_t SenseKitContext::set_parameter(sensekit_streamconnection_t* connection,
                                                     sensekit_parameter_id parameterId,
                                                     size_t byteLength,
                                                     sensekit_parameter_data_t* data)
    {
        assert(connection != nullptr);
        assert(connection->handle != nullptr);

        StreamConnection* actualConnection =
            reinterpret_cast<StreamConnection*>(connection->handle);

        Stream* stream = actualConnection->get_stream();

        //TODO: This could perhaps change bin, thus underlying connection parameters (desc)
        stream->set_parameter(actualConnection, parameterId, byteLength, data);

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::get_parameter_size(sensekit_streamconnection_t* connection,
                                                          sensekit_parameter_id parameterId,
                                                          size_t& byteLength)
    {
        assert(connection != nullptr);
        assert(connection->handle != nullptr);

        StreamConnection* actualConnection =
            reinterpret_cast<StreamConnection*>(connection->handle);

        Stream* stream = actualConnection->get_stream();

        stream->get_parameter_size(actualConnection, parameterId, byteLength);

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::get_parameter_data(sensekit_streamconnection_t* connection,
                                                          sensekit_parameter_id parameterId,
                                                          size_t byteLength,
                                                          sensekit_parameter_data_t* data)
    {
        assert(connection != nullptr);
        assert(connection->handle != nullptr);

        StreamConnection* actualConnection =
            reinterpret_cast<StreamConnection*>(connection->handle);

        Stream* stream = actualConnection->get_stream();

        stream->get_parameter_data(actualConnection, parameterId, byteLength, data);

        return SENSEKIT_STATUS_SUCCESS;
    }
}
