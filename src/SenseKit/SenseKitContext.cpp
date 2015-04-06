#include "SenseKitContext.h"
#include "Core/shared_library.h"
#include "Core/libs.h"
#include "StreamReader.h"
#include "StreamConnection.h"
#include "StreamServiceDelegate.h"
#include <Plugins/StreamServiceProxyBase.h>
#include <Plugins/PluginServiceProxyBase.h>
#include <SenseKitAPI.h>
#include "CreateStreamProxy.h"

namespace sensekit {

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

        sensekit_api_set_proxy(get_streamServiceProxy());

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

    sensekit_status_t SenseKitContext::streamset_open(const char* uri, sensekit_streamset_t*& streamSet)
    {
        streamSet = reinterpret_cast<sensekit_streamset_t*>(&get_rootSet());

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::streamset_close(sensekit_streamset_t*& streamSet)
    {
        streamSet = nullptr;

        return SENSEKIT_STATUS_SUCCESS;
    }

    char* SenseKitContext::get_status_string(sensekit_status_t status)
    {
        //TODO
        return nullptr;
    }

    sensekit_status_t SenseKitContext::reader_create(sensekit_streamset_t* streamSet,
                                                     sensekit_reader_t*& reader)
    {
        assert(streamSet != nullptr);

        StreamSet* actualSet = reinterpret_cast<StreamSet*>(streamSet);
        reader = reinterpret_cast<sensekit_reader_t*>(new StreamReader(*actualSet));

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::reader_destroy(sensekit_reader_t*& reader)
    {
        assert(reader != nullptr);

        StreamReader* actualReader = reinterpret_cast<StreamReader*>(reader);

        delete actualReader;
        reader = nullptr;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::reader_get_stream(sensekit_reader_t* reader,
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

    sensekit_status_t SenseKitContext::stream_get_description(sensekit_streamconnection_t* connection, sensekit_stream_desc_t* description)
    {
        //TODO
        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::stream_start(sensekit_streamconnection_t* connection)
    {
        assert(connection != nullptr);
        assert(connection->handle != nullptr);

        StreamConnection* actualConnection =
            reinterpret_cast<StreamConnection*>(connection->handle);

        actualConnection->start();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::stream_stop(sensekit_streamconnection_t* connection)
    {
        assert(connection != nullptr);
        assert(connection->handle != nullptr);

        StreamConnection* actualConnection =
            reinterpret_cast<StreamConnection*>(connection->handle);

        actualConnection->stop();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::reader_open_frame(sensekit_reader_t* reader,
                                                  int timeoutMillis,
                                                  sensekit_reader_frame_t*& frame)
    {
        assert(reader != nullptr);

        StreamReader* actualReader = reinterpret_cast<StreamReader*>(reader);

        actualReader->lock();
        frame = reader;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::reader_close_frame(sensekit_reader_frame_t*& frame)
    {
        assert(frame != nullptr);

        StreamReader* actualReader = reinterpret_cast<StreamReader*>(frame);
        actualReader->unlock();

        frame = nullptr;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::reader_get_frame(sensekit_reader_frame_t* frame,
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

    sensekit_status_t SenseKitContext::stream_set_parameter(sensekit_streamconnection_t* connection,
                                                     sensekit_parameter_id parameterId,
                                                     size_t byteLength,
                                                     sensekit_parameter_data_t* data)
    {
        assert(connection != nullptr);
        assert(connection->handle != nullptr);

        StreamConnection* actualConnection =
            reinterpret_cast<StreamConnection*>(connection->handle);

        actualConnection->set_parameter(parameterId, byteLength, data);

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::stream_get_parameter_size(sensekit_streamconnection_t* connection,
                                                          sensekit_parameter_id parameterId,
                                                          size_t& byteLength)
    {
        assert(connection != nullptr);
        assert(connection->handle != nullptr);

        StreamConnection* actualConnection =
            reinterpret_cast<StreamConnection*>(connection->handle);

        actualConnection->get_parameter_size(parameterId, byteLength);

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::stream_get_parameter_data(sensekit_streamconnection_t* connection,
                                                          sensekit_parameter_id parameterId,
                                                          size_t byteLength,
                                                          sensekit_parameter_data_t* data)
    {
        assert(connection != nullptr);
        assert(connection->handle != nullptr);

        StreamConnection* actualConnection =
            reinterpret_cast<StreamConnection*>(connection->handle);

        actualConnection->get_parameter_data(parameterId, byteLength, data);

        return SENSEKIT_STATUS_SUCCESS;
    }
}
