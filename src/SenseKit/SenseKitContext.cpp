#include "SenseKitContext.h"
#include <SenseKit/sensekit_capi.h>
#include <SenseKit/Plugins/plugin_capi.h>
#include <SenseKitAPI.h>
#include "Core/shared_library.h"
#include "Core/libs.h"
#include "StreamReader.h"
#include "StreamConnection.h"
#include "StreamServiceDelegate.h"
#include "CreateStreamProxy.h"
#include "Logging.h"

INITIALIZE_LOGGING

namespace sensekit {

    SenseKitContext::SenseKitContext()
        : m_pluginService(*this),
          m_logger("Context")
    {}

    SenseKitContext::~SenseKitContext()
    {}

    sensekit_status_t SenseKitContext::initialize()
    {
        if (m_initialized)
            return SENSEKIT_STATUS_SUCCESS;

        m_logger.warn("Hold on to yer butts");
        m_pluginServiceProxy = m_pluginService.create_proxy();
        m_streamServiceProxy = create_stream_proxy(this);

        sensekit_api_set_proxy(get_streamServiceProxy());

        //TODO: OMG ERROR HANDLING
        LibHandle libHandle = nullptr;

        std::vector<std::string> libs = get_libs();
        if (libs.size() == 0)
        {
            m_logger.warn("Warning: Sensekit found no plugins. Is there a Plugins folder? Is the working directory correct?");
        }

        for(auto lib : libs)
        {
            os_load_library((PLUGIN_DIRECTORY + lib).c_str(), libHandle);

            PluginFuncs pluginFuncs;
            os_get_proc_address(libHandle, SK_STRINGIFY(sensekit_plugin_initialize), (FarProc&)pluginFuncs.initialize);
            os_get_proc_address(libHandle, SK_STRINGIFY(sensekit_plugin_terminate), (FarProc&)pluginFuncs.terminate);
            os_get_proc_address(libHandle, SK_STRINGIFY(sensekit_plugin_update), (FarProc&)pluginFuncs.update);
            pluginFuncs.libHandle = libHandle;

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

        m_initialized = true;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::terminate()
    {
        if (!m_initialized)
            return SENSEKIT_STATUS_SUCCESS;

        for(auto pluginFuncs : m_pluginList)
        {
            pluginFuncs.terminate();
            os_free_library(pluginFuncs.libHandle);
        }

        if (m_pluginServiceProxy)
            delete m_pluginServiceProxy;

        if (m_streamServiceProxy)
            delete m_streamServiceProxy;

        m_initialized = false;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::streamset_open(const char* uri, sensekit_streamset_t& streamSet)
    {
        streamSet = get_rootSet().get_handle();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::streamset_close(sensekit_streamset_t& streamSet)
    {
        streamSet = nullptr;

        return SENSEKIT_STATUS_SUCCESS;
    }

    char* SenseKitContext::get_status_string(sensekit_status_t status)
    {
        //TODO
        return nullptr;
    }

    sensekit_status_t SenseKitContext::reader_create(sensekit_streamset_t streamSet,
                                                     sensekit_reader_t& reader)
    {
        assert(streamSet != nullptr);

        StreamSet* actualSet = StreamSet::get_ptr(streamSet);
        ReaderPtr actualReader(new StreamReader(*actualSet));

        reader = actualReader->get_handle();

        m_readers.push_back(std::move(actualReader));

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::reader_destroy(sensekit_reader_t& reader)
    {
        assert(reader != nullptr);

        StreamReader* actualReader = StreamReader::get_ptr(reader);

        auto it = std::find_if(m_readers.begin(),
                               m_readers.end(),
                               [&actualReader] (ReaderPtr& element)
                               -> bool
                               {
                                   return actualReader == element.get();
                               });

        if (it != m_readers.end())
        {
            m_readers.erase(it);
        }

        reader = nullptr;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::reader_get_stream(sensekit_reader_t reader,
                                                         sensekit_stream_type_t type,
                                                         sensekit_stream_subtype_t subtype,
                                                         sensekit_streamconnection_t& connection)
    {
        assert(reader != nullptr);

        StreamReader* actualReader = StreamReader::get_ptr(reader);

        sensekit_stream_desc_t desc;
        desc.type = type;
        desc.subtype = subtype;

        connection = actualReader->get_stream(desc)->get_handle();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::stream_get_description(sensekit_streamconnection_t connection,
                                                              sensekit_stream_desc_t* description)
    {
        *description = StreamConnection::get_ptr(connection)->get_description();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::stream_start(sensekit_streamconnection_t connection)
    {
        assert(connection != nullptr);
        assert(connection->handle != nullptr);

        StreamConnection* actualConnection = StreamConnection::get_ptr(connection);

        actualConnection->start();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::stream_stop(sensekit_streamconnection_t connection)
    {
        assert(connection != nullptr);
        assert(connection->handle != nullptr);

        StreamConnection* actualConnection = StreamConnection::get_ptr(connection);

        actualConnection->stop();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::reader_open_frame(sensekit_reader_t reader,
                                                         int timeoutMillis,
                                                         sensekit_reader_frame_t& frame)
    {
        assert(reader != nullptr);

        StreamReader* actualReader = StreamReader::get_ptr(reader);
        sensekit_status_t rc = actualReader->lock(timeoutMillis);

        if (rc == SENSEKIT_STATUS_SUCCESS)
        {
            frame = reader;
        }
        else
        {
            frame = nullptr;
        }

        return rc;
    }

    sensekit_status_t SenseKitContext::reader_close_frame(sensekit_reader_frame_t& frame)
    {
        assert(frame != nullptr);

        StreamReader* actualReader = StreamReader::from_frame(frame);
        actualReader->unlock();

        frame = nullptr;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::reader_register_frame_ready_callback(sensekit_reader_t reader,
                                                                            sensekit_frame_ready_callback_t callback,
                                                                            void* clientTag,
                                                                            sensekit_reader_callback_id_t& callbackId)
    {
        assert(reader != nullptr);
        callbackId = nullptr;

        StreamReader* actualReader = StreamReader::get_ptr(reader);

        CallbackId cbId = actualReader->register_frame_ready_callback(callback, clientTag);

        sensekit_reader_callback_id_t cb = new _sensekit_reader_callback_id;
        callbackId = cb;

        cb->reader = reader;
        cb->callbackId = cbId;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::reader_unregister_frame_ready_callback(sensekit_reader_callback_id_t& callbackId)
    {
        sensekit_reader_callback_id_t cb = callbackId;
        assert(cb != nullptr);
        assert(cb->reader != nullptr);

        CallbackId cbId = cb->callbackId;
        StreamReader* actualReader = StreamReader::get_ptr(cb->reader);

        delete cb;
        callbackId = nullptr;
        actualReader->unregister_frame_ready_callback(cbId);

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::reader_get_frame(sensekit_reader_frame_t frame,
                                                        sensekit_stream_type_t type,
                                                        sensekit_stream_subtype_t subtype,
                                                        sensekit_frame_ref_t*& frameRef)
    {
        assert(frame != nullptr);

        StreamReader* actualReader = StreamReader::from_frame(frame);

        sensekit_stream_desc_t desc;
        desc.type = type;
        desc.subtype = subtype;

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

    void SenseKitContext::raise_existing_streams_added(stream_added_callback_t callback, void* clientTag)
    {
        //TODO loop for all created rootsets
        StreamSet set = get_rootSet();
        auto setHandle = set.get_handle();

        std::function<void(Stream*)> visitor = [setHandle, callback, clientTag](Stream* stream)
            {
                callback(clientTag, setHandle, stream->get_handle(), stream->get_description());
            };

        set.visit_streams(visitor);
    }

    sensekit_status_t SenseKitContext::stream_set_parameter(sensekit_streamconnection_t connection,
                                                            sensekit_parameter_id parameterId,
                                                            size_t inByteLength,
                                                            sensekit_parameter_data_t inData)
    {
        assert(connection != nullptr);
        assert(connection->handle != nullptr);

        StreamConnection* actualConnection = StreamConnection::get_ptr(connection);
        actualConnection->set_parameter(parameterId, inByteLength, inData);

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::stream_get_parameter(sensekit_streamconnection_t connection,
                                                            sensekit_parameter_id parameterId,
                                                            size_t& resultByteLength,
                                                            sensekit_result_token_t& token)
    {
        assert(connection != nullptr);
        assert(connection->handle != nullptr);

        StreamConnection* actualConnection = StreamConnection::get_ptr(connection);

        actualConnection->get_parameter(parameterId, resultByteLength, token);

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::stream_get_result(sensekit_streamconnection_t connection,
                                                         sensekit_result_token_t token,
                                                         size_t dataByteLength,
                                                         sensekit_parameter_data_t dataDestination)
    {
        assert(connection != nullptr);
        assert(connection->handle != nullptr);

        StreamConnection* actualConnection = StreamConnection::get_ptr(connection);
        return actualConnection->get_result(token, dataByteLength, dataDestination);
    }

    sensekit_status_t SenseKitContext::stream_invoke(sensekit_streamconnection_t connection,
                                                     sensekit_command_id commandId,
                                                     size_t inByteLength,
                                                     sensekit_parameter_data_t inData,
                                                     size_t& resultByteLength,
                                                     sensekit_result_token_t& token)
    {
        assert(connection != nullptr);
        assert(connection->handle != nullptr);

        StreamConnection* actualConnection = StreamConnection::get_ptr(connection);
        actualConnection->invoke(commandId, inByteLength, inData, resultByteLength, token);

        return SENSEKIT_STATUS_SUCCESS;
    }
}
