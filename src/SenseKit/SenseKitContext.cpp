#include "SenseKitContext.h"
#include <SenseKit/sensekit_capi.h>
#include <SenseKit/Plugins/plugin_capi.h>
#include <SenseKitAPI.h>
#include "StreamReader.h"
#include "StreamConnection.h"
#include "StreamSetConnection.h"
#include "StreamServiceDelegate.h"
#include "CreateStreamProxy.h"
#include "Logging.h"
#include "Core/OSProcesses.h"
#include "sensekit_private.h"

INITIALIZE_LOGGING

namespace sensekit {

    const char PLUGIN_DIRECTORY[] = "./Plugins/";

    sensekit_status_t SenseKitContext::initialize()
    {
        if (m_initialized)
            return SENSEKIT_STATUS_SUCCESS;

#if __ANDROID__
        std::string logPath = get_application_filepath() + "sensekit.log";
#else
        std::string logPath = "logs/sensekit.log";
#endif
        initialize_logging(logPath.c_str());

        m_logger = std::make_unique<Logger>("Context");

        m_logger->warn("Hold on to yer butts");
#ifdef NYAN_MODE
        log_nyan(*m_logger.get());
#endif
        m_logger->info("logger file: %s", logPath.c_str());

        m_streamServiceProxy = create_stream_proxy(this);
        sensekit_api_set_proxy(get_streamServiceProxy());
        m_logger->trace("API Proxy set");

        m_pluginManager = std::make_unique<PluginManager>(*this);

#if !__ANDROID__
        m_pluginManager->load_plugins(PLUGIN_DIRECTORY);
#else
        m_pluginManager->load_plugin("libopenni_sensor.so");
        m_pluginManager->load_plugin("liborbbec_hand.so");
        m_pluginManager->load_plugin("liborbbec_xs.so");
#endif

        if (m_pluginManager->plugin_count() == 0)
        {
            m_logger->warn("SenseKit found no plugins. Is there a Plugins folder? Is the working directory correct?");
        }

        m_initialized = true;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::terminate()
    {
        if (!m_initialized)
            return SENSEKIT_STATUS_UNINITIALIZED;

        m_pluginManager.reset();

        if (m_streamServiceProxy)
            delete m_streamServiceProxy;

        m_initialized = false;

        m_logger->info("SenseKit terminated.");

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::streamset_open(const char* uri, sensekit_streamsetconnection_t& streamSet)
    {
        m_logger->info("client opening streamset: %s", uri);

        StreamSetConnection& conn = m_setCatalog.open_set_connection(uri);
        streamSet = conn.get_handle();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::streamset_close(sensekit_streamsetconnection_t& streamSet)
    {
        if (!m_initialized)
        {
            streamSet = nullptr;
            return SENSEKIT_STATUS_SUCCESS;
        }

        StreamSetConnection* actualConnection = StreamSetConnection::get_ptr(streamSet);

        if (actualConnection)
        {
            m_setCatalog.close_set_connection(actualConnection);
        }
        else
        {
            m_logger->warn("attempt to close a non-existent stream set");
        }

        streamSet = nullptr;

        return actualConnection != nullptr ? SENSEKIT_STATUS_SUCCESS : SENSEKIT_STATUS_INVALID_PARAMETER;
    }

    // char* SenseKitContext::get_status_string(sensekit_status_t status)
    // {
    //     //TODO
    //     return nullptr;
    // }

    sensekit_status_t SenseKitContext::reader_create(sensekit_streamsetconnection_t streamSet,
                                                     sensekit_reader_t& reader)
    {
        assert(streamSet != nullptr);

        StreamSetConnection* actualConnection = StreamSetConnection::get_ptr(streamSet);

        if (actualConnection)
        {
            StreamReader* actualReader = actualConnection->create_reader();
            m_activeReaders.push_back(actualReader);

            reader = actualReader->get_handle();

            return SENSEKIT_STATUS_SUCCESS;
        }
        else
        {
            m_logger->warn("attempt to create reader from non-existent stream set");
            return SENSEKIT_STATUS_INVALID_PARAMETER;
        }
    }

    sensekit_status_t SenseKitContext::reader_destroy(sensekit_reader_t& reader)
    {
        assert(reader != nullptr);

        StreamReader* actualReader = StreamReader::get_ptr(reader);

        if (actualReader)
        {
            StreamSetConnection& connection = actualReader->get_connection();
            connection.destroy_reader(actualReader);
        }
        else
        {
            m_logger->warn("attempt to destroy a non-existent reader: %p", reader);
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

        if (actualReader)
        {
            sensekit_stream_desc_t desc;
            desc.type = type;
            desc.subtype = subtype;

            connection = actualReader->get_stream(desc)->get_handle();
        }
        else
        {
            m_logger->warn("get_stream called on non-existent reader");
            connection = nullptr;
        }

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::stream_get_description(sensekit_streamconnection_t connection,
                                                              sensekit_stream_desc_t* description)
    {
        StreamConnection* actualConnection = StreamConnection::get_ptr(connection);

        if (actualConnection)
        {
            *description = actualConnection->get_description();
        }
        else
        {
            m_logger->warn("get_description called on non-existent stream");
        }

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::stream_start(sensekit_streamconnection_t connection)
    {
        assert(connection != nullptr);
        assert(connection->handle != nullptr);

        StreamConnection* actualConnection = StreamConnection::get_ptr(connection);

        if (actualConnection)
        {
            actualConnection->start();
        }
        else
        {
            m_logger->warn("start called on non-existent stream");
        }

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::stream_stop(sensekit_streamconnection_t connection)
    {
        assert(connection != nullptr);
        assert(connection->handle != nullptr);

        StreamConnection* actualConnection = StreamConnection::get_ptr(connection);

        if (actualConnection)
        {
            actualConnection->start();
        }
        else
        {
            m_logger->warn("stop called on non-existent stream");
        }

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t SenseKitContext::reader_open_frame(sensekit_reader_t reader,
                                                         int timeoutMillis,
                                                         sensekit_reader_frame_t& frame)
    {
        if (reader == nullptr)
        {
            m_logger->warn("reader_open_frame called with null reader");
            assert(reader != nullptr);
            return SENSEKIT_STATUS_INVALID_OPERATION;
        }

        StreamReader* actualReader = StreamReader::get_ptr(reader);

        if (actualReader)
        {
            return actualReader->lock(timeoutMillis, frame);
        }
        else
        {
            m_logger->warn("open_frame called on non-existent reader");
            return SENSEKIT_STATUS_INVALID_PARAMETER;
        }
    }

    sensekit_status_t SenseKitContext::reader_close_frame(sensekit_reader_frame_t& frame)
    {
        if (frame == nullptr)
        {
            m_logger->warn("reader_close_frame called with null frame");
            assert(frame != nullptr);
            return SENSEKIT_STATUS_INVALID_OPERATION;
        }

        StreamReader* actualReader = StreamReader::from_frame(frame);

        if (!actualReader)
        {
            m_logger->warn("reader_close_frame couldn't retrieve StreamReader from frame");
            assert(actualReader != nullptr);
            return SENSEKIT_STATUS_INTERNAL_ERROR;
        }

        return actualReader->unlock(frame);
    }

    sensekit_status_t SenseKitContext::reader_register_frame_ready_callback(sensekit_reader_t reader,
                                                                            sensekit_frame_ready_callback_t callback,
                                                                            void* clientTag,
                                                                            sensekit_reader_callback_id_t& callbackId)
    {
        assert(reader != nullptr);
        callbackId = nullptr;

        StreamReader* actualReader = StreamReader::get_ptr(reader);

        if (actualReader)
        {
            CallbackId cbId = actualReader->register_frame_ready_callback(callback, clientTag);

            sensekit_reader_callback_id_t cb = new _sensekit_reader_callback_id;
            callbackId = cb;

            cb->reader = reader;
            cb->callbackId = cbId;

            return SENSEKIT_STATUS_SUCCESS;
        }
        else
        {
            m_logger->warn("register_frame_ready_callback called on non-existent reader");
            return SENSEKIT_STATUS_INVALID_PARAMETER;
        }
    }

    sensekit_status_t SenseKitContext::reader_unregister_frame_ready_callback(sensekit_reader_callback_id_t& callbackId)
    {
        if (!m_initialized)
        {
            delete callbackId;
            callbackId = nullptr;
            return SENSEKIT_STATUS_SUCCESS;
        }

        sensekit_reader_callback_id_t cb = callbackId;
        assert(cb != nullptr);
        assert(cb->reader != nullptr);

        CallbackId cbId = cb->callbackId;
        StreamReader* actualReader = StreamReader::get_ptr(cb->reader);

        if (actualReader)
        {
            actualReader->unregister_frame_ready_callback(cbId);
        }
        else
        {
            m_logger->warn("unregister_frame_ready_callback for non-existent reader: %p", cb->reader);
        }

        delete cb;
        callbackId = nullptr;

        return actualReader != nullptr ? SENSEKIT_STATUS_SUCCESS : SENSEKIT_STATUS_INVALID_PARAMETER;
    }

    sensekit_status_t SenseKitContext::reader_get_frame(sensekit_reader_frame_t frame,
                                                        sensekit_stream_type_t type,
                                                        sensekit_stream_subtype_t subtype,
                                                        sensekit_frame_t*& subFrame)
    {
        assert(frame != nullptr);

        StreamReader* actualReader = StreamReader::from_frame(frame);

        if (actualReader)
        {
            sensekit_stream_desc_t desc;
            desc.type = type;
            desc.subtype = subtype;

            subFrame = actualReader->get_subframe(desc);
            return SENSEKIT_STATUS_SUCCESS;
        }
        else
        {
            m_logger->warn("get_frame called on non-existent reader/frame combo");
            return SENSEKIT_STATUS_INVALID_PARAMETER;
        }
    }

    sensekit_status_t SenseKitContext::temp_update()
    {
        m_pluginManager->update();

        return SENSEKIT_STATUS_SUCCESS;
    }

    void SenseKitContext::raise_existing_streams_added(stream_added_callback_t callback, void* clientTag)
    {
        m_setCatalog.visit_sets(
            [&callback, &clientTag] (StreamSet* set)
            {
                sensekit_streamset_t setHandle = set->get_handle();
                set->visit_streams(
                    [&setHandle, &callback, &clientTag] (Stream* stream)
                    {
                        callback(clientTag, setHandle, stream->get_handle(), stream->get_description());
                    });
            });
    }

    sensekit_status_t SenseKitContext::stream_set_parameter(sensekit_streamconnection_t connection,
                                                            sensekit_parameter_id parameterId,
                                                            size_t inByteLength,
                                                            sensekit_parameter_data_t inData)
    {
        assert(connection != nullptr);
        assert(connection->handle != nullptr);

        StreamConnection* actualConnection = StreamConnection::get_ptr(connection);

        if (actualConnection)
        {
            actualConnection->set_parameter(parameterId, inByteLength, inData);
            return SENSEKIT_STATUS_SUCCESS;
        }
        else
        {
            m_logger->warn("set_parameter called on non-existent stream");
            return SENSEKIT_STATUS_INVALID_PARAMETER;
        }
    }

    sensekit_status_t SenseKitContext::stream_get_parameter(sensekit_streamconnection_t connection,
                                                            sensekit_parameter_id parameterId,
                                                            size_t& resultByteLength,
                                                            sensekit_result_token_t& token)
    {
        assert(connection != nullptr);
        assert(connection->handle != nullptr);

        StreamConnection* actualConnection = StreamConnection::get_ptr(connection);

        if (actualConnection)
        {
            actualConnection->get_parameter(parameterId, resultByteLength, token);
            return SENSEKIT_STATUS_SUCCESS;
        }
        else
        {
            m_logger->warn("get_parameter called on non-existent stream");
            return SENSEKIT_STATUS_INVALID_PARAMETER;
        }
    }

    sensekit_status_t SenseKitContext::stream_get_result(sensekit_streamconnection_t connection,
                                                         sensekit_result_token_t token,
                                                         size_t dataByteLength,
                                                         sensekit_parameter_data_t dataDestination)
    {
        assert(connection != nullptr);
        assert(connection->handle != nullptr);

        StreamConnection* actualConnection = StreamConnection::get_ptr(connection);
        if (actualConnection)
        {
            return actualConnection->get_result(token, dataByteLength, dataDestination);
        }
        else
        {
            m_logger->warn("get_result called on non-existent stream");
            return SENSEKIT_STATUS_INVALID_PARAMETER;
        }
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

        if (actualConnection)
        {
            actualConnection->invoke(commandId, inByteLength, inData, resultByteLength, token);
            return SENSEKIT_STATUS_SUCCESS;
        }
        else
        {
            m_logger->warn("invoke called on non-existent stream");
            return SENSEKIT_STATUS_INVALID_PARAMETER;
        }
    }

    sensekit_status_t SenseKitContext::notify_host_event(sensekit_event_id id, const void* data, size_t dataSize)
    {
        m_pluginManager->notify_host_event(id, data, dataSize);
        return SENSEKIT_STATUS_SUCCESS;
    }
}
