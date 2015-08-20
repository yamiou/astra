#include <Astra/astra_types.h>
#include "StreamConnection.h"
#include "StreamBin.h"
#include "Stream.h"
#include <cassert>

namespace astra {

    StreamConnection::StreamConnection(Stream* stream)
        : m_stream(stream)
    {
        assert (stream != nullptr);

        m_connection.handle =
            reinterpret_cast<astra_streamconnection_handle_t>(this);

        m_connection.desc = stream->get_description();

        m_binFrontBufferReadyCallback =
            [this](StreamBin* b, astra_frame_index_t frameIndex)
            {
                this->on_bin_front_buffer_ready(b, frameIndex);
            };
    }

    StreamConnection::~StreamConnection()
    {
        clear_pending_parameter_result();
        set_bin(nullptr);
    }

    astra_bin_t StreamConnection::get_bin_handle()
    {
        if (m_bin != nullptr)
        {
            return m_bin->get_handle();
        }
        else
        {
            return nullptr;
        }
    }

    const astra_stream_desc_t& StreamConnection::get_description() const
    {
        return m_stream->get_description();
    }

    astra_frame_t* StreamConnection::lock()
    {
        STRACE("StreamConnection", "%x lock", this);
        if (m_locked)
        {
            return m_currentFrame;
        }

        if (m_bin != nullptr)
        {
            m_currentFrame = m_bin->lock_front_buffer();
        }
        else
        {
            m_currentFrame = nullptr;
        }

        m_locked = true;

        return m_currentFrame;
    }

    void StreamConnection::unlock()
    {
        STRACE("StreamConnection", "%x unlock", this);

        if (!m_locked)
        {
            SWARN("StreamConnection", "%x StreamConnection::unlock() not locked", this);
            assert(m_locked);
        }

        if (m_currentFrame != nullptr && m_bin != nullptr)
        {
            m_currentFrame = nullptr;
            m_locked = false;
            m_bin->unlock_front_buffer();
        }
    }

    void StreamConnection::start()
    {
        if (m_started)
            return;

        m_started = true;
    }

    void StreamConnection::stop()
    {
        if (!m_started)
            return;

        m_started = false;
    }

    void StreamConnection::set_bin(StreamBin* bin)
    {
        if (m_bin != nullptr)
        {
            m_bin->unregister_front_buffer_ready_callback(m_binFrontBufferReadyCallbackId);
            m_bin->dec_connected();
        }

        m_bin = bin;

        if (m_bin != nullptr)
        {
            m_bin->inc_connected();

            m_binFrontBufferReadyCallbackId =
                m_bin->register_front_buffer_ready_callback(m_binFrontBufferReadyCallback);
        }
    }

    void StreamConnection::on_bin_front_buffer_ready(StreamBin* bin, astra_frame_index_t frameIndex)
    {
        assert(m_bin != nullptr);
        assert(m_bin == bin);
        m_frameReadySignal.raise(this, frameIndex);
    }

    astra_callback_id_t StreamConnection::register_frame_ready_callback(FrameReadyCallback callback)
    {
        return m_frameReadySignal += callback;
    }

    void StreamConnection::unregister_frame_ready_callback(astra_callback_id_t& callbackId)
    {
        m_frameReadySignal -= callbackId;
        callbackId = 0;
    }

    void StreamConnection::set_parameter(astra_parameter_id id,
                                         size_t inByteLength,
                                         astra_parameter_data_t inData)
    {
        m_stream->set_parameter(this, id, inByteLength, inData);
    }

    void StreamConnection::get_parameter(astra_parameter_id id,
                                         size_t& resultByteLength,
                                         astra_result_token_t& token)
    {
        astra_parameter_bin_t parameterBinHandle = nullptr;
        m_stream->get_parameter(this, id, parameterBinHandle);

        assert(parameterBinHandle != nullptr);
        cache_parameter_bin_token(parameterBinHandle, resultByteLength, token);
    }

    astra_status_t StreamConnection::get_result(astra_result_token_t token,
                                                   size_t dataByteLength,
                                                   astra_parameter_data_t dataDestination)
    {
        ParameterBin* parameterBin = ParameterBin::get_ptr(token);
        if (m_pendingParameterResult != nullptr && parameterBin == m_pendingParameterResult)
        {
            memcpy(dataDestination, parameterBin->data(), dataByteLength);
            clear_pending_parameter_result();
            return ASTRA_STATUS_SUCCESS;
        }
        else
        {
            //Results are read-once-only, then they self destruct.
            //Client tried to get result in wrong order, or with duplicate or stale token
            SWARN("StreamConnection", "no pending parameter result.", this);
            clear_pending_parameter_result();
            return ASTRA_STATUS_INVALID_PARAMETER_TOKEN;
        }
    }

    void StreamConnection::invoke(astra_command_id commandId,
                                  size_t inByteLength,
                                  astra_parameter_data_t inData,
                                  size_t& resultByteLength,
                                  astra_result_token_t& token)
    {
        astra_parameter_bin_t parameterBinHandle = nullptr;
        m_stream->invoke(this, commandId, inByteLength, inData, parameterBinHandle);

        cache_parameter_bin_token(parameterBinHandle, resultByteLength, token);
    }

    void StreamConnection::cache_parameter_bin_token(astra_parameter_bin_t parameterBinHandle,
                                                     size_t& resultByteLength,
                                                     astra_result_token_t& token)
    {
        //delete old parameter result -- only one outstanding allowed at a time
        clear_pending_parameter_result();
        if (parameterBinHandle != nullptr)
        {
            m_pendingParameterResult = ParameterBin::get_ptr(parameterBinHandle);
            resultByteLength = m_pendingParameterResult->byteLength();
            token = parameterBinHandle;
        }
        else
        {
            m_pendingParameterResult = nullptr;
            resultByteLength = 0;
            token = nullptr;
        }
    }

    void StreamConnection::clear_pending_parameter_result()
    {
        if (m_pendingParameterResult != nullptr)
        {
            delete m_pendingParameterResult;
            m_pendingParameterResult = nullptr;
        }
    }
}
