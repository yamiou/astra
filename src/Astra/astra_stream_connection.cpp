#include <Astra/astra_types.h>
#include "astra_stream_connection.hpp"
#include "astra_stream_bin.hpp"
#include "astra_stream.hpp"
#include <cassert>

namespace astra {

    stream_connection::stream_connection(stream* stream)
        : m_stream(stream)
    {
        assert (stream != nullptr);

        m_connection.handle =
            reinterpret_cast<astra_streamconnection_handle_t>(this);

        m_connection.desc = stream->get_description();

        m_binFrontBufferReadyCallback =
            [this](stream_bin* b, astra_frame_index_t frameIndex)
            {
                this->on_bin_front_buffer_ready(b, frameIndex);
            };
    }

    stream_connection::~stream_connection()
    {
        clear_pending_parameter_result();
        set_bin(nullptr);
    }

    astra_bin_t stream_connection::get_bin_handle()
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

    const astra_stream_desc_t& stream_connection::get_description() const
    {
        return m_stream->get_description();
    }

    astra_frame_t* stream_connection::lock()
    {
        LOG_TRACE("astra.stream_connection", "%x lock", this);

        if (m_locked)
        {
            return m_currentFrame;
        }

        if (is_started() && m_bin != nullptr)
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

    void stream_connection::unlock()
    {
        LOG_TRACE("astra.stream_connection", "%x unlock", this);

        if (!m_locked)
        {
            LOG_WARN("astra.stream_connection", "%x stream_connection::unlock() not locked", this);
            assert(m_locked);
        }

        if (is_started() && m_bin != nullptr)
        {
            m_bin->unlock_front_buffer();
        }

        m_currentFrame = nullptr;
        m_locked = false;
    }

    void stream_connection::start()
    {
        if (m_started)
            return;

        LOG_DEBUG("astra.stream_connection",
                  "%p starting (%d, %d)",
                  this,
                  get_description().type,
                  get_description().subtype);

        m_stream->start_connection(this);
        m_started = true;
    }

    void stream_connection::stop()
    {
        if (!m_started)
            return;

        LOG_DEBUG("astra.stream_connection",
                  "%p stopping, stream = (%u, %u)",
                  this,
                  get_description().type,
                  get_description().subtype);

        m_stream->stop_connection(this);
        m_started = false;
    }

    void stream_connection::set_bin(stream_bin* bin)
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

    void stream_connection::on_bin_front_buffer_ready(stream_bin* bin, astra_frame_index_t frameIndex)
    {
        assert(m_bin != nullptr);
        assert(m_bin == bin);

        m_frameReadySignal.raise(this, frameIndex);
    }

    astra_callback_id_t stream_connection::register_frame_ready_callback(FrameReadyCallback callback)
    {
        return m_frameReadySignal += callback;
    }

    void stream_connection::unregister_frame_ready_callback(astra_callback_id_t& callbackId)
    {
        m_frameReadySignal -= callbackId;
        callbackId = 0;
    }

    void stream_connection::set_parameter(astra_parameter_id id,
                                         size_t inByteLength,
                                         astra_parameter_data_t inData)
    {
        m_stream->set_parameter(this, id, inByteLength, inData);
    }

    void stream_connection::get_parameter(astra_parameter_id id,
                                         size_t& resultByteLength,
                                         astra_result_token_t& token)
    {
        astra_parameter_bin_t parameterBinHandle = nullptr;
        m_stream->get_parameter(this, id, parameterBinHandle);

        assert(parameterBinHandle != nullptr);
        cache_parameter_bin_token(parameterBinHandle, resultByteLength, token);
    }

    astra_status_t stream_connection::get_result(astra_result_token_t token,
                                                   size_t dataByteLength,
                                                   astra_parameter_data_t dataDestination)
    {
        parameter_bin* parameterBin = parameter_bin::get_ptr(token);
        if (m_pendingParameterResult != nullptr && parameterBin == m_pendingParameterResult)
        {
            std::memcpy(dataDestination, parameterBin->data(), dataByteLength);
            clear_pending_parameter_result();
            return ASTRA_STATUS_SUCCESS;
        }
        else
        {
            //Results are read-once-only, then they self destruct.
            //Client tried to get result in wrong order, or with duplicate or stale token
            LOG_WARN("astra.stream_connection", "%p no pending parameter result.", this);
            clear_pending_parameter_result();
            return ASTRA_STATUS_INVALID_PARAMETER_TOKEN;
        }
    }

    void stream_connection::invoke(astra_command_id commandId,
                                  size_t inByteLength,
                                  astra_parameter_data_t inData,
                                  size_t& resultByteLength,
                                  astra_result_token_t& token)
    {
        astra_parameter_bin_t parameterBinHandle = nullptr;
        m_stream->invoke(this, commandId, inByteLength, inData, parameterBinHandle);

        cache_parameter_bin_token(parameterBinHandle, resultByteLength, token);
    }

    void stream_connection::cache_parameter_bin_token(astra_parameter_bin_t parameterBinHandle,
                                                     size_t& resultByteLength,
                                                     astra_result_token_t& token)
    {
        //delete old parameter result -- only one outstanding allowed at a time
        clear_pending_parameter_result();
        if (parameterBinHandle != nullptr)
        {
            m_pendingParameterResult = parameter_bin::get_ptr(parameterBinHandle);
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

    void stream_connection::clear_pending_parameter_result()
    {
        if (m_pendingParameterResult != nullptr)
        {
            delete m_pendingParameterResult;
            m_pendingParameterResult = nullptr;
        }
    }
}
