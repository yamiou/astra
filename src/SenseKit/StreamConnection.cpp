#include <SenseKit/sensekit_types.h>
#include "StreamConnection.h"
#include "StreamBin.h"
#include "Stream.h"
#include "ParameterBin.h"

namespace sensekit {
    using namespace std::placeholders;

    StreamConnection::StreamConnection(Stream* stream)
        : m_stream(stream)
    {
        assert (stream != nullptr);

        m_connection.handle =
            reinterpret_cast<sensekit_streamconnection_handle_t>(this);

        m_connection.desc = stream->get_description();

        m_binFrontBufferReadyCallback =
            [this](StreamBin* b, sensekit_frame_index_t frameIndex)
            {
                this->on_bin_front_buffer_ready(b, frameIndex);
            };
    }

    sensekit_bin_t StreamConnection::get_bin_handle()
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

    const sensekit_stream_desc_t& StreamConnection::get_description() const
    {
        return m_stream->get_description();
    }

    sensekit_frame_ref_t* StreamConnection::lock()
    {
        if (m_locked)
            return &m_currentFrame;

        if (m_bin != nullptr)
        {
            m_currentFrame.frame = m_bin->lock_front_buffer();
        }
        else
        {
            m_currentFrame.frame = nullptr;
        }

        m_currentFrame.streamConnection = &m_connection;

        m_locked = true;

        return &m_currentFrame;
    }

    void StreamConnection::unlock()
    {
        if (!m_locked)
            return;

        if (m_bin != nullptr)
            m_bin->unlock_front_buffer();

        m_locked = false;
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

    void StreamConnection::on_bin_front_buffer_ready(StreamBin* bin, sensekit_frame_index_t frameIndex)
    {
        assert(m_bin != nullptr);
        assert(m_bin == bin);
        m_frameReadySignal.raise(this, frameIndex);
    }

    sensekit_callback_id_t StreamConnection::register_frame_ready_callback(FrameReadyCallback callback)
    {
        return m_frameReadySignal += callback;
    }

    void StreamConnection::unregister_frame_ready_callback(sensekit_callback_id_t& callbackId)
    {
        m_frameReadySignal -= callbackId;
        callbackId = 0;
    }

    void StreamConnection::set_parameter(sensekit_parameter_id id,
                                         size_t inByteLength,
                                         sensekit_parameter_data_t inData)
    {
        m_stream->set_parameter(this, id, inByteLength, inData);
    }

    void StreamConnection::get_parameter(sensekit_parameter_id id, 
                                         size_t& resultByteLength, 
                                         sensekit_result_token_t& token)
    {

        sensekit_parameter_bin_t parameterBinHandle;
        m_stream->get_parameter(this, id, parameterBinHandle);
        
        ParameterBin* parameterBin = ParameterBin::get_ptr(parameterBinHandle);
        resultByteLength = parameterBin->byteLength();
        token = parameterBinHandle;
        //TODO: cache resultData with a token lookup, set the token out parameter
    }

    void StreamConnection::get_result(sensekit_result_token_t token, 
                                      size_t dataByteLength, 
                                      sensekit_parameter_data_t dataDestination)
    {
        //TODO lookup the token 
        ParameterBin* parameterBin = ParameterBin::get_ptr(token);
        memcpy(dataDestination, parameterBin->data(), dataByteLength);
    }

    void StreamConnection::invoke(sensekit_command_id commandId, 
                                  size_t inByteLength, 
                                  sensekit_parameter_data_t inData,
                                  size_t& resultByteLength,
                                  sensekit_result_token_t token)
    {
        sensekit_parameter_bin_t parameterBin;
        m_stream->invoke(this, commandId, inByteLength, inData, parameterBin);
    }
    
    StreamConnection::~StreamConnection()
    {
        set_bin(nullptr);
    }
}
