#include "StreamReader.h"
#include <algorithm>
#include <cassert>
#include <chrono>
#include <SenseKit/sensekit_capi.h>
#include "StreamSetConnection.h"

namespace sensekit {
    using namespace std::placeholders;

    StreamReader::StreamReader(StreamSetConnection& connection) :
        m_connection(connection),
        m_scFrameReadyCallback(nullptr),
        m_logger("StreamReader")
    {}

    StreamReader::~StreamReader()
    {
        m_logger.trace("destroying reader: %p", this);
        for (auto pair : m_streamMap)
        {
            ReaderConnectionData* data = pair.second;
            data->connection->unregister_frame_ready_callback(data->scFrameReadyCallbackId);
            m_connection.get_streamSet()->destroy_stream_connection(data->connection);
            delete data;
        }

        m_streamMap.clear();
    }

    StreamConnection* StreamReader::find_stream_of_type(sensekit_stream_desc_t& desc)
    {
        auto it = m_streamMap.find(desc);

        if (it != m_streamMap.end())
        {
            return it->second->connection;
        }

        return nullptr;
    }

    StreamConnection::FrameReadyCallback StreamReader::get_sc_frame_ready_callback()
    {
        if (m_scFrameReadyCallback == nullptr)
        {
            m_scFrameReadyCallback = [this](StreamConnection* sc, sensekit_frame_index_t frameIndex)
                { this->on_connection_frame_ready(sc, frameIndex); };
        }
        return m_scFrameReadyCallback;
    }

    StreamConnection* StreamReader::get_stream(sensekit_stream_desc_t& desc)
    {
        StreamConnection* connection = find_stream_of_type(desc);

        if (connection != nullptr)
            return connection;

        connection = m_connection.get_streamSet()->create_stream_connection(desc);

        assert(connection != nullptr);

        sensekit_callback_id_t cbId = connection->register_frame_ready_callback(get_sc_frame_ready_callback());

        ReaderConnectionData* data = new ReaderConnectionData;
        data->connection = connection;
        data->scFrameReadyCallbackId = cbId;
        data->isNewFrameReady = false;
        data->currentFrameIndex = -1;

        m_streamMap.insert(std::make_pair(desc, data));

        return connection;
    }

    sensekit_frame_t* StreamReader::get_subframe(sensekit_stream_desc_t& desc)
    {
        if (!m_locked)
            return nullptr;

        StreamConnection* connection = find_stream_of_type(desc);
        if (connection == nullptr)
            return nullptr;

        return connection->lock();
    }

    sensekit_callback_id_t StreamReader::register_frame_ready_callback(sensekit_frame_ready_callback_t callback, void* clientTag)
    {
        auto thunk = [clientTag, callback](sensekit_reader_t reader, sensekit_reader_frame_t frame)
            { callback(clientTag, reader, frame); };
        return m_frameReadySignal += thunk;
    }

    void StreamReader::unregister_frame_ready_callback(sensekit_callback_id_t& callbackId)
    {
        m_frameReadySignal -= callbackId;
        callbackId = 0;
    }

    StreamReader::block_result StreamReader::block_until_frame_ready_or_timeout(int timeoutMillis)
    {
        m_logger.trace("%x block_until_frame_ready_or_timeout", this);
        if (m_isFrameReadyForLock)
        {
            return block_result::FRAMEREADY;
        }

        if (timeoutMillis != SENSEKIT_TIMEOUT_RETURN_IMMEDIATELY)
        {
            long long milliseconds = 0;
            std::chrono::steady_clock::time_point start, end;
            start = std::chrono::steady_clock::now();
            bool forever = timeoutMillis == SENSEKIT_TIMEOUT_FOREVER;
            do
            {
                sensekit_temp_update();
                if (m_isFrameReadyForLock)
                {
                    return block_result::FRAMEREADY;
                }

                end = std::chrono::steady_clock::now();
                std::chrono::duration<double> elapsed_seconds = end - start;
                milliseconds = std::chrono::duration_cast<std::chrono::milliseconds>(elapsed_seconds).count();
            } while (forever || milliseconds < timeoutMillis);
        }

        return m_isFrameReadyForLock ? block_result::FRAMEREADY : block_result::TIMEOUT;
    }

    sensekit_status_t StreamReader::lock(int timeoutMillis, sensekit_reader_frame_t& readerFrame)
    {
        m_logger.trace("%x lock", this);
        if (!m_locked)
        {
            StreamReader::block_result result = block_until_frame_ready_or_timeout(timeoutMillis);

            m_isFrameReadyForLock = false;

            if (result == block_result::TIMEOUT)
            {
                readerFrame = nullptr;
                return SENSEKIT_STATUS_TIMEOUT;
            }
        }

        readerFrame = lock_frame_for_poll();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t StreamReader::unlock(sensekit_reader_frame_t& readerFrame)
    {
        m_logger.trace("%x unlock", this);
        if (readerFrame == nullptr)
        {
            m_logger.warn("%x unlock with null frame parameter", this);
            assert(readerFrame != nullptr);
            return SENSEKIT_STATUS_INVALID_PARAMETER;
        }

        if (readerFrame->status == SENSEKIT_FRAME_STATUS_AVAILABLE)
        {
            m_logger.warn("%x readerFrame was closed more than once", this);
            assert(readerFrame->status != SENSEKIT_FRAME_STATUS_AVAILABLE);
            return SENSEKIT_STATUS_INVALID_OPERATION;
        }

        if (readerFrame->status == SENSEKIT_FRAME_STATUS_LOCKED_EVENT)
        {
            m_logger.warn("%x readerFrame from FrameReady event was closed manually", this);
            assert(readerFrame->status != SENSEKIT_FRAME_STATUS_LOCKED_EVENT);
            return SENSEKIT_STATUS_INVALID_OPERATION;
        }

        return unlock_frame_and_check_connections(readerFrame);
    }

    sensekit_status_t StreamReader::unlock_frame_and_check_connections(sensekit_reader_frame_t& readerFrame)
    {
        m_logger.trace("%x unlock_frame_and_check_connections", this);
        sensekit_status_t rc = return_locked_frame(readerFrame);
        if (rc != SENSEKIT_STATUS_SUCCESS)
        {
            return rc;
        }

        return unlock_connections_if_able();
    }

    sensekit_reader_frame_t StreamReader::lock_frame_for_event_callback()
    {
        m_logger.trace("%x lock_frame_for_event_callback", this);
        ensure_connections_locked();

        sensekit_reader_frame_t frame = acquire_available_reader_frame();
        frame->status = SENSEKIT_FRAME_STATUS_LOCKED_EVENT;
        ++m_lockedFrameCount;
        return frame;
    }

    sensekit_reader_frame_t StreamReader::lock_frame_for_poll()
    {
        m_logger.trace("%x lock_frame_for_poll", this);
        ensure_connections_locked();

        sensekit_reader_frame_t frame = acquire_available_reader_frame();
        frame->status = SENSEKIT_FRAME_STATUS_LOCKED_POLL;
        ++m_lockedFrameCount;
        return frame;
    }

    sensekit_reader_frame_t StreamReader::acquire_available_reader_frame()
    {
        m_logger.trace("%x acquire_reader_frame", this);

        for (auto& frame : m_frameList)
        {
            if (frame->status == SENSEKIT_FRAME_STATUS_AVAILABLE)
            {
                return frame.get();
            }
        }

        //m_frameList empty or all frames locked already

        FramePtr newFrame = std::make_unique<_sensekit_reader_frame>();
        newFrame->id = m_frameList.size();
        newFrame->status = SENSEKIT_FRAME_STATUS_AVAILABLE;
        newFrame->reader = get_handle();

        sensekit_reader_frame_t framePtr = newFrame.get();
        m_frameList.push_back(std::move(newFrame));

        return framePtr;
    }

    sensekit_status_t StreamReader::return_locked_frame(sensekit_reader_frame_t& readerFrame)
    {
        m_logger.trace("%x return_locked_frame", this);
        if (m_lockedFrameCount == 0)
        {
            m_logger.warn("%x return_locked_frame too many times (lockedFrameCount == 0)", this);
            assert(m_lockedFrameCount != 0);
            return SENSEKIT_STATUS_INVALID_OPERATION;
        }
        if (readerFrame == nullptr)
        {
            m_logger.warn("%x return_locked_frame with null readerFrame parameter", this);
            assert(readerFrame != nullptr);
            return SENSEKIT_STATUS_INVALID_PARAMETER;
        }
        if (readerFrame->reader != get_handle())
        {
            m_logger.warn("%x return_locked_frame readerFrame closed on wrong StreamReader", this);
            assert(readerFrame->reader == get_handle());
            return SENSEKIT_STATUS_INVALID_OPERATION;
        }
        if (readerFrame->id >= m_frameList.size())
        {
            m_logger.warn("%x return_locked_frame readerFrame parameter with id greater than frameList size", this);
            assert(readerFrame->id < m_frameList.size());
            return SENSEKIT_STATUS_INVALID_PARAMETER;
        }
        if (readerFrame->status == SENSEKIT_FRAME_STATUS_AVAILABLE)
        {
            m_logger.warn("%x return_locked_frame frame status is already available", this);
            assert(readerFrame->status != SENSEKIT_FRAME_STATUS_AVAILABLE);
            return SENSEKIT_STATUS_INVALID_PARAMETER;
        }

        sensekit_reader_frame_t checkFrame = m_frameList[readerFrame->id].get();
        if (readerFrame != checkFrame)
        {
            m_logger.warn("%x return_locked_frame readerFrame parameter does not match pointer in frameList", this);
            assert(readerFrame == checkFrame);
            return SENSEKIT_STATUS_INVALID_PARAMETER;
        }

        checkFrame->status = SENSEKIT_FRAME_STATUS_AVAILABLE;
        --m_lockedFrameCount;

        readerFrame = nullptr;
        return SENSEKIT_STATUS_SUCCESS;
    }

    void StreamReader::ensure_connections_locked()
    {
        m_logger.trace("%x ensure_connections_locked m_locked: %d", this, m_locked);

        if (!m_locked)
        {
            for (auto pair : m_streamMap)
            {
                ReaderConnectionData* data = pair.second;
                data->connection->lock();
            }
            m_locked = true;
        }
    }

    sensekit_status_t StreamReader::unlock_connections_if_able()
    {
        m_logger.trace("%x unlock_connections_if_able m_lockedFrameCount: %d m_locked: %d",
                            this, m_lockedFrameCount, m_locked);
        if (!m_locked)
        {
            m_logger.warn("%x unlock_connections_if_able called too many times (m_locked == false)", this);
            assert(m_locked);
            return SENSEKIT_STATUS_INVALID_OPERATION;
        }

        if (m_lockedFrameCount > 0)
        {
            //don't unlock connections when there are outstanding frames
            return SENSEKIT_STATUS_SUCCESS;
        }

        //all frames should be available at this point
        for (auto& frame : m_frameList)
        {
            if (frame->status != SENSEKIT_FRAME_STATUS_AVAILABLE)
            {
                m_logger.warn("%x unlock_connections_if_able called but not all frames have been returned", this);
                assert(frame->status == SENSEKIT_FRAME_STATUS_AVAILABLE);
            }
        }

        for(auto pair : m_streamMap)
        {
            ReaderConnectionData* data = pair.second;
            data->isNewFrameReady = false;
            if (data->currentFrameIndex > m_lastFrameIndex)
            {
                m_lastFrameIndex = data->currentFrameIndex;
            }
        }

        m_locked = false;

        //Do the connection unlock separately because unlock()
        //could call connection_frame_ready(...) again and we want to be ready
        for(auto pair : m_streamMap)
        {
            ReaderConnectionData* data = pair.second;
            data->connection->unlock();
        }

        return SENSEKIT_STATUS_SUCCESS;
    }

    void StreamReader::on_connection_frame_ready(StreamConnection* connection, sensekit_frame_index_t frameIndex)
    {
        m_logger.trace("%x connection_frame_ready fi: %d lfi: %d", this, frameIndex, m_lastFrameIndex);
        if (frameIndex > m_lastFrameIndex)
        {
            auto& desc = connection->get_description();

            auto pair = m_streamMap.find(desc);

            if (pair != m_streamMap.end())
            {
                //TODO optimization/special case -- if m_streamMap.size() == 1, call raise_frame_ready() directly
                ReaderConnectionData* data = pair->second;
                data->isNewFrameReady = true;
                data->currentFrameIndex = frameIndex;
            }

            check_for_all_frames_ready();
        }
    }

    void StreamReader::check_for_all_frames_ready()
    {
        m_logger.trace("%x check_for_all_frames_ready", this);
        bool allReady = true;
        for (auto pair : m_streamMap)
        {
            ReaderConnectionData* data = pair.second;
            if (!data->isNewFrameReady)
            {
                //TODO the new frames may not be synced.
                //We need matching frame indices in the future
                allReady = false;
            }
        }

        if (allReady)
        {
            m_isFrameReadyForLock = true;
            raise_frame_ready();
        }
    }

    void StreamReader::raise_frame_ready()
    {
        m_logger.trace("%x raise_frame_ready", this);
        if (m_frameReadySignal.slot_count() == 0)
        {
            //no clients to serve, don't bother locking and unlocking
            return;
        }

        sensekit_reader_t reader = get_handle();
        sensekit_reader_frame_t frame = lock_frame_for_event_callback();

        m_logger.trace("%x raise_frame_ready raising frameReady signal", this);

        m_frameReadySignal.raise(reader, frame);

        if (frame->status == SENSEKIT_FRAME_STATUS_AVAILABLE)
        {
            m_logger.warn("%x Frame was closed manually during StreamReader FrameReady callback", this);
        }
        else
        {
            m_logger.trace("%x raise_frame_ready unlocking frame");
            unlock_frame_and_check_connections(frame);
        }
    }
}
