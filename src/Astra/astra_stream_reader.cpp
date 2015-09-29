#include "astra_stream_reader.hpp"
#include <algorithm>
#include <cassert>
#include <chrono>
#include <Astra/astra_capi.h>
#include "astra_streamset_connection.hpp"
#include "astra_streamset.hpp"
#include "astra_logger.hpp"

namespace astra {
    using namespace std::placeholders;

    stream_reader::stream_reader(streamset_connection& connection)
        : m_connection(connection),
          m_scFrameReadyCallback(nullptr)
    {
    }

    stream_reader::~stream_reader()
    {
        LOG_TRACE("astra.stream_reader", "destroying reader: %p", this);
        for (auto& pair : m_streamMap)
        {
            reader_connection_data* data = pair.second;
            data->connection->unregister_frame_ready_callback(data->scFrameReadyCallbackId);
            m_connection.get_streamSet()->destroy_stream_connection(data->connection);
            delete data;
        }

        m_streamMap.clear();
    }

    stream_connection* stream_reader::find_stream_of_type(astra_stream_desc_t& desc)
    {
        auto it = m_streamMap.find(desc);

        if (it != m_streamMap.end())
        {
            return it->second->connection;
        }

        return nullptr;
    }

    stream_connection::FrameReadyCallback stream_reader::get_sc_frame_ready_callback()
    {
        if (m_scFrameReadyCallback == nullptr)
        {
            m_scFrameReadyCallback = [this](stream_connection* sc, astra_frame_index_t frameIndex)
                { this->on_connection_frame_ready(sc, frameIndex); };
        }
        return m_scFrameReadyCallback;
    }

    stream_connection* stream_reader::get_stream(astra_stream_desc_t& desc)
    {
        stream_connection* connection = find_stream_of_type(desc);

        if (connection != nullptr)
            return connection;

        connection = m_connection.get_streamSet()->create_stream_connection(desc);

        assert(connection != nullptr);

        astra_callback_id_t cbId = connection->register_frame_ready_callback(get_sc_frame_ready_callback());

        reader_connection_data* data = new reader_connection_data;
        data->connection = connection;
        data->scFrameReadyCallbackId = cbId;
        data->isNewFrameReady = false;
        data->currentFrameIndex = -1;

        m_streamMap.insert(std::make_pair(desc, data));

        return connection;
    }

    astra_frame_t* stream_reader::get_subframe(astra_stream_desc_t& desc)
    {
        if (!m_locked)
            return nullptr;

        stream_connection* connection = find_stream_of_type(desc);

        if (connection == nullptr)
        {
            return nullptr;
        }

        return connection->lock();
    }

    astra_callback_id_t stream_reader::register_frame_ready_callback(astra_frame_ready_callback_t callback,
                                                                    void* clientTag)
    {
        auto thunk = [clientTag, callback](astra_reader_t reader, astra_reader_frame_t frame)
            { callback(clientTag, reader, frame); };
        return m_frameReadySignal += thunk;
    }

    void stream_reader::unregister_frame_ready_callback(astra_callback_id_t& callbackId)
    {
        m_frameReadySignal -= callbackId;
        callbackId = 0;
    }

    stream_reader::block_result stream_reader::block_until_frame_ready_or_timeout(int timeoutMillis)
    {
        LOG_TRACE("astra.stream_reader", "%p block_until_frame_ready_or_timeout", this);
        if (m_isFrameReadyForLock)
        {
            return block_result::FRAMEREADY;
        }

        if (timeoutMillis != ASTRA_TIMEOUT_RETURN_IMMEDIATELY)
        {
            long long milliseconds = 0;
            std::chrono::steady_clock::time_point start, end;
            start = std::chrono::steady_clock::now();
            bool forever = timeoutMillis == ASTRA_TIMEOUT_FOREVER;
            do
            {
                astra_temp_update();
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

    astra_status_t stream_reader::lock(int timeoutMillis, astra_reader_frame_t& readerFrame)
    {
        LOG_TRACE("astra.stream_reader", "%p lock", this);
        if (!m_locked)
        {
            stream_reader::block_result result = block_until_frame_ready_or_timeout(timeoutMillis);

            m_isFrameReadyForLock = false;

            if (result == block_result::TIMEOUT)
            {
                readerFrame = nullptr;
                return ASTRA_STATUS_TIMEOUT;
            }
        }

        readerFrame = lock_frame_for_poll();

        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t stream_reader::unlock(astra_reader_frame_t& readerFrame)
    {
        LOG_TRACE("astra.stream_reader", "%p unlock", this);
        if (readerFrame == nullptr)
        {
            LOG_WARN("astra.stream_reader", "%p unlock with null frame parameter", this);
            assert(readerFrame != nullptr);
            return ASTRA_STATUS_INVALID_PARAMETER;
        }

        if (readerFrame->status == ASTRA_FRAME_STATUS_AVAILABLE)
        {
            LOG_WARN("astra.stream_reader", "%p readerFrame was closed more than once", this);
            assert(readerFrame->status != ASTRA_FRAME_STATUS_AVAILABLE);
            return ASTRA_STATUS_INVALID_OPERATION;
        }

        if (readerFrame->status == ASTRA_FRAME_STATUS_LOCKED_EVENT)
        {
            LOG_WARN("astra.stream_reader", "%p readerFrame from FrameReady event was closed manually", this);
            assert(readerFrame->status != ASTRA_FRAME_STATUS_LOCKED_EVENT);
            return ASTRA_STATUS_INVALID_OPERATION;
        }

        return unlock_frame_and_check_connections(readerFrame);
    }

    astra_status_t stream_reader::unlock_frame_and_check_connections(astra_reader_frame_t& readerFrame)
    {
        LOG_TRACE("astra.stream_reader", "%p unlock_frame_and_check_connections", this);
        astra_status_t rc = return_locked_frame(readerFrame);
        if (rc != ASTRA_STATUS_SUCCESS)
        {
            return rc;
        }

        return unlock_connections_if_able();
    }

    astra_reader_frame_t stream_reader::lock_frame_for_event_callback()
    {
        LOG_TRACE("astra.stream_reader", "%p lock_frame_for_event_callback", this);
        ensure_connections_locked();

        astra_reader_frame_t frame = acquire_available_reader_frame();
        frame->status = ASTRA_FRAME_STATUS_LOCKED_EVENT;
        ++m_lockedFrameCount;
        return frame;
    }

    astra_reader_frame_t stream_reader::lock_frame_for_poll()
    {
        LOG_TRACE("astra.stream_reader", "%p lock_frame_for_poll", this);
        ensure_connections_locked();

        astra_reader_frame_t frame = acquire_available_reader_frame();
        frame->status = ASTRA_FRAME_STATUS_LOCKED_POLL;
        ++m_lockedFrameCount;
        return frame;
    }

    astra_reader_frame_t stream_reader::acquire_available_reader_frame()
    {
        LOG_TRACE("astra.stream_reader", "%p acquire_reader_frame", this);

        for (auto& frame : m_frameList)
        {
            if (frame->status == ASTRA_FRAME_STATUS_AVAILABLE)
            {
                return frame.get();
            }
        }

        //m_frameList empty or all frames locked already

        FramePtr newFrame = std::make_unique<_astra_reader_frame>();
        newFrame->id = m_frameList.size();
        newFrame->status = ASTRA_FRAME_STATUS_AVAILABLE;
        newFrame->reader = get_handle();

        astra_reader_frame_t framePtr = newFrame.get();
        m_frameList.push_back(std::move(newFrame));

        return framePtr;
    }

    astra_status_t stream_reader::return_locked_frame(astra_reader_frame_t& readerFrame)
    {
        LOG_TRACE("astra.stream_reader", "%p return_locked_frame", this);
        if (m_lockedFrameCount == 0)
        {
            LOG_WARN("astra.stream_reader", "%p return_locked_frame too many times (lockedFrameCount == 0)", this);
            assert(m_lockedFrameCount != 0);
            return ASTRA_STATUS_INVALID_OPERATION;
        }
        if (readerFrame == nullptr)
        {
            LOG_WARN("astra.stream_reader", "%p return_locked_frame with null readerFrame parameter", this);
            assert(readerFrame != nullptr);
            return ASTRA_STATUS_INVALID_PARAMETER;
        }
        if (readerFrame->reader != get_handle())
        {
            LOG_WARN("astra.stream_reader", "%p return_locked_frame readerFrame closed on wrong stream_reader", this);
            assert(readerFrame->reader == get_handle());
            return ASTRA_STATUS_INVALID_OPERATION;
        }
        if (readerFrame->id >= static_cast<int>(m_frameList.size()))
        {
            LOG_WARN("astra.stream_reader", "%p return_locked_frame readerFrame parameter with id greater than frameList size", this);
            assert(readerFrame->id < static_cast<int>(m_frameList.size()));
            return ASTRA_STATUS_INVALID_PARAMETER;
        }
        if (readerFrame->status == ASTRA_FRAME_STATUS_AVAILABLE)
        {
            LOG_WARN("astra.stream_reader", "%p return_locked_frame frame status is already available", this);
            assert(readerFrame->status != ASTRA_FRAME_STATUS_AVAILABLE);
            return ASTRA_STATUS_INVALID_PARAMETER;
        }

        astra_reader_frame_t checkFrame = m_frameList[readerFrame->id].get();
        if (readerFrame != checkFrame)
        {
            LOG_WARN("astra.stream_reader", "%p return_locked_frame readerFrame parameter does not match pointer in frameList", this);
            assert(readerFrame == checkFrame);
            return ASTRA_STATUS_INVALID_PARAMETER;
        }

        checkFrame->status = ASTRA_FRAME_STATUS_AVAILABLE;
        --m_lockedFrameCount;

        readerFrame = nullptr;
        return ASTRA_STATUS_SUCCESS;
    }

    void stream_reader::ensure_connections_locked()
    {
        LOG_TRACE("astra.stream_reader", "%p ensure_connections_locked m_locked: %d", this, m_locked);

        if (!m_locked)
        {
            //LOG_INFO("astra.stream_reader", "locked run start");
            for (auto pair : m_streamMap)
            {
                reader_connection_data* data = pair.second;
                //LOG_INFO("astra.stream_reader", "locking: %u", data->connection->get_stream()->get_description().type);
                if (data->connection->is_started())
                {
                    //LOG_INFO("astra.stream_reader", "locked: %u", data->connection->get_stream()->get_description().type);
                    data->connection->lock();
                }
            }
            //LOG_INFO("astra.stream_reader", "locked run end");
            m_locked = true;
        }
    }

    astra_status_t stream_reader::unlock_connections_if_able()
    {
        LOG_TRACE("astra.stream_reader", "%p unlock_connections_if_able m_lockedFrameCount: %d m_locked: %d",
               this, m_lockedFrameCount, m_locked);
        if (!m_locked)
        {
            LOG_WARN("astra.stream_reader", "%p unlock_connections_if_able called too many times (m_locked == false)", this);
            assert(m_locked);
            return ASTRA_STATUS_INVALID_OPERATION;
        }

        if (m_lockedFrameCount > 0)
        {
            //don't unlock connections when there are outstanding frames
            return ASTRA_STATUS_SUCCESS;
        }

        //all frames should be available at this point
        for (auto& frame : m_frameList)
        {
            if (frame->status != ASTRA_FRAME_STATUS_AVAILABLE)
            {
                LOG_WARN("astra.stream_reader", "%p unlock_connections_if_able called but not all frames have been returned", this);
                assert(frame->status == ASTRA_FRAME_STATUS_AVAILABLE);
            }
        }

        for(auto& pair : m_streamMap)
        {
            reader_connection_data* data = pair.second;
            data->isNewFrameReady = false;
            if (data->currentFrameIndex > m_lastFrameIndex)
            {
                m_lastFrameIndex = data->currentFrameIndex;
            }
        }

        m_locked = false;

        //Do the connection unlock separately because unlock()
        //could call connection_frame_ready(...) again and we want to be ready
        for(auto& pair : m_streamMap)
        {
            reader_connection_data* data = pair.second;
            if (data->connection->is_started())
            {
                data->connection->unlock();
            }
        }

        return ASTRA_STATUS_SUCCESS;
    }

    void stream_reader::on_connection_frame_ready(stream_connection* connection, astra_frame_index_t frameIndex)
    {
        LOG_TRACE("astra.stream_reader", "%p connection_frame_ready fi: %d lfi: %d", this, frameIndex, m_lastFrameIndex);
        if (frameIndex > m_lastFrameIndex)
        {
            auto& desc = connection->get_description();

            auto pair = m_streamMap.find(desc);

            if (pair != m_streamMap.end())
            {
                //TODO optimization/special case -- if m_streamMap.size() == 1, call raise_frame_ready() directly
                reader_connection_data* data = pair->second;
                data->isNewFrameReady = true;
                data->currentFrameIndex = frameIndex;
            }
            else
            {
                LOG_WARN("astra.stream_reader", "Unknown frame readied!");
            }

            check_for_all_frames_ready();
        }
    }

    void stream_reader::check_for_all_frames_ready()
    {
        LOG_TRACE("astra.stream_reader", "%p check_for_all_frames_ready", this);

        bool allReady = true;
        for (auto& pair : m_streamMap)
        {
            reader_connection_data* data = pair.second;
            if (!data->isNewFrameReady && data->connection->is_started())
            {
                //TODO the new frames may not be synced.
                //We need matching frame indices in the future
                allReady = false;
                break;
            }
        }

        if (allReady)
        {
            m_isFrameReadyForLock = true;
            raise_frame_ready();
        }
    }

    void stream_reader::raise_frame_ready()
    {
        LOG_TRACE("astra.stream_reader", "%p raise_frame_ready", this);
        if (m_frameReadySignal.slot_count() == 0)
        {
            //no clients to serve, don't bother locking and unlocking
            return;
        }

        astra_reader_t reader = get_handle();
        astra_reader_frame_t frame = lock_frame_for_event_callback();

        LOG_TRACE("astra.stream_reader", "%p raise_frame_ready raising frameReady signal", this);

        m_frameReadySignal.raise(reader, frame);

        if (frame->status == ASTRA_FRAME_STATUS_AVAILABLE)
        {
            LOG_WARN("astra.stream_reader", "%p Frame was closed manually during stream_reader FrameReady callback", this);
        }
        else
        {
            LOG_TRACE("astra.stream_reader", "%p raise_frame_ready unlocking frame");
            unlock_frame_and_check_connections(frame);
        }
    }
}
