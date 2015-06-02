#include "StreamReader.h"
#include <algorithm>
#include <cassert>
#include <chrono>
#include <SenseKit/sensekit_capi.h>

namespace sensekit {
    using namespace std::placeholders;

    StreamReader::StreamReader(StreamSet& streamSet) :
        m_streamSet(streamSet),
        m_scFrameReadyCallback(nullptr),
        m_logger("StreamReader")
    {}

    StreamReader::~StreamReader()
    {
        for (auto pair : m_streamMap)
        {
            ReaderConnectionData* data = pair.second;
            data->connection->unregister_frame_ready_callback(data->scFrameReadyCallbackId);
            m_streamSet.destroy_stream_connection(data->connection);
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

        connection = m_streamSet.create_stream_connection(desc);

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
        if (m_isFrameReadyForLock)
        {
            return block_result::FRAMEREADY;
        }

        long long milliseconds = 0;
        if (timeoutMillis != SENSEKIT_TIMEOUT_RETURN_IMMEDIATELY)
        {
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

    sensekit_status_t StreamReader::lock(int timeoutMillis)
    {
        if (m_locked)
            return SENSEKIT_STATUS_SUCCESS;

        StreamReader::block_result result = block_until_frame_ready_or_timeout(timeoutMillis);

        m_isFrameReadyForLock = false;

        if (result == block_result::TIMEOUT)
        {
            return SENSEKIT_STATUS_TIMEOUT;
        }

        lock_private();
        return SENSEKIT_STATUS_SUCCESS;
    }

    void StreamReader::lock_private()
    {
        m_logger.trace("%x lock_private", this);

        for (auto pair : m_streamMap)
        {
            ReaderConnectionData* data = pair.second;
            data->connection->lock();
        }

        m_locked = true;
    }

    void StreamReader::unlock()
    {
        m_logger.trace("%x unlock m_locked: %d", this, m_locked);

        if (!m_locked)
            return; // TODO: exception here?
        m_locked = false;

        for(auto pair : m_streamMap)
        {
            ReaderConnectionData* data = pair.second;
            data->isNewFrameReady = false;
            if (data->currentFrameIndex > m_lastFrameIndex)
            {
                m_lastFrameIndex = data->currentFrameIndex;
            }
        }
        for(auto pair : m_streamMap)
        {
            ReaderConnectionData* data = pair.second;
            data->connection->unlock();
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
        m_logger.trace("%x raise_frame_ready 1 m_locked: %d", this, m_locked);
        if (m_frameReadySignal.slot_count() == 0)
        {
            //no clients to serve, don't bother locking and unlocking
            return;
        }
        bool wasLocked = m_locked;
        if (!wasLocked)
        {
            lock_private();
        }
        sensekit_reader_t reader = get_handle();
        sensekit_reader_frame_t frame = get_handle();
        m_frameReadySignal.raise(reader, frame);

        m_logger.trace("%x raise_frame_ready 2 m_locked: %d", this, m_locked);
        if (!wasLocked && m_locked)
        {
            unlock();
        }
    }
}
