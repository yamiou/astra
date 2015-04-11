#include "StreamReader.h"
#include <algorithm>
#include <cassert>
#include <chrono>
#include <sensekit_capi.h>

namespace sensekit {
    using namespace std::placeholders;

    StreamReader::StreamReader(StreamSet& streamSet) :
        m_streamSet(streamSet),
        m_scFrameReadyCallback(nullptr)
    {
    }

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

    sensekit_frame_ref_t* StreamReader::get_subframe(sensekit_stream_desc_t& desc)
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

    StreamReader::blockresult StreamReader::block_until_frame_ready_or_timeout(int timeoutMillis)
    {
        if (m_isFrameReady)
        {
            return blockresult::BLOCKRESULT_FRAMEREADY;
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
                if (m_isFrameReady)
                {
                    return blockresult::BLOCKRESULT_FRAMEREADY;
                }

                end = std::chrono::steady_clock::now();
                std::chrono::duration<double> elapsed_seconds = end - start;
                milliseconds = std::chrono::duration_cast<std::chrono::milliseconds>(elapsed_seconds).count();
            } while (forever || milliseconds < timeoutMillis);
        }

        return m_isFrameReady ? blockresult::BLOCKRESULT_FRAMEREADY : blockresult::BLOCKRESULT_TIMEOUT;
    }

    sensekit_status_t StreamReader::lock(int timeoutMillis)
    {
        if (m_locked)
            return SENSEKIT_STATUS_SUCCESS;

        StreamReader::blockresult result = block_until_frame_ready_or_timeout(timeoutMillis);

        if (result == blockresult::BLOCKRESULT_TIMEOUT)
        {
            return SENSEKIT_STATUS_TIMEOUT;
        }

        for(auto pair : m_streamMap)
        {
            ReaderConnectionData* data = pair.second;
            data->connection->lock();
        }

        m_locked = true;
        return SENSEKIT_STATUS_SUCCESS;
    }

    void StreamReader::unlock()
    {
        if (!m_locked)
            return; // TODO: exception here?

        for(auto pair : m_streamMap)
        {
            ReaderConnectionData* data = pair.second;
            data->connection->unlock();
            data->isNewFrameReady = false;
            if (data->currentFrameIndex > m_lastFrameIndex)
            {
                m_lastFrameIndex = data->currentFrameIndex;
            }
        }

        m_isFrameReady = false;
        m_locked = false;
    }

    bool StreamReader::destroy_stream_connection(StreamConnection* connection)
    {
        assert(connection != nullptr);

        if (m_streamMap.erase(connection->get_description()) > 0)
        {
            m_streamSet.destroy_stream_connection(connection);
            return true;
        }

        return false;
    }

    void StreamReader::check_for_all_frames_ready()
    {
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
            m_isFrameReady = true;
            raise_frame_ready();
        }
    }

    void StreamReader::raise_frame_ready()
    {
        if (m_frameReadySignal.slot_count() == 0)
        {
            //no clients to serve, don't bother locking and unlocking
            return;
        }
        bool wasLocked = m_locked;
        if (!wasLocked)
        {
            lock(SENSEKIT_TIMEOUT_RETURN_IMMEDIATELY);
        }
        sensekit_reader_t reader = get_handle();
        sensekit_reader_frame_t frame = get_handle();
        m_frameReadySignal.raise(reader, frame);

        if (!wasLocked && m_locked)
        {
            unlock();
        }
    }
}
