#ifndef STREAM_H
#define STREAM_H

#include "Signal.h"
#include "StreamBin.h"
#include <atomic>
#include <memory>
#include <vector>
#include <map>
#include "StreamConnection.h"

namespace sensekit {

    class StreamBin;

    using StreamId = unsigned;
    using StreamTypeId = unsigned;
    using SetParameterCallback = void(*)(void*,sensekit_streamconnection_t*, sensekit_parameter_id, size_t, sensekit_parameter_data_t*);
    using GetParameterSizeCallback = void(*)(void*, sensekit_streamconnection_t*, sensekit_parameter_id, /*out*/size_t*);
    using GetParameterDataCallback = void(*)(void*, sensekit_streamconnection_t*, sensekit_parameter_id, size_t, sensekit_parameter_data_t*);
    using ConnectionAddedCallback = void(*)(void*, sensekit_streamconnection_t*);
    using ConnectionRemovedCallback = void(*)(void*, sensekit_streamconnection_t*);

    struct StreamPluginCallbacks
    {
        void* context;
        SetParameterCallback setParameterCallback;
        GetParameterSizeCallback getParameterSizeCallback;
        GetParameterDataCallback getParameterDataCallback;
        ConnectionAddedCallback connectionAddedCallback;
        ConnectionRemovedCallback connectionRemovedCallback;

        StreamPluginCallbacks(void* context) :
            context(context),
            setParameterCallback(nullptr),
            getParameterSizeCallback(nullptr),
            getParameterDataCallback(nullptr),
            connectionAddedCallback(nullptr),
            connectionRemovedCallback(nullptr)
        {
        }
    };

    class Stream
    {
    public:
        Stream(StreamId id, StreamTypeId typeId, int index, StreamPluginCallbacks pluginCallbacks)
            : m_id(id),
            m_typeId(typeId),
            m_callbacks(pluginCallbacks)
        {
            m_nextBinId = 0;
        }

        ~Stream();

        StreamConnection* open();
        void close(StreamConnection* connection);

        StreamId get_id() { return m_id; }
        StreamTypeId get_typeId() { return m_typeId; }

        StreamBin* create_bin(size_t byteLength);
        void destroy_bin(StreamBin* bin);

        StreamBin* get_bin_by_id(StreamBinId id);

        void set_parameter(StreamConnection* connection, sensekit_parameter_id id, size_t byteLength, sensekit_parameter_data_t* data);
        void get_parameter_size(StreamConnection* connection, sensekit_parameter_id id, /*out*/size_t& byteLength);
        void get_parameter_data(StreamConnection* connection, sensekit_parameter_id id, size_t byteLength, sensekit_parameter_data_t* data);

    private:
        using ConnPtr = std::unique_ptr < StreamConnection > ;
        using ConnectionList = std::vector < ConnPtr > ;

        using BinPtr = std::unique_ptr < StreamBin > ;
        using BinMap = std::map < StreamBinId, BinPtr > ;

        const StreamId m_id{ 0 };
        const StreamTypeId m_typeId{ 0 };

        ConnectionList m_connections;
        BinMap m_bins;
        StreamPluginCallbacks m_callbacks;

        std::atomic_int m_nextBinId;
    };
}

#endif /* STREAM_H */
