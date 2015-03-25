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

    class Stream
    {
    public:
        Stream(StreamType type, StreamSubtype subtype, StreamPluginCallbacks pluginCallbacks) :
            m_type(type),
            m_subtype(subtype),
            m_callbacks(pluginCallbacks)
        {
            m_nextBinId = 0;
        }

        ~Stream();

        StreamConnection* open();
        void close(StreamConnection* connection);

        StreamType get_type() { return m_type; }
        StreamSubtype get_subtype() { return m_subtype; }

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

        const StreamType m_type{ 0 };
        const StreamSubtype m_subtype{ -1 };

        ConnectionList m_connections;
        BinMap m_bins;
        StreamPluginCallbacks m_callbacks;
        
        std::atomic_int m_nextBinId;
    };
}

#endif /* STREAM_H */
