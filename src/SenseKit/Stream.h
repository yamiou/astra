#ifndef STREAM_H
#define STREAM_H

#include <atomic>
#include <memory>
#include <vector>
#include "Signal.h"
#include "StreamBin.h"
#include "StreamConnection.h"

namespace sensekit {

    class StreamBin;

    class Stream
    {
    public:
        Stream(sensekit_stream_desc_t description,
               stream_callbacks_t pluginCallbacks)
            : m_description(description),
              m_callbacks(pluginCallbacks)
            {}

        ~Stream();

        StreamConnection* create_connection();
        void destroy_connection(StreamConnection* connection);

        StreamType get_type() { return m_description.type; }
        StreamSubtype get_subtype() { return m_description.subType; }

        StreamBin* create_bin(size_t byteLength);
        void destroy_bin(StreamBin* bin);

        void set_parameter(StreamConnection* connection,
                           sensekit_parameter_id id,
                           size_t byteLength,
                           sensekit_parameter_data_t* data);

        void get_parameter_size(StreamConnection* connection,
                                sensekit_parameter_id id,
                                size_t& byteLength);

        void get_parameter_data(StreamConnection* connection,
                                sensekit_parameter_id id,
                                size_t byteLength,
                                sensekit_parameter_data_t* data);

        const sensekit_stream_desc_t& get_description() const { return m_description; }

    private:
        using ConnPtr = std::unique_ptr<StreamConnection>;
        using ConnectionList = std::vector<ConnPtr>;

        using BinPtr = std::unique_ptr<StreamBin>;
        using BinList = std::vector<BinPtr>;

        sensekit_stream_desc_t m_description;

        ConnectionList m_connections;
        BinList m_bins;
        stream_callbacks_t m_callbacks;
    };
}

#endif /* STREAM_H */
