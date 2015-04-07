#ifndef STREAMIMPL_H
#define STREAMIMPL_H

#include "StreamBin.h"
#include "StreamConnection.h"
#include <vector>
#include <memory>

namespace sensekit {

    class StreamImpl
    {
    public:
        StreamImpl(sensekit_stream_desc_t description,
                   stream_callbacks_t callbacks)
            : m_description(description),
              m_callbacks(callbacks)
            {}

        ~StreamImpl();

        StreamBin* create_bin(size_t byteLength);
        void destroy_bin(StreamBin* bin);

        void on_connection_created(StreamConnection* connection);
        void on_connection_destroyed(StreamConnection* connection);

        void on_set_parameter(StreamConnection* connection,
                           sensekit_parameter_id id,
                           size_t byteLength,
                           sensekit_parameter_data_t* data);

        void on_get_parameter_size(StreamConnection* connection,
                                sensekit_parameter_id id,
                                size_t& byteLength);

        void on_get_parameter_data(StreamConnection* connection,
                                sensekit_parameter_id id,
                                size_t byteLength,
                                sensekit_parameter_data_t* data);

        const sensekit_stream_desc_t& get_description() const { return m_description; }

    private:
        using BinPtr = std::unique_ptr<StreamBin>;
        using BinList = std::vector<BinPtr>;

        BinList m_bins;

        sensekit_stream_desc_t m_description;
        stream_callbacks_t m_callbacks;
    };
}


#endif /* STREAMIMPL_H */
