#ifndef STREAMBACKEND_H
#define STREAMBACKEND_H

#include <sensekit_types.h>
#include "sensekit_internal.h"
#include "StreamConnection.h"
#include <vector>
#include <memory>

namespace sensekit {

    class StreamBin;

    class StreamBackend
    {
    public:
        StreamBackend(sensekit_stream_desc_t description,
                      stream_callbacks_t callbacks)
            : m_callbacks(callbacks),
              m_description(description) {}

        virtual ~StreamBackend()
            {
                m_bins.clear();
            }

        StreamBin* create_bin(size_t byteLength);
        void destroy_bin(StreamBin* bin);

        const sensekit_stream_desc_t& get_description() const { return m_description; }

    protected:
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

    private:
        using BinPtr = std::unique_ptr<StreamBin>;
        using BinList = std::vector<BinPtr>;

        sensekit_stream_desc_t m_description;
        stream_callbacks_t m_callbacks;

        BinList m_bins;
    };
}


#endif /* STREAMBACKEND_H */
