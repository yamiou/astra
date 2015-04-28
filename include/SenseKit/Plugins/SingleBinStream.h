#ifndef SINGLEBINSTREAM_H
#define SINGLEBINSTREAM_H

#include "Stream.h"
#include "StreamBin.h"
#include <memory>

namespace sensekit { namespace plugins {

    template<typename TFrameType, typename TBlockType>
    class SingleBinStream : public Stream
    {
    public:
        SingleBinStream(PluginServiceProxy& pluginService,
                        Sensor streamSet,
                        StreamDescription description,
                        size_t bufferSize)
            : Stream(pluginService,
                     streamSet,
                     description)
        {
            m_bin = std::make_unique<bin_type>(get_pluginService(),
                                               get_handle(),
                                               sizeof(TFrameType) + bufferSize);
        }

        using frame_type = TFrameType;
        using block_type = TBlockType;

        bool has_connections()
        {
            return m_bin->has_connections();
        }

        TFrameType* begin_write(size_t frameIndex)
        {
            return m_bin->begin_write(frameIndex);
        }

        void end_write()
        {
            return m_bin->end_write();
        }

        virtual void on_connection_added(sensekit_streamconnection_t connection) override
        {
            m_bin->link_connection(connection);
        }

        virtual void on_connection_removed(sensekit_bin_t bin,
                                           sensekit_streamconnection_t connection) override
        {
            m_bin->unlink_connection(connection);
        }

    private:
        using bin_type = StreamBin<TFrameType>;
        std::unique_ptr<bin_type> m_bin;
    };
}}

#endif /* SINGLEBINSTREAM_H */
