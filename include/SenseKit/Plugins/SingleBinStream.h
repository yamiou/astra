#ifndef SINGLEBINSTREAM_H
#define SINGLEBINSTREAM_H

#include "Stream.h"

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
                create_bin(sizeof(TFrameType) + bufferSize, m_binHandle, m_frame);
            }

        using frame_type = TFrameType;
        using block_type = TBlockType;

        bool has_connections()
            {
                if (m_binHandle == nullptr)
                    return false;

                return bin_has_connections(m_binHandle);
            }

        frame_type* begin_write(size_t frameIndex)
            {
                if (m_locked)
                    return reinterpret_cast<TFrameType*>(m_frame->data);

                m_locked = true;
                m_frame->frameIndex = frameIndex;
                return reinterpret_cast<TFrameType*>(m_frame->data);
            }

        void end_write()
            {
                if (!m_locked)
                    return;

                cycle_bin(m_binHandle, m_frame);
                m_locked = false;
            }

        virtual void on_connection_added(sensekit_streamconnection_t connection) override
            {
                link_connection_to_bin(connection, m_binHandle);
            }

        virtual void on_connection_removed(sensekit_bin_t bin,
                                            sensekit_streamconnection_t connection) override
            {
                link_connection_to_bin(connection, nullptr);
            }

    private:
        bool m_locked{false};
        sensekit_bin_t m_binHandle{nullptr};
        sensekit_frame_t* m_frame{nullptr};
    };
}}

#endif /* SINGLEBINSTREAM_H */
