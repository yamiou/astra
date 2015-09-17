#ifndef DEBUGHANDSTREAM_H
#define DEBUGHANDSTREAM_H

#include <Astra/Plugins/SingleBinStream.h>
#include <AstraUL/skul_ctypes.h>
#include <AstraUL/Plugins/stream_types.h>
#include <AstraUL/Vector.h>

namespace astra { namespace plugins { namespace hand {

    using DebugHandViewType = astra_debug_hand_view_type_t;

    class DebugHandStream : public SingleBinStream<astra_imageframe_wrapper_t>

    {
    public:
        DebugHandStream(PluginServiceProxy& pluginService,
                        astra_streamset_t streamSet,
                        uint32_t width,
                        uint32_t height,
                        uint32_t bytesPerPixel)
            : SingleBinStream(pluginService,
                              streamSet,
                              StreamDescription(ASTRA_STREAM_DEBUG_HAND,
                                                DEFAULT_SUBTYPE),
                              width * height * bytesPerPixel)
        { }

        DebugHandViewType view_type() const { return m_viewType; }
        void set_view_type(DebugHandViewType view) { m_viewType = view; }

        bool use_mouse_probe() const { return m_useMouseProbe; }
        const Vector2f& mouse_norm_position() const { return m_mouseNormPosition; }
        bool pause_input() const { return m_pauseInput; }
        bool spawn_point_locked() const { return m_lockSpawnPoint; }
        const Vector2f& spawn_norm_position() const { return m_spawnNormPosition; }

    protected:
        virtual void on_set_parameter(astra_streamconnection_t connection,
                                      astra_parameter_id id,
                                      size_t inByteLength,
                                      astra_parameter_data_t inData) override;

        virtual void on_get_parameter(astra_streamconnection_t connection,
                                      astra_parameter_id id,
                                      astra_parameter_bin_t& parameterBin) override;

        virtual void on_invoke(astra_streamconnection_t connection,
                               astra_command_id commandId,
                               size_t inByteLength,
                               astra_parameter_data_t inData,
                               astra_parameter_bin_t& parameterBin) override;
    private:
        void get_view_parameter(astra_parameter_bin_t& parameterBin);
        void set_view_parameter(size_t inByteLength,
                                astra_parameter_data_t& inData);
        void set_use_mouse_probe(size_t inByteLength, astra_parameter_data_t& inData);
        void set_mouse_norm_position(size_t inByteLength, astra_parameter_data_t& inData);
        void set_pause_input(size_t inByteLength, astra_parameter_data_t& inData);
        void set_lock_spawn_point(size_t inByteLength, astra_parameter_data_t& inData);

        DebugHandViewType m_viewType{ DEBUG_HAND_VIEW_DEPTH };
        bool m_useMouseProbe { false };
        Vector2f m_mouseNormPosition;
        Vector2f m_spawnNormPosition;
        bool m_pauseInput { false };
        bool m_lockSpawnPoint { false };
    };

}}}

#endif /* DEBUGHANDSTREAM_H */
