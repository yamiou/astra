#ifndef HND_DEBUG_HANDSTREAM_H
#define HND_DEBUG_HANDSTREAM_H

#include <Astra/Plugins/SingleBinStream.h>
#include <AstraUL/astraul_ctypes.h>
#include <AstraUL/Plugins/stream_types.h>
#include <AstraUL/Vector.h>

namespace astra { namespace hand {

    using debug_handview_type = astra_debug_hand_view_type_t;

    class debug_handstream : public plugins::SingleBinStream<astra_imageframe_wrapper_t>

    {
    public:
        debug_handstream(PluginServiceProxy& pluginService,
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

        debug_handview_type view_type() const { return viewType_; }
        void set_view_type(debug_handview_type view) { viewType_ = view; }

        bool use_mouse_probe() const { return useMouseProbe_; }
        const Vector2f& mouse_norm_position() const { return mouseNormPosition_; }
        bool pause_input() const { return pauseInput_; }
        bool spawn_point_locked() const { return lockSpawnPoint_; }
        const Vector2f& spawn_norm_position() const { return spawnNormPosition_; }

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

        debug_handview_type viewType_{ DEBUG_HAND_VIEW_DEPTH };
        bool useMouseProbe_{false};
        Vector2f mouseNormPosition_;
        Vector2f spawnNormPosition_;
        bool pauseInput_{false};
        bool lockSpawnPoint_{false};
    };
}}

#endif /* HND_DEBUG_HANDSTREAM_H */
