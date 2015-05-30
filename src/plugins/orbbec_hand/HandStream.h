#ifndef HANDSTREAM_H
#define HANDSTREAM_H

#include <SenseKit/Plugins/SingleBinStream.h>
#include <SenseKitUL/streams/hand_types.h>
#include <SenseKitUL/skul_ctypes.h>
#include <SenseKitUL/Plugins/stream_types.h>
#include <Shiny.h>

namespace sensekit { namespace plugins { namespace hand {

    class HandStream : public SingleBinStream<sensekit_handframe_wrapper_t,
                                              sensekit_handpoint_t>

    {
    public:
        HandStream(PluginServiceProxy& pluginService,
                   sensekit_streamset_t streamSet,
                   size_t maxHandCount)
            : SingleBinStream(pluginService,
                              streamSet,
                              StreamDescription(SENSEKIT_STREAM_HAND,
                                                DEFAULT_SUBTYPE),
                              sizeof(sensekit_handpoint_t) * maxHandCount)
        { }

        bool include_candidate_points() const { return m_includeCandidatePoints; }
        void set_include_candidate_points(bool includeCandidatePoints)
        {
            m_includeCandidatePoints = includeCandidatePoints;
        }
    protected:
        virtual void on_set_parameter(sensekit_streamconnection_t connection,
                                      sensekit_parameter_id id,
                                      size_t inByteLength,
                                      sensekit_parameter_data_t inData) override;

        virtual void on_get_parameter(sensekit_streamconnection_t connection,
                                      sensekit_parameter_id id,
                                      sensekit_parameter_bin_t& parameterBin) override;
    private:
        void get_include_candidates(sensekit_parameter_bin_t& parameterBin);
        void set_include_candidates(size_t inByteLength, sensekit_parameter_data_t& inData);

        virtual void on_connection_removed(sensekit_bin_t bin,
                                           sensekit_streamconnection_t connection) override
        {
            SingleBinStream::on_connection_removed(bin, connection);

            #ifdef __ANDROID__
                PROFILE_UPDATE();
                PROFILE_OUTPUT("/sdcard/profile_orbbec_hand.txt");
            #endif
        }

        bool m_includeCandidatePoints{ false };
    };

}}}

#endif /* HANDSTREAM_H */
