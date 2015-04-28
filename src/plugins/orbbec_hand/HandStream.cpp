#include "HandStream.h"
#include <SenseKitUL/streams/hand_parameters.h>

namespace sensekit { namespace plugins { namespace hand {

    void HandStream::set_parameter(sensekit_streamconnection_t connection,
                                        sensekit_parameter_id id,
                                        size_t inByteLength,
                                        sensekit_parameter_data_t inData)
    {
        switch (id)
        {
        case SENSEKIT_PARAMETER_HAND_INCLUDE_CANDIDATE_POINTS:
            set_include_candidates(inByteLength, inData);
            break;
        }
    }

    void HandStream::get_parameter(sensekit_streamconnection_t connection,
                                        sensekit_parameter_id id,
                                        sensekit_parameter_bin_t& parameterBin)
    {
        switch (id)
        {
        case SENSEKIT_PARAMETER_HAND_INCLUDE_CANDIDATE_POINTS:
            get_include_candidates(parameterBin);
            break;
        }
    }

    void HandStream::get_include_candidates(sensekit_parameter_bin_t& parameterBin)
    {
        size_t resultByteLength = sizeof(bool);

        sensekit_parameter_data_t parameterData;
        sensekit_status_t rc = get_pluginService().get_parameter_bin(resultByteLength,
                                                                     &parameterBin,
                                                                     &parameterData);
        if (rc == SENSEKIT_STATUS_SUCCESS)
        {
            memcpy(parameterData, &m_includeCandidatePoints, resultByteLength);
        }
    }

    void HandStream::set_include_candidates(size_t inByteLength, sensekit_parameter_data_t& inData)
    {
        if (inByteLength >= sizeof(bool))
        {
            bool newIncludeCandidatePoints;
            memcpy(&newIncludeCandidatePoints, inData, sizeof(bool));

            set_include_candidate_points(newIncludeCandidatePoints);
        }
    }
}}}