#include "HandStream.h"
#include <AstraUL/streams/hand_parameters.h>

namespace astra { namespace plugins { namespace hand {

    void HandStream::on_set_parameter(astra_streamconnection_t connection,
                                      astra_parameter_id id,
                                      size_t inByteLength,
                                      astra_parameter_data_t inData)
    {
        switch (id)
        {
        case ASTRA_PARAMETER_HAND_INCLUDE_CANDIDATE_POINTS:
            set_include_candidates(inByteLength, inData);
            break;
        }
    }

    void HandStream::on_get_parameter(astra_streamconnection_t connection,
                                      astra_parameter_id id,
                                      astra_parameter_bin_t& parameterBin)
    {
        switch (id)
        {
        case ASTRA_PARAMETER_HAND_INCLUDE_CANDIDATE_POINTS:
            get_include_candidates(parameterBin);
            break;
        }
    }

    void HandStream::get_include_candidates(astra_parameter_bin_t& parameterBin)
    {
        size_t resultByteLength = sizeof(bool);

        astra_parameter_data_t parameterData;
        astra_status_t rc = get_pluginService().get_parameter_bin(resultByteLength,
                                                                     &parameterBin,
                                                                     &parameterData);
        if (rc == ASTRA_STATUS_SUCCESS)
        {
            memcpy(parameterData, &m_includeCandidatePoints, resultByteLength);
        }
    }

    void HandStream::set_include_candidates(size_t inByteLength, astra_parameter_data_t& inData)
    {
        if (inByteLength >= sizeof(bool))
        {
            bool newIncludeCandidatePoints;
            memcpy(&newIncludeCandidatePoints, inData, sizeof(bool));

            set_include_candidate_points(newIncludeCandidatePoints);
        }
    }
}}}