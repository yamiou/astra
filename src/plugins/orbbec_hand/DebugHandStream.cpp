#include "DebugHandStream.h"
#include <AstraUL/streams/hand_parameters.h>

namespace astra { namespace plugins { namespace hand {

    void DebugHandStream::on_set_parameter(astra_streamconnection_t connection,
                                           astra_parameter_id id,
                                           size_t inByteLength,
                                           astra_parameter_data_t inData)
    {
        switch (id)
        {
        case ASTRA_PARAMETER_DEBUG_HAND_VIEW_TYPE:
            set_view_parameter(inByteLength, inData);
            break;
        case ASTRA_PARAMETER_DEBUG_HAND_USE_MOUSE_PROBE:
            set_use_mouse_probe(inByteLength, inData);
            break;
        case ASTRA_PARAMETER_DEBUG_HAND_MOUSE_NORM_POSITION:
            set_mouse_norm_position(inByteLength, inData);
            break;
        case ASTRA_PARAMETER_DEBUG_HAND_PAUSE_INPUT:
            set_pause_input(inByteLength, inData);
            break;
        case ASTRA_PARAMETER_DEBUG_HAND_LOCK_SPAWN_POINT:
            set_lock_spawn_point(inByteLength, inData);
            break;
        }
    }

    void DebugHandStream::on_get_parameter(astra_streamconnection_t connection,
                                           astra_parameter_id id,
                                           astra_parameter_bin_t& parameterBin)
    {
        switch (id)
        {
        case ASTRA_PARAMETER_DEBUG_HAND_VIEW_TYPE:
            get_view_parameter(parameterBin);
            break;
        }
    }

    void DebugHandStream::on_invoke(astra_streamconnection_t connection,
                                    astra_command_id commandId,
                                    size_t inByteLength,
                                    astra_parameter_data_t inData,
                                    astra_parameter_bin_t& parameterBin)
    {
    }

    void DebugHandStream::get_view_parameter(astra_parameter_bin_t& parameterBin)
    {
        size_t resultByteLength = sizeof(DebugHandViewType);

        astra_parameter_data_t parameterData;
        astra_status_t rc = pluginService().get_parameter_bin(resultByteLength,
                                                                     &parameterBin,
                                                                     &parameterData);
        if (rc == ASTRA_STATUS_SUCCESS)
        {
            memcpy(parameterData, &m_viewType, resultByteLength);
        }
    }

    void DebugHandStream::set_view_parameter(size_t inByteLength, astra_parameter_data_t& inData)
    {
        if (inByteLength >= sizeof(DebugHandViewType))
        {
            DebugHandViewType newViewType;
            memcpy(&newViewType, inData, sizeof(DebugHandViewType));

            set_view_type(newViewType);
        }
    }

    void DebugHandStream::set_use_mouse_probe(size_t inByteLength, astra_parameter_data_t& inData)
    {
        if (inByteLength >= sizeof(bool))
        {
            bool newUseMouseProbe;
            memcpy(&newUseMouseProbe, inData, sizeof(bool));

            m_useMouseProbe = newUseMouseProbe;
        }
    }

    void DebugHandStream::set_mouse_norm_position(size_t inByteLength, astra_parameter_data_t& inData)
    {
        if (inByteLength >= sizeof(astra_vector2f_t))
        {
            astra_vector2f_t newMousePosition;
            memcpy(&newMousePosition, inData, sizeof(astra_vector2f_t));

            m_mouseNormPosition = Vector2f::from_cvector(newMousePosition);
        }
    }

    void DebugHandStream::set_pause_input(size_t inByteLength, astra_parameter_data_t& inData)
    {
        if (inByteLength >= sizeof(bool))
        {
            bool newPauseInput;
            memcpy(&newPauseInput, inData, sizeof(bool));

            m_pauseInput = newPauseInput;
        }
    }

    void DebugHandStream::set_lock_spawn_point(size_t inByteLength, astra_parameter_data_t& inData)
    {
        if (inByteLength >= sizeof(bool))
        {
            bool newLockSpawnPoint;
            memcpy(&newLockSpawnPoint, inData, sizeof(bool));

            if (newLockSpawnPoint && !m_lockSpawnPoint)
            {
                m_spawnNormPosition = m_mouseNormPosition;
            }
            m_lockSpawnPoint = newLockSpawnPoint;
        }
    }
}}}
