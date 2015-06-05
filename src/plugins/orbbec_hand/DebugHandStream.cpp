#include "DebugHandStream.h"
#include <SenseKitUL/streams/hand_parameters.h>

namespace sensekit { namespace plugins { namespace hand {

    void DebugHandStream::on_set_parameter(sensekit_streamconnection_t connection,
                                           sensekit_parameter_id id,
                                           size_t inByteLength,
                                           sensekit_parameter_data_t inData)
    {
        switch (id)
        {
        case SENSEKIT_PARAMETER_DEBUG_HAND_VIEW_TYPE:
            set_view_parameter(inByteLength, inData);
            break;
        case SENSEKIT_PARAMETER_DEBUG_HAND_USE_MOUSE_PROBE:
            set_use_mouse_probe(inByteLength, inData);
            break;
        case SENSEKIT_PARAMETER_DEBUG_HAND_MOUSE_NORM_POSITION:
            set_mouse_norm_position(inByteLength, inData);
            break;
        case SENSEKIT_PARAMETER_DEBUG_HAND_PAUSE_INPUT:
            set_pause_input(inByteLength, inData);
            break;
        }
    }

    void DebugHandStream::on_get_parameter(sensekit_streamconnection_t connection,
                                           sensekit_parameter_id id,
                                           sensekit_parameter_bin_t& parameterBin)
    {
        switch (id)
        {
        case SENSEKIT_PARAMETER_DEBUG_HAND_VIEW_TYPE:
            get_view_parameter(parameterBin);
            break;
        }
    }

    void DebugHandStream::on_invoke(sensekit_streamconnection_t connection,
                                    sensekit_command_id commandId,
                                    size_t inByteLength,
                                    sensekit_parameter_data_t inData,
                                    sensekit_parameter_bin_t& parameterBin)
    {
        switch (commandId)
        {
        case SENSEKIT_PARAMETER_DEBUG_HAND_SPAWN_POINT:
            invoke_spawn_point(inByteLength, inData, parameterBin);
        }
    }

    void DebugHandStream::get_view_parameter(sensekit_parameter_bin_t& parameterBin)
    {
        size_t resultByteLength = sizeof(DebugHandViewType);

        sensekit_parameter_data_t parameterData;
        sensekit_status_t rc = get_pluginService().get_parameter_bin(resultByteLength,
                                                                     &parameterBin,
                                                                     &parameterData);
        if (rc == SENSEKIT_STATUS_SUCCESS)
        {
            memcpy(parameterData, &m_viewType, resultByteLength);
        }
    }

    void DebugHandStream::set_view_parameter(size_t inByteLength, sensekit_parameter_data_t& inData)
    {
        if (inByteLength >= sizeof(DebugHandViewType))
        {
            DebugHandViewType newViewType;
            memcpy(&newViewType, inData, sizeof(DebugHandViewType));

            set_view_type(newViewType);
        }
    }

    void DebugHandStream::set_use_mouse_probe(size_t inByteLength, sensekit_parameter_data_t& inData)
    {
        if (inByteLength >= sizeof(bool))
        {
            bool newUseMouseProbe;
            memcpy(&newUseMouseProbe, inData, sizeof(bool));

            m_useMouseProbe = newUseMouseProbe;
        }
    }

    void DebugHandStream::set_mouse_norm_position(size_t inByteLength, sensekit_parameter_data_t& inData)
    {
        if (inByteLength >= sizeof(sensekit_vector2f_t))
        {
            sensekit_vector2f_t newMousePosition;
            memcpy(&newMousePosition, inData, sizeof(sensekit_vector2f_t));

            m_mouseNormPosition = Vector2f::from_cvector(newMousePosition);
        }
    }

    void DebugHandStream::set_pause_input(size_t inByteLength, sensekit_parameter_data_t& inData)
    {
        if (inByteLength >= sizeof(bool))
        {
            bool newPauseInput;
            memcpy(&newPauseInput, inData, sizeof(bool));

            m_pauseInput = newPauseInput;
        }
    }

    void DebugHandStream::invoke_spawn_point(size_t inByteLength,
                                             sensekit_parameter_data_t inData,
                                             sensekit_parameter_bin_t& parameterBin)
    {
        m_spawnPointRequested = true;
    }

}}}
