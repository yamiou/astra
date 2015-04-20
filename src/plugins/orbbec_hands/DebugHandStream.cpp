#include "DebugHandStream.h"

namespace sensekit { namespace plugins { namespace hands {

    void DebugHandStream::set_parameter(sensekit_streamconnection_t connection,
                                        sensekit_parameter_id id,
                                        size_t inByteLength,
                                        sensekit_parameter_data_t inData)
    {
    }

    void DebugHandStream::get_parameter(sensekit_streamconnection_t connection,
                                        sensekit_parameter_id id,
                                        sensekit_parameter_bin_t& parameterBin)
    {
    }

    void DebugHandStream::invoke(sensekit_streamconnection_t connection,
                                    sensekit_command_id commandId,
                                    size_t inByteLength,
                                    sensekit_parameter_data_t inData,
                                    sensekit_parameter_bin_t& parameterBin)
    {
    }

} } }