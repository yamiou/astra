#include <sensekit_core.h>
#include "generic_stream_api.h"
#include <streams/color_types.h>
#include "SenseKitUL_internal.h"
#include <math.h>
#include <StreamTypes.h>

SENSEKIT_BEGIN_DECLS

SENSEKIT_API_EX sensekit_status_t sensekit_color_stream_get(sensekit_reader_t reader,
                                                            sensekit_colorstream_t* colorStream)

{
    return sensekit_generic_stream_get(reader,
                                       SENSEKIT_STREAM_COLOR,
                                       ANY_SUBTYPE,
                                       colorStream);
}

SENSEKIT_API_EX sensekit_status_t sensekit_color_frame_get(sensekit_reader_frame_t readerFrame,
                                                           sensekit_colorframe_t** colorFrame)
{
    return sensekit_generic_frame_get<sensekit_colorframe_wrapper_t>(readerFrame,
                                                                     SENSEKIT_STREAM_COLOR,
                                                                     ANY_SUBTYPE,
                                                                     colorFrame);
}

SENSEKIT_API_EX sensekit_status_t sensekit_colorframe_get_frameindex(sensekit_colorframe_t* colorFrame,
                                                                      uint32_t* index)
{
    return sensekit_generic_frame_get_frameindex(colorFrame, index);
}

SENSEKIT_API_EX sensekit_status_t sensekit_colorframe_get_metadata(sensekit_colorframe_t* colorFrame,
                                                                    sensekit_colorframe_metadata_t* metadata ){
    *metadata = colorFrame->metadata;
    return SENSEKIT_STATUS_SUCCESS;
}

SENSEKIT_END_DECLS
