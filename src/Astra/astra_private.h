#ifndef ASTRA_PRIVATE_H
#define ASTRA_PRIVATE_H

using astra_frame_id_t = int32_t;

using astra_frame_status_t = int8_t;
const astra_frame_status_t ASTRA_FRAME_STATUS_AVAILABLE = 0;
const astra_frame_status_t ASTRA_FRAME_STATUS_LOCKED_POLL = 1;
const astra_frame_status_t ASTRA_FRAME_STATUS_LOCKED_EVENT = 2;

struct _astra_reader_frame
{
    astra_frame_id_t id;
    astra_frame_status_t status;
    astra_reader_t reader;
};

#endif // ASTRA_PRIVATE_H
