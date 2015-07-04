#ifndef SENSEKIT_PRIVATE_H
#define SENSEKIT_PRIVATE_H

using sensekit_frame_id_t = int32_t;

using sensekit_frame_status_t = int8_t;
const sensekit_frame_status_t SENSEKIT_FRAME_STATUS_AVAILABLE = 0;
const sensekit_frame_status_t SENSEKIT_FRAME_STATUS_LOCKED_POLL = 1;
const sensekit_frame_status_t SENSEKIT_FRAME_STATUS_LOCKED_EVENT = 2;

struct _sensekit_reader_frame
{
    sensekit_frame_id_t id;
    sensekit_frame_status_t status;
    sensekit_reader_t reader;
};

#endif // SENSEKIT_PRIVATE_H
