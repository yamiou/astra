#ifndef DEPTHTYPES_H
#define DEPTHTYPES_H

struct sensekit_depthframe_header_t {
    int frameIndex;
    int compressiontype;
    sensekit_frame_t* sk_frame;
};

struct _sensekit_depthframe {
    sensekit_depthframe_header_t header;
    short sampleValue;
};

typedef struct _sensekit_depthstream sensekit_depthstream_t;
typedef struct _sensekit_depthframe sensekit_depthframe_t;


#endif // DEPTHTYPES_H

