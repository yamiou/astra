#ifndef DEPTH_TYPES_H
#define DEPTH_TYPES_H


// TODO: references sensekit_frame_t*, includes need reorg to allow for the including of approp header
// its a miracle it works now.
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


#endif // DEPTH_TYPES_H
