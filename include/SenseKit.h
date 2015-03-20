#ifndef SENSEKIT_H
#define SENSEKIT_H

#ifdef  __cplusplus
# define SENSEKIT_BEGIN_DECLS  extern "C" {
# define SENSEKIT_END_DECLS    }
# include <cstdint>
# include <cstddef>
#else
# define SENSEKIT_BEGIN_DECLS
# define SENSEKIT_END_DECLS
# include <stdint.h>
# include <stddef.h>
#endif

#ifndef SENSEKIT_API
# if defined (_MSC_VER) && ! defined (SENSEKIT_WIN32_STATIC_BUILD)
#  define SENSEKIT_API __declspec(dllimport)
# else
#  define SENSEKIT_API
# endif
#endif

namespace sensekit{
    struct buffer;
}

SENSEKIT_BEGIN_DECLS

const unsigned MAX_STRING_FIELD_LENGTH = 256;

typedef enum _sensekit_status {
    SENSEKIT_STATUS_SUCCESS = 0,
    SENSEKIT_STATUS_INVALID_PARAMETER = 1,
    SENSEKIT_STATUS_DEVICE_ERROR = 2,
    SENSEKIT_STATUS_TIMEOUT = 3
} sensekit_status_t;

typedef struct _sensekit_device_desc {
    char uri[MAX_STRING_FIELD_LENGTH];
    char vendor[MAX_STRING_FIELD_LENGTH];
    char name[MAX_STRING_FIELD_LENGTH];
    uint16_t usbVendorId;
    uint16_t usbProductId;
} sensekit_device_desc_t;

typedef struct _sensekit_frame_desc {
    unsigned bytesPerPixel;
    unsigned width;
    unsigned height;
    unsigned length;
    float hFOV;
    float vFOV;
} sensekit_frame_desc_t;

typedef enum _sensekit_pixel_format {
    SENSEKIT_PIXEL_FORMAT_UNKNOWN = 0,
    SENSEKIT_PIXEL_FORMAT_DEPTH_MM = 1,
    SENSEKIT_PIXEL_FORMAT_GRAY8 = 2,
    SENSEKIT_PIXEL_FORMAT_GRAY16 = 3,
    SENSEKIT_PIXEL_FORMAT_RGB888 = 4
} sensekit_pixel_format_t;

typedef struct _sensekit_stream_mode_desc {
    sensekit_pixel_format_t pixelFormat;
    sensekit_frame_desc_t frameDescription;
    float framesPerSecond;
} sensekit_stream_mode_desc_t;

typedef enum sensekit_streamtype {
    SENSEKIT_STREAM_UNKNOWN = 0,
    SENSEKIT_STREAM_DEPTH,
    SENSEKIT_STREAM_COLOR,
    SENSEKIT_STREAM_CUSTOM
} sensekit_streamtype;

typedef struct _sensekit_streamsource_desc {
    size_t modeCount;
    sensekit_stream_mode_desc_t* modes;
    sensekit_streamtype type;
} sensekit_streamsource_desc_t;

struct _sensekit_frame
{
    unsigned int byteLength;
    void* data;
};

typedef struct _sensekit_streamset sensekit_streamset_t;
typedef struct _sensekit_stream sensekit_stream_t;
typedef struct _sensekit_frame sensekit_frame_t;

SENSEKIT_API void sensekit_initialize();

SENSEKIT_API void sensekit_terminate();

SENSEKIT_API sensekit_status_t sensekit_open_streamset(
    const char* connection_string,
    /*out*/ sensekit_streamset_t** streamset);

SENSEKIT_API sensekit_status_t sensekit_close_streamset(
    sensekit_streamset_t** streamset);

SENSEKIT_API char * sensekit_get_status_string(sensekit_status_t status);

SENSEKIT_API sensekit_status_t sensekit_stream_open(
    sensekit_streamset_t* streamset,
    sensekit_stream_t** stream);

SENSEKIT_API sensekit_status_t sensekit_stream_close(
    sensekit_stream_t** stream);

SENSEKIT_API sensekit_status_t sensekit_stream_frame_open(
    sensekit_stream_t* stream,
    int timeout_milliseconds,
    sensekit_frame_t** frame); //0 = return immediately

SENSEKIT_API sensekit_status_t sensekit_stream_frame_close(
    sensekit_frame_t** frame); //frame set to null

SENSEKIT_API sensekit_status_t sensekit_temp_update();

SENSEKIT_END_DECLS

#endif // SENSEKIT_H
