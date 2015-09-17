#ifndef PB_UTIL_H
#define PB_UTIL_H

#include "google/protobuf/message_lite.h"

using namespace google::protobuf;
using namespace google::protobuf::io;

namespace astra { namespace serialization { namespace proto {

    bool write_delimited_to(const MessageLite& message, ZeroCopyOutputStream* rawOutput);
    bool read_delimited_to(ZeroCopyInputStream* rawInput, MessageLite* message);

}}}

#endif