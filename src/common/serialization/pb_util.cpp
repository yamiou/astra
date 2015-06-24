#include "pb_util.h"

#include "google/protobuf/message_lite.h"
#include "google/protobuf/io/coded_stream.h"
#include "google/protobuf/io/zero_copy_stream_impl.h"
#include <stdint.h>

using namespace google::protobuf;
using namespace google::protobuf::io;

/*
This code is credited to Kenton Varda, author of ProtoBuf and Cap'n Proto, from the following Stackoverflow article:
http://stackoverflow.com/questions/22881876/protocol-buffers-how-to-serialize-and-deserialize-multiple-messages-into-a-file
*/


namespace sensekit { namespace serialization { namespace proto {
    
    bool write_delimited_to( const MessageLite& message, ZeroCopyOutputStream* rawOutput) 
    {
        // We create a new coded stream for each message.  Don't worry, this is fast.
        CodedOutputStream output(rawOutput);

        // Write the size.
        const int size = message.ByteSize();
        output.WriteLittleEndian32(size);

        uint8_t* buffer = output.GetDirectBufferForNBytesAndAdvance(size);
        if (buffer != NULL) {
            // Optimization:  The message fits in one buffer, so use the faster
            // direct-to-array serialization path.
            message.SerializeWithCachedSizesToArray(buffer);
        }
        else {
            // Slightly-slower path when the message is multiple buffers.
            message.SerializeWithCachedSizes(&output);
            if (output.HadError()) return false;
        }

        return true;
    }

    bool read_delimited_to(ZeroCopyInputStream* rawInput, MessageLite* message) 
    {
        // We create a new coded stream for each message.  Don't worry, this is fast,
        // and it makes sure the 64MB total size limit is imposed per-message rather
        // than on the whole stream.  (See the CodedInputStream interface for more
        // info on this limit.)
        CodedInputStream input(rawInput);

        // Read the size.
        uint32_t size;
        if (!input.ReadLittleEndian32(&size)) return false;

        // Tell the stream not to read beyond that size.
        CodedInputStream::Limit limit =
            input.PushLimit(size);

        // Parse the message.
        if (!message->MergeFromCodedStream(&input)) return false;
        if (!input.ConsumedEntireMessage()) return false;

        // Release the limit.
        input.PopLimit(limit);

        return true;
    }

}}}



