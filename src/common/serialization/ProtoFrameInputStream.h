#ifndef PROTOFRAMEINPUTSTREAM_H
#define PROTOFRAMEINPUTSTREAM_H

#include <common/serialization/FrameInputStream.h>

#include "gensrc/Frame.pb.h"
#include "gensrc/FrameDescription.pb.h"
#include "gensrc/StreamHeader.pb.h"

#include <google/protobuf/io/zero_copy_stream_impl.h>

#include <cstdint>
#include <memory>
#include <sys/stat.h>

using namespace google::protobuf::io;

namespace astra { namespace serialization {

    class ProtoFrameInputStream : public FrameInputStream
    {
    public:
        ProtoFrameInputStream(const char* path);

        virtual ~ProtoFrameInputStream();

        void close() override final;
        bool read_stream_header(StreamHeader*& streamHeader) override;
        bool read_frame(Frame*& frame) override;
        bool read_frame_description(FrameDescription*& frameDescription) override;
        bool seek(int offset) override;
        bool seek_to_first_frame() override;
        int64_t get_position() override;
        bool is_end_of_file() override;
        int get_frame_description_size() override;
        int get_stream_header_size() override;

    private:
        long get_file_size(int fd);
        int get_file_descriptor(FILE* file);

        FILE* m_file;
        int m_fileDescriptor{-1};

        int m_positionOffset{0};

        std::unique_ptr<ZeroCopyInputStream> m_inputStream;
        proto::Frame m_frameMessage;
        proto::FrameDescription m_frameDescriptionMessage;
        proto::StreamHeader m_streamHeaderMessage;

        Frame m_frame;
        FrameDescription m_frameDescription;
        StreamHeader m_streamHeader;
        long m_fileSize{-1};
    };

}}

#endif /* PROTOFRAMEINPUTSTREAM_H */