#ifndef STREAM_H
#define STREAM_H

namespace sensekit {

    struct stream_type_id
    {
        int type{0};
    };

    class Stream
    {
    public:
        Stream(stream_type_id typeId)
            : m_typeId(typeId) {}

        void initialize();
        void terminate();
        void open();
        void close();

    private:
        stream_type_id m_typeId;
    };
}

#endif /* STREAM_H */
