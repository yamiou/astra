#ifndef SIGNAL_H
#define SIGNAL_H

#include <functional>

//TODO: make add/remove callbacks threadsafe with mutex

template<typename R, typename Signature> class CallbackList;

template<typename R, typename Signature>
class CallbackNode
{

public:

    typedef std::function<R (Signature)> callback_type;

    CallbackNode(const callback_type& cb)
        : m_prev(NULL), m_next(NULL), m_callback(cb), m_refCount(1)
        {}

    void inc_reference() { m_refCount++; }
    void dec_reference()
        {
            m_refCount--;
            if (m_refCount == 0)
            {
                delete this;
            }
        }

    void deactivate()
        {
            m_callback = nullptr;
        }

    void remove_self()
        {
            deactivate();
            if (m_next)
            {
                m_next->m_prev = m_prev;
            }
            if (m_prev)
            {
                m_prev->m_next = m_next;
            }
            dec_reference();
        }

private:

    CallbackNode* m_prev;
    CallbackNode* m_next;
    callback_type m_callback;
    unsigned int m_refCount;

    friend class CallbackList<R, Signature>;
};

template<typename R, typename Signature>
class CallbackList
{
public:

    typedef CallbackNode<R, Signature> node_type;
    typedef typename node_type::callback_type callback_type;

    CallbackList()
        : m_count(0), m_head(NULL) { };

    ~CallbackList()
    {
        if (!m_head)
            return;

        node_type* node = m_head;
        while(node->m_next != m_head)
        {
            node->m_next->remove_self();
        }
        m_head->dec_reference();
        m_head->dec_reference();
    }

    size_t add(const callback_type& cb)
        {
            node_type* node = new node_type(cb);

            if (!m_head)
            {
                m_head = node;
                m_head->inc_reference();
                // set up as ring list
                m_head->m_next = m_head;
                m_head->m_prev = m_head;
            }
            else
            {
                node->m_prev = m_head->m_prev;
                node->m_next = m_head;

                if (m_head->m_prev)
                {
                    m_head->m_prev->m_next = node;
                }

                m_head->m_prev = node;
            }

            m_count++;

            return size_t(node);
        }

    bool remove(size_t id)
        {
            if (!m_head)
                return false;

            if (size_t(m_head) == id)
            {
                m_head->deactivate();
                m_count--;
                return true;
            }

            for (node_type* node = m_head->m_next ? m_head->m_next : m_head;
                 node != m_head;
                 node = node->m_next)
            {
                if (id == size_t(node))
                {
                    node->remove_self();
                    m_count--;
                    return true;
                }
            }

            return false;
        }

    unsigned int debug_count()
        {
            if (!m_head)
                return 0;

            unsigned int count = 0;
            for (node_type* node = m_head->m_next ? m_head->m_next : m_head;
                 node != m_head;
                 node = node->m_next)
            {
                count++;
            }

            return count;
        }
    unsigned int& count()
        {
            return m_count;
        }

    void invoke(Signature sig)
        {
            if (!m_head)
                return;

            node_type* node = m_head;
            node->inc_reference();
            do
            {
                if (node->m_callback != NULL)
                {
                    node->m_callback(sig);
                }
                node_type* prev = node;
                node = node->m_next;
                node->inc_reference();
                prev->dec_reference();
            } while (node != m_head);
            node->dec_reference();
        }

private:

    node_type* m_head;
    unsigned int m_count;
};

template<typename... Args>
class Signal
{
    typedef CallbackList<void, Args...> callback_list_t;
    typedef typename callback_list_t::callback_type callback_type;

public:
    Signal()
        : m_callbackList() { }

    size_t operator+=(const callback_type& cb)
        {
            return m_callbackList.add(cb);
        }
    bool operator-=(size_t id)
        {
            return m_callbackList.remove(id);
        }

    void raise(Args... args)
        {
            m_callbackList.invoke(args...);
        }
private:
    callback_list_t m_callbackList;

};

template<>
class Signal<void>
{
    typedef CallbackList<void, bool> callback_list_t;
    typedef std::function<void ()> callback_type;

public:
    Signal()
        : m_callbackList() { }

    size_t operator+=(const callback_type& cb)
    {
        return m_callbackList.add([cb] (bool b) { cb(); });
    }

    bool operator-=(size_t id)
    {
        return m_callbackList.remove(id);
    }

    void raise()
    {
        m_callbackList.invoke(true);
    }
private:
    callback_list_t m_callbackList;

};

#endif /* SIGNAL_H */
