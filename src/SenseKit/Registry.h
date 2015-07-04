#ifndef REGISTRY_H
#define REGISTRY_H

#include <set>

namespace sensekit {

    template<typename>
    class TrackedInstance;

    class Registry
    {
    public:
        Registry();
        virtual ~Registry();

        template<typename T>
        static T* get(void* obj);

    private:
        struct tag_ {};

        using key = std::pair<const tag_*, const void*>;
        using store = std::set<key>;

        template<typename T>
        static void register_instance(TrackedInstance<T>* instance);

        template<typename T>
        static void unregister_instance(TrackedInstance<T>* instance);

        static store& get_store();

        template<typename T>
        friend class TrackedInstance;
    };

    template<typename T>
    void Registry::register_instance(TrackedInstance<T>* instance)
    {
        if (!instance)
            return;

        const tag_* tag = &TrackedInstance<T>::TAG;
        const void* object = instance;

        get_store().insert(std::make_pair(tag, object));
    }

    template<typename T>
    void Registry::unregister_instance(TrackedInstance<T>* instance)
    {
        if (!instance)
            return;

        const tag_* tag = &TrackedInstance<T>::TAG;
        const void* object = instance;

        get_store().erase(std::make_pair(tag, object));
    }

    template<typename T>
    T* Registry::get(void* obj)
    {
        if (!obj)
            return nullptr;

        const tag_* tag = &TrackedInstance<T>::TAG;
        if (get_store().count(std::make_pair(tag, obj)) == 0)
            return nullptr;

        return static_cast<T*>(reinterpret_cast<TrackedInstance<T>*>(obj));
    }

    template<typename T>
    class TrackedInstance
    {
    public:
        TrackedInstance();
        virtual ~TrackedInstance();

        // comment these out until we find a workaround for VS2013
        //TrackedInstance(TrackedInstance&&) = default;
        //TrackedInstance& operator=(TrackedInstance&&) = default;

        TrackedInstance(const TrackedInstance&) = default;
        TrackedInstance& operator=(const TrackedInstance&) = default;

    private:
        static const Registry::tag_ TAG;

        friend class Registry;

    };

    template<typename T>
    const Registry::tag_ TrackedInstance<T>::TAG = Registry::tag_();

    template<typename T>
    TrackedInstance<T>::TrackedInstance()
    {
        Registry::register_instance(this);
    }

    template<typename T>
    TrackedInstance<T>::~TrackedInstance()
    {
        Registry::unregister_instance(this);
    }
}

#endif /* REGISTRY_H */
