#ifndef VECTOR2_H
#define VECTOR2_H

#include <math.h>
#include <SenseKitUL/cvector.h>

namespace sensekit
{
    template<typename TType>
    struct Vector2_;

    using Vector2i = Vector2_ < int >;
    using Vector2f = Vector2_ < float >;

    template<typename TType>
    struct Vector2_
    {
        TType x;
        TType y;

        Vector2_() :
            x(0), y(0)
        { }

        Vector2_(TType x, TType y) :
            x(x), y(y)
        { }

        float length() const;
        float lengthSquared() const;
        float dot(const Vector2_& v);

        friend inline bool operator==(const Vector2_& lhs, const Vector2_& rhs);
        
        Vector2_& operator+=(const Vector2_& rhs);
        Vector2_& operator-=(const Vector2_& rhs);
        Vector2_& operator*=(const float& rhs);
        Vector2_& operator/=(const float& rhs);

        Vector2_ operator-();

        friend Vector2_ operator+(const Vector2_& lhs, const Vector2_& rhs);
        friend Vector2_ operator-(const Vector2_& lhs, const Vector2_& rhs);

        friend Vector2_ operator*(const Vector2_& lhs, const float& rhs);
        friend Vector2_ operator*(const float& lhs, const Vector2_& rhs);
        friend Vector2_ operator/(const Vector2_& lhs, const float& rhs);
    };

    inline Vector2i cvectorToVector(const sensekit_vector2i_t& other)
    {
        return Vector2i(other.x, other.y);
    }

    inline sensekit_vector2i_t vectorToCvector(const Vector2i& other)
    {
        sensekit_vector2i_t v;
        v.x = other.x;
        v.y = other.y;
        return v;
    }

    template<typename TType>
    inline float Vector2_<TType>::length() const
    {
        return sqrtf(x * x + y * y);
    }

    template<typename TType>
    inline float Vector2_<TType>::lengthSquared() const
    {
        return x * x + y * y;
    }

    template<typename TType>
    inline float Vector2_<TType>::dot(const Vector2_& v)
    {
        return x * v.x + y * v.y;
    }

    template<typename TType>
    inline bool operator==(const Vector2_<TType>& lhs, const Vector2_<TType>& rhs)
    {
        return lhs.x == rhs.x && lhs.y == rhs.y;
    }

    template<typename TType>
    inline bool operator!=(const Vector2_<TType>& lhs, const Vector2_<TType>& rhs)
    {
        return !(lhs == rhs);
    }

    template<typename TType>
    inline Vector2_<TType>& Vector2_<TType>::operator+=(const Vector2_<TType>& rhs)
    {
        this->x = this->x + rhs.x;
        this->y = this->y + rhs.y;
        return *this;
    }

    template<typename TType>
    inline Vector2_<TType>& Vector2_<TType>::operator-=(const Vector2_<TType>& rhs)
    {
        this->x = this->x - rhs.x;
        this->y = this->y - rhs.y;
        return *this;
    }

    template<typename TType>
    inline Vector2_<TType>& Vector2_<TType>::operator*=(const float& rhs)
    {
        this->x = this->x * rhs;
        this->y = this->y * rhs;
        return *this;
    }

    template<typename TType>
    inline Vector2_<TType>& Vector2_<TType>::operator/=(const float& rhs)
    {
        this->x = this->x / rhs;
        this->y = this->y / rhs;
        return *this;
    }

    template<typename TType>
    inline Vector2_<TType> Vector2_<TType>::operator-()
    {
        return Vector2_(-this->x, -this->y);
    }

    template<typename TType>
    inline Vector2_<TType> operator+(const Vector2_<TType>& lhs, const Vector2_<TType>& rhs)
    {
        return Vector2_<TType>(lhs.x + rhs.x, lhs.y + rhs.y);
    }

    template<typename TType>
    inline Vector2_<TType> operator-(const Vector2_<TType>& lhs, const Vector2_<TType>& rhs)
    {
        return Vector2_<TType>(lhs.x - rhs.x, lhs.y - rhs.y);
    }

    template<typename TType>
    inline Vector2_<TType> operator*(const Vector2_<TType>& lhs, const float& rhs)
    {
        return Vector2_<TType>(lhs.x * rhs, lhs.y * rhs);
    }

    template<typename TType>
    inline Vector2_<TType> operator*(const float& lhs, const Vector2_<TType>& rhs)
    {
        return rhs * lhs;
    }

    template<typename TType>
    inline Vector2_<TType> operator/(const Vector2_<TType>& lhs, const float& rhs)
    {
        return Vector2_<TType>(lhs.x / rhs, lhs.y / rhs);
    }
}

#endif // VECTOR2_H