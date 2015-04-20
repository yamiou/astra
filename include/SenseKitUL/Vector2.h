#ifndef VECTOR2_H
#define VECTOR2_H

#include <math.h>
#include <SenseKitUL/skul_ctypes.h>

namespace sensekit
{
    template<typename TType>
    struct Vector2_;

    using Vector2i = Vector2_<int>;
    using Vector2f = Vector2_<float>;

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
        float length_squared() const;
        float dot(const Vector2_& v) const;

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

    inline Vector2i cvector_to_vector(const sensekit_vector2i_t& cvector)
    {
        return Vector2i(cvector.x, cvector.y);
    }

    inline sensekit_vector2i_t vector_to_cvector(const Vector2i& vector)
    {
        sensekit_vector2i_t cvector;
        cvector.x = vector.x;
        cvector.y = vector.y;
        return cvector;
    }

    template<typename TType>
    inline float Vector2_<TType>::length() const
    {
        return sqrtf(x * x + y * y);
    }

    template<typename TType>
    inline float Vector2_<TType>::length_squared() const
    {
        return x * x + y * y;
    }

    template<typename TType>
    inline float Vector2_<TType>::dot(const Vector2_& v) const
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