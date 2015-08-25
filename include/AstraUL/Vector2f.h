#ifndef VECTOR2F_H
#define VECTOR2F_H

#include <math.h>
#include <AstraUL/skul_ctypes.h>

namespace astra
{
    struct Vector2f : public astra_vector2f_t
    {
        Vector2f()
        {
            this->x = 0.0f;
            this->y = 0.0f;
        }

        Vector2f(const astra_vector2f_t& v)
        {
            *this = v;
        }

        Vector2f(float x, float y)
        {
            this->x = x;
            this->y = y;
        }

        operator ::astra_vector2f_t*() { return this; }
        operator const ::astra_vector2f_t*() const { return this; }

        float length() const;
        float length_squared() const;
        float dot(const Vector2f& v) const;

        static Vector2f normalize(Vector2f v);

        static inline Vector2f zero();
        inline bool is_zero() const;

        friend bool operator==(const Vector2f& lhs, const Vector2f& rhs);
        friend Vector2f operator+(const Vector2f& lhs, const Vector2f& rhs);
        friend Vector2f operator-(const Vector2f& lhs, const Vector2f& rhs);
        friend Vector2f operator*(const Vector2f& lhs, const float& rhs);
        friend Vector2f operator*(const float& lhs, const Vector2f& rhs);
        friend Vector2f operator/(const Vector2f& lhs, const float& rhs);

        Vector2f operator-();
        Vector2f& operator+=(const Vector2f& rhs);
        Vector2f& operator-=(const Vector2f& rhs);
        Vector2f& operator*=(const float& rhs);
        Vector2f& operator/=(const float& rhs);
    };

    inline Vector2f Vector2f::normalize(Vector2f v)
    {
        double length = sqrtf(v.x*v.x + v.y*v.y);
        if (length < 1e-9)
        {
            return Vector2f(0, 0);
        }
        else
        {
            return Vector2f(v.x / length, v.y / length);
        }
    }

    inline float Vector2f::length() const
    {
        return sqrtf(x * x + y * y);
    }

    inline float Vector2f::length_squared() const
    {
        return x * x + y * y;
    }

    inline float Vector2f::dot(const Vector2f& v) const
    {
        return x * v.x + y * v.y;
    }

    inline Vector2f& Vector2f::operator+=(const Vector2f& rhs)
    {
        this->x = this->x + rhs.x;
        this->y = this->y + rhs.y;
        return *this;
    }

    inline Vector2f& Vector2f::operator-=(const Vector2f& rhs)
    {
        this->x = this->x - rhs.x;
        this->y = this->y - rhs.y;
        return *this;
    }

    inline Vector2f& Vector2f::operator*=(const float& rhs)
    {
        this->x = this->x * rhs;
        this->y = this->y * rhs;
        return *this;
    }

    inline Vector2f& Vector2f::operator/=(const float& rhs)
    {
        this->x = this->x / rhs;
        this->y = this->y / rhs;
        return *this;
    }

    inline Vector2f Vector2f::operator-()
    {
        return Vector2f(-this->x, -this->y);
    }

    inline bool operator==(const Vector2f& lhs, const Vector2f& rhs)
    {
        return lhs.x == rhs.x && lhs.y == rhs.y;
    }

    inline bool operator!=(const Vector2f& lhs, const Vector2f& rhs)
    {
        return !(lhs == rhs);
    }

    inline Vector2f operator+(const Vector2f& lhs, const Vector2f& rhs)
    {
        return Vector2f(lhs.x + rhs.x, lhs.y + rhs.y);
    }

    inline Vector2f operator-(const Vector2f& lhs, const Vector2f& rhs)
    {
        return Vector2f(lhs.x - rhs.x, lhs.y - rhs.y);
    }

    inline Vector2f operator*(const Vector2f& lhs, const float& rhs)
    {
        return Vector2f(lhs.x * rhs, lhs.y * rhs);
    }

    inline Vector2f operator*(const float& lhs, const Vector2f& rhs)
    {
        return rhs * lhs;
    }

    inline Vector2f operator/(const Vector2f& lhs, const float& rhs)
    {
        return Vector2f(lhs.x / rhs, lhs.y / rhs);
    }

    inline Vector2f Vector2f::zero()
    {
        Vector2f zero;
        return zero;
    }

    inline bool Vector2f::is_zero() const
    {
        return *this == zero();
    }

}

#endif // VECTOR2F_H
