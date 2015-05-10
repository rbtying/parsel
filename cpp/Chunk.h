#pragma once

#include <functional>
#include <memory>
#include <iostream>
#include <cassert>

namespace psl
{
    template<class T>
    class Chunk
    {
    public:
        Chunk();
        Chunk(const Chunk& other);
        Chunk(std::function<T()> f);
        Chunk& operator=(const Chunk& other);
        T operator()();

        std::function<T()> f_;

    private:
        std::shared_ptr<T> cache_;

    };

    template<class T, class F>
    T operator+(Chunk<T> t1, Chunk<F> t2);

    template<class T, class F>
    T operator-(Chunk<T> t1, Chunk<F> t2);

    template<class T, class F>
    T operator*(Chunk<T> t1, Chunk<F> t2);

    template<class T, class F>
    T operator/(Chunk<T> t1, Chunk<F> t2);

    template<class T, class F>
    T operator>=(Chunk<T> t1, Chunk<F> t2);

    template<class T, class F>
    T operator<=(Chunk<T> t1, Chunk<F> t2);

    template<class T, class F>
    T operator>(Chunk<T> t1, Chunk<F> t2);

    template<class T, class F>
    T operator<(Chunk<T> t1, Chunk<F> t2);

    template<class T, class F>
    T operator==(Chunk<T> t1, Chunk<F> t2);

    template<class T, class F>
    T operator&&(Chunk<T> t1, Chunk<F> t2);

    template<class T, class F>
    T operator||(Chunk<T> t1, Chunk<F> t2);

    template<class T, class F>
    T operator!(Chunk<T> t1);
}

using namespace psl;

template<class T>
Chunk<T>::Chunk() :
    f_(),
    cache_(nullptr)
{ }

template<class T>
Chunk<T>::Chunk(std::function<T()> f) :
    f_(f),
    cache_(nullptr)
{ }

template<class T>
Chunk<T>::Chunk(const Chunk& other) :
    f_(other.f_),
    cache_(other.cache_)
{ }

template<class T>
Chunk<T>& Chunk<T>::operator=(const Chunk& other)
{
    f_ = other.f_;
    cache_ = other.cache_;
}

template<class T>
T Chunk<T>::operator()()
{
    if(cache_ == nullptr)
        cache_ = std::make_shared<T>(f_());

    return *cache_;
}

template<class T, class F>
T psl::operator+(Chunk<T> t1, Chunk<F> t2)
{
    return t1() + t2();
}

template<class T, class F>
T psl::operator-(Chunk<T> t1, Chunk<F> t2)
{
    return t1() - t2();
}

template<class T, class F>
T psl::operator*(Chunk<T> t1, Chunk<F> t2)
{
    return t1() * t2();
}

template<class T, class F>
T psl::operator/(Chunk<T> t1, Chunk<F> t2)
{
    return t1() / t2();
}

template<class T, class F>
T psl::operator>=(Chunk<T> t1, Chunk<F> t2)
{
    return t1() >= t2();
}

template<class T, class F>
T psl::operator<=(Chunk<T> t1, Chunk<F> t2)
{
    return t1() <= t2();
}

template<class T, class F>
T psl::operator>(Chunk<T> t1, Chunk<F> t2)
{
    return t1() > t2();
}

template<class T, class F>
T psl::operator<(Chunk<T> t1, Chunk<F> t2)
{
    return t1() < t2();
}

template<class T, class F>
T psl::operator==(Chunk<T> t1, Chunk<F> t2)
{
    return t1() == t2();
}

template<class T, class F>
T psl::operator&&(Chunk<T> t1, Chunk<F> t2)
{
    return t1() && t2();
}

template<class T, class F>
T psl::operator||(Chunk<T> t1, Chunk<F> t2)
{
    return t1() || t2();
}

template<class T, class F>
T psl::operator!(Chunk<T> t1)
{
    return !t1();
}
