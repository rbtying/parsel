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
        Chunk(std::function<T()> f);
        T operator()();

        std::function<T()> f_;

    private:
        std::shared_ptr<T> cache_;

    };
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
T Chunk<T>::operator()()
{
    if(cache_ == nullptr)
        cache_ = std::make_shared<T>(f_());

    return *cache_;
}
