#pragma once

#include <functional>

namespace psl
{
    template<class T>
    class Chunk
    {
    public:
        Chunk();
        Chunk(std::function<T()> f);
        T& operator()();
        Chunk<T>& operator=(std::function<T()> f);
        operator T();
        operator std::function<T()>();

        std::function<T()> f_;

    };
}
