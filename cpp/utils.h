#pragma once

#include "Signal.h"
#include "Chunk.h"
#include "fillers.h"

namespace psl
{
    Signal toSignal(double d);

    template<class F, class... Args>
    auto apply(F&& f, Args... as) -> decltype(f(as...))
    {
        return f(as...);
    }

    template<class T, class... Args>
    T apply(Chunk<std::function<T(Args...)>> c, Args... as)
    {
        c()(as...);
    }

    template<class F, class T>
    void set(Chunk<T> c, F&& f);
//    {
//        c = f;
//    }

    template<class F>
    auto toChunk(F&& f) -> Chunk<decltype(f())>
    {
        return Chunk<decltype(f())>(f);
    }

    auto multiply = [](auto x, auto y) { return x * y; };
    auto plus = [](auto x, auto y) { return x + y; };
}
