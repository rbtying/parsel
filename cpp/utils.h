#pragma once

#include "Signal.h"
#include "Chunk.h"
#include "fillers.h"

#include <vector>
#include <string>

namespace psl
{
    template<class F, class... Args>
    auto apply(F f, Args... as) -> decltype(f(as...))
    {
        return f(as...);
    }

    template<class F, class... Args>
    auto apply(Chunk<F> c, Args... as) -> decltype(c()(as...))
    {
        return c()(as...);
    }

    template<class F>
    auto toChunk(F f) -> Chunk<decltype(f())>
    {
        return Chunk<decltype(f())>(f);
    }

    template<class T, class F>
    void set(Chunk<T>& c1, const Chunk<F>& c2)
    {
        c1.f_ = c2.f_;
    }

    template<class T>
    std::vector<T> toVector(const std::initializer_list<T>& v)
    {
        return std::vector<T>(v);
    }

    inline std::string toString(std::vector<Chunk<char>> cs)
    {
        int size = cs.size();
        std::string s(size, ' ');
        for(int i = 0; i < size; i++)
            s[i] = cs[i]();
        return s;
    }

    inline std::vector<Chunk<char>> fromString(std::string s)
    {
        int size = s.size();
        std::vector<Chunk<char>> cs(size);
        for(int i = 0; i < size; i++)
            cs[i] = toChunk([s, i]() { return s[i]; });

        return cs;
    }

    inline std::function<Signal()> makeWriter(Chunk<std::vector<Chunk<char>>> path,
            Chunk<Signal> signal, Chunk<std::vector<Chunk<char>>> time)
    {
        return [=]() mutable
        {
            return Signal(std::bind(toWavFile, signal, toString(path()), stoi(toString(time()))),
                    signal().sampleRate(), signal().channels());
        };
    }
}
