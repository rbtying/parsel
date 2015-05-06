#pragma once

#include "Signal.h"
#include "Chunk.h"
#include "fillers.h"

#include <vector>
#include <string>
#include <algorithm>

namespace psl
{
    template<class F, class... Args>
    auto apply(const F&& f, Args... as) -> decltype(f(as...))
    {
        return f(as...);
    }

    template<class T, class... Args>
    T apply(Chunk<std::function<T(Args...)>>& c, Args... as)
    {
        return c()(as...);
    }

    template<class T, class F>
    void set(Chunk<T>& c1, const Chunk<F>& c2)
    {
        c1.f_ = c2.f_;
    }

    template<class F>
    auto toChunk(const F& f) -> Chunk<decltype(f())>
    {
        return Chunk<decltype(f())>(f);
    }

    auto multiply = [](auto x, auto y) { return x() * y(); };
    auto plus = [](auto x, auto y) { return x() + y(); };


    std::string toString(std::vector<Chunk<char>> cs)
    {
        int size = cs.size();
        std::string s(size, ' ');
        for(int i = 0; i < size; i++)
        {
            auto c = cs[i];
            s[i] = c();
        }
        return s;
    }

    std::vector<Chunk<char>> fromString(std::string s)
    {
        int size = s.size();
        std::vector<Chunk<char>> cs(size);
        for(int i = 0; i < size; i++)
        {
            cs[i] = toChunk([s, i]() { return s[i]; });
        }
        return cs;
    }

    Signal fromWav(Chunk<std::vector<Chunk<char>>> file)
    {
        auto f = file();
        auto string = toString(f);
        return Signal(string);
    }

    // TODO: fix seconds thing, make this lazy?
    std::function<Signal()> makeWriter(Chunk<std::vector<Chunk<char>>> path,
            Chunk<Signal> signal)
    {
        return [=]() mutable
        {
            return Signal(toWavFile(signal, toString(path()), 1),
                    signal().sampleRate(), signal().channels());
        };
    }

    template<class T>
    std::vector<T> toVector(const std::initializer_list<T>& v)
    {
        return std::vector<T>(v);
    }
}
