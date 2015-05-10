#pragma once

#include "utils.h"
#include "Signal.h"
#include "FSignal.h"
#include "Chunk.h"
#include "fillers.h"

#include <cmath>

namespace psl
{

    auto ft = toChunk([]() { return
    std::function<FSignal(Chunk<Signal>)>(
        [](auto signal)
        {
            return FSignal(signal, 5824);
        });
    });

    auto ift = toChunk([]() { return
    std::function<Signal(Chunk<FSignal>)>(
        [](auto fsignal)
        {
            return Signal(psl::fillFromFrequency(fsignal),
                    fsignal().sampleRate(), fsignal().channels());
        });
    });

    auto loadSignal = toChunk([] () { return
    std::function<Signal(Chunk<std::vector<Chunk<char>>>)>(
        [](auto file)
        {
            return Signal(toString(file()));
        });
    });

    auto signal = toChunk([] () { return
    std::function<Signal(Chunk<dubop_t>)>(
        [](auto f)
        {
            // TODO: get these in a better way!
            int sampleRate = 44100;
            int channels = 2;
            return Signal(fillFromFunction(f, sampleRate, channels),
                    sampleRate, channels);
        });
    });

    auto sin = toChunk([] () { return
    dubop_t(
        [](auto t)
        {
            return std::sin(t());
        });
    });

    auto length = [](auto v) { return v().size(); };

    auto plus = [](auto x, auto y) { return x + y; };
    auto minus = [](auto x, auto y) { return x - y; };
    auto multiply = [](auto x, auto y) { return x * y; };
    auto divide = [](auto x, auto y) { return x / y; };
    auto lessThan = [](auto x, auto y) { return x < y; };
    auto greaterThan = [](auto x, auto y) { return x > y; };
    auto lessThanEq = [](auto x, auto y) { return x <= y; };
    auto greaterThanEq = [](auto x, auto y) { return x >= y; };
    auto eq = [](auto x, auto y) { return x == y; };
    auto and_ = [](auto x, auto y) { return x && y; };
    auto or_ = [](auto x, auto y) { return x || y; };
    auto negate = [](auto x) { return !x; };

    // TODO: not lazy!
    auto map = [](auto &v, auto f) 
    {
        int size = v().size();
        std::vector<Chunk<decltype(f()(v().at(std::declval<int>())))>> ret(size);

        for(int i = 0; i < size; i++)
            ret[i] = toChunk([v, f, i]() mutable { return f()(v().at(i)); });

        return ret;
    };

    auto fold = [](auto &v, auto f)
    {
        int size = v().size();

        auto ret = v().at(0)();
        for (int i = 1; i < size; i++)
            ret = f()(toChunk([=] { return ret; }), v().at(i));

        return ret;
    };

    auto foldl = [](auto &v, auto f, auto initValue)
    {
        int size = v().size();

        auto ret = initValue;
        for (int i = 1; i < size; i++)
            ret = f(ret, v().at(i));

        return ret;
    };
}

Signal psl::ift(Chunk<FSignal> fsignal)
{
    return Signal(std::bind(psl::fillFromFrequency, fsignal),
            fsignal().sampleRate(), fsignal().channels());
}

FSignal psl::ft(Chunk<Signal> signal)
{
    return FSignal(signal, 500e3);
}

Signal psl::signal(Chunk<dubop_t> f)
{
    // TODO: get these in a better way!
    int sampleRate = 44100;
    int channels = 2;
    return Signal(std::bind(fillFromFunction, f, sampleRate, channels), sampleRate, channels);
}
