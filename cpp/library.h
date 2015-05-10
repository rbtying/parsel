#pragma once

#include "utils.h"
#include "Signal.h"
#include "FSignal.h"
#include "Chunk.h"
#include "fillers.h"

#include <cmath>

namespace psl
{
    Signal ift(Chunk<FSignal> fsignal);

    FSignal ft(Chunk<Signal> signal);

    Signal loadSignal(Chunk<std::vector<Chunk<char>>> file);

    Signal signal(Chunk<dubop_t> f);

    dubop_t sin =
        [](auto t)
        {
            return std::sin(t());
        };

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
}

using namespace psl;

Signal psl::ift(Chunk<FSignal> fsignal)
{
    return Signal(std::bind(psl::fillFromFrequency, fsignal),
            fsignal().sampleRate(), fsignal().channels());
}

FSignal psl::ft(Chunk<Signal> signal)
{
    return FSignal(signal, 500e3);
}

Signal psl::loadSignal(Chunk<std::vector<Chunk<char>>> file)
{
    return Signal(toString(file()));
}

Signal psl::signal(Chunk<dubop_t> f)
{
    // TODO: get these in a better way!
    int sampleRate = 44100;
    int channels = 2;
    return Signal(std::bind(fillFromFunction, f, sampleRate, channels), sampleRate, channels);
}
