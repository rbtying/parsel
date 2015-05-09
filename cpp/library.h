#pragma once

#include "utils.h"
#include "Signal.h"
#include "FSignal.h"
#include "Chunk.h"

#include <cmath>

namespace psl
{
    Signal ift(Chunk<FSignal> fsignal);

    FSignal ft(Chunk<Signal> signal);

    Signal loadSignal(Chunk<std::vector<Chunk<char>>> file);
    Signal signal(Chunk<dubop_t> f);

    dubop_t sin_ =
        [](auto t)
        {
            return sin(t());
        };
    Chunk<dubop_t> sin = toChunk([]() { return sin_; });

    auto plus = [](auto x, auto y) { return x + y; };
    auto minus = [](auto x, auto y) { return x - y; };
    auto multiply = [](auto x, auto y) { return x * y; };
    auto divide = [](auto x, auto y) { return x / y; };
}

using namespace psl;

Signal psl::ift(Chunk<FSignal> fsignal)
{
    return Signal(psl::fillFromFrequency(fsignal),
            fsignal().sampleRate(), fsignal().channels());
}

FSignal psl::ft(Chunk<Signal> signal)
{
    return FSignal(signal, 1024);
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
    return Signal(fillFromFunction(f, sampleRate, channels), sampleRate, channels);
}
