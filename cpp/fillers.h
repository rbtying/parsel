#pragma once

#include "Signal.h"
#include "Chunk.h"

#include <functional>

namespace psl
{
    fill_t toWavFile(Chunk<Signal> signal, std::string filepath, float seconds);

    fill_t fillFromFile(SndfileHandle& file);

    fill_t fillFromFunction(Chunk<dubop_t> f, int sampleRate, int channels);

    fill_t fillFromOperator(binop_t f, Chunk<Signal> lhs, Chunk<Signal> rhs);

    fill_t fillFromOperator(unop_t f, Chunk<Signal> s);

    fill_t fillFromPhaseShift(utime_t delay, Chunk<Signal> s);
}
