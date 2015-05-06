#pragma once

#include "Signal.h"
#include "Chunk.h"

#include <functional>

namespace psl
{
    fill_t toWavFile(Chunk<Signal> signal, std::string filepath, float seconds);
    
    fill_t fillFromFile(SndfileHandle& file);
    fill_t fillFromFunction(op_tc, int sampleRate, int channels);
    
    fill_t fillFromOperator(op_ta, Signal* lhs, Signal* rhs);
    fill_t fillFromOperator(op_tb, Signal* lhs);

    fill_t fillFromPhaseShift(utime_t delay, Signal* s);
}
