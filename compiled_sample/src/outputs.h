#pragma once

#include "Signal.h"

#include <functional>

namespace psl
{
    fill_t toWavFile(Signal* signalP, std::string filepath, float seconds);
    
    fill_t fillFromFile(SndfileHandle& file);

    fill_t fillFromOperator(op_t, Signal*, Signal*);
}
