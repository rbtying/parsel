#pragma once

#include "Signal.h"

#include <functional>

namespace psl
{
    fill_t toWavFile(Signal* signalP, std::string filepath, float seconds);

    fill_t fillFromFile(SndfileHandle& file);

    fill_t fillFromOperator(std::function<std::complex<double>(std::complex<double>, std::complex<double>)>, Signal&, Signal&);
}
