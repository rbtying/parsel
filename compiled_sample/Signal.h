// fun facts
// sample rate will be global to program
// will need WavSignal, DAWSignal, FunctionSignal

#include <vector>
#include <complex>

namespace psl
{
    typedef std::vector<std::tuple<std::complex<double>, std::complex<double>>> buffer_t;

    class Signal
    {
    pubic:
        virtual fillBuffer()
    private:
        double sampleRate_;
        buffer_t buffer_;
    };
}
