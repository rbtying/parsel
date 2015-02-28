// fun facts
// sample rate will be global to program
// will need WavSignal, DAWSignal, FunctionSignal

#include <vector>
#include <tuple>
#include <complex>

namespace psl
{
    typedef std::vector<std::tuple<std::complex<double>, std::complex<double>>> buffer_t;

    class Signal
    {
    public:
        virtual void fillBuffer();
        double sampleRate_;
        buffer_t buffer_;
    };
}
