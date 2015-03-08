// fun facts
// sample rate will be global to program

#include <vector>
#include <tuple>
#include <complex>

namespace psl
{
    typedef std::vector<std::complex<double>> sample_t;
    typedef std::vector<sample_t> buffer_t;

    class Signal
    {
    public:
        Signal(std::string fileName);

        void fillBuffer();

        double sampleRate_;
        buffer_t buffer_;
    };
}
