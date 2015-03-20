#pragma once
// fun facts
// sample rate will be global to program

#include <vector>
#include <complex>
#include <functional>

namespace psl
{
    typedef std::vector<std::complex<double>> sample_t;
    typedef std::vector<sample_t> buffer_t;
    typedef std::function<void(buffer_t&)> fill_t;

    class Signal
    {
    public:
        Signal(std::string filename);
        Signal(fill_t fill);

        void fillBuffer();

        fill_t fill_;
        double sampleRate_;
        buffer_t buffer_;

    private:
        static fill_t fillFromFile(std::string filename);
    };
}
