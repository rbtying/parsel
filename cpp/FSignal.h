#pragma once

#include <vector>
#include <complex>
#include <functional>

#include "Signal.h"
#include "Interval.h"

// declaration for complex dft
void cdft(int, int, double *, int *, double *);

namespace psl
{
    typedef std::function<bool(Interval*, bool)> freq_fill_t;
    typedef std::vector<std::complex<double>> fsample_t;
    typedef std::vector<fsample_t> fbuffer_t;

    class FSignal
    {
        public:
            FSignal(freq_fill_t freq_f);
            FSignal(Signal& sig, utime_t timestep);

            int bins() const { return bins_; }
            int sampleRate() const { return timeSpace_.sampleRate(); }
            int consistent() const { return consistent_; }

            bool fillBuffer(bool B);

            Interval timeSpace_;
            fbuffer_t freqSpace_;

        private:
            void computeTransform();

            freq_fill_t fillFromSignal(Signal& sig, utime_t timestep);

            freq_fill_t freq_f_;
            bool consistent_;
            long bins_;

            long NMAX_;
            long NMAXSQRT_;
    };
}
