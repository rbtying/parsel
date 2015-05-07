#include "FSignal.h"

#include <cassert>
#include <memory>
#include "fft4g.c"

namespace psl {
    FSignal::FSignal(freq_fill_t freq_f) :
        freq_f_(freq_f), consistent_(false), bins_(0)
    {
        fillBuffer(true);
        long bufsize = timeSpace_.buffer().size();
        assert(bufsize && !(bufsize & (bufsize - 1)));
    }

    FSignal::FSignal(Signal& sig, utime_t timestep) :
        freq_f_(fillFromSignal(sig, timestep)),
        consistent_(false), bins_(0)
    {
        // buffer size must be power of 2
        fillBuffer(true);
        long bufsize = timeSpace_.buffer().size();
        assert(bufsize && !(bufsize & (bufsize - 1)));
    }

    void FSignal::computeTransform() {
        if (consistent_) {
            return;
        }

        const buffer_t buf = timeSpace_.buffer();
        long n = buf.size() * 2;
        long sqrt_n = static_cast<long>(std::sqrt(n) + 1);
        int ip[sqrt_n + 2];
        double a[n + 1], w[n * 5 / 4];
        ip[0] = 0; // ask cdft to initialize w

        freqSpace_.clear();
        freqSpace_.resize(n);

        for (int channel = 0; channel < timeSpace_.channels(); ++channel)
        {
            long idx = 0;
            for (buffer_t::const_iterator iter = buf.begin(); iter != buf.end(); ++iter)
            {
                const sample_t samp = *iter;
                a[2 * idx] = samp[channel].real();
                a[2 * idx + 1] = samp[channel].imag();
                ++idx;
            }

            cdft(n, 1, a, ip, w);

            // a now contains the frequency-domain interpretation
            for (int k = 0; k < n; ++k) {
                freqSpace_[k].push_back(std::complex<double>(a[2 * k], a[2 * k + 1]));
            }
        }
        bins_ = n;

        consistent_ = true;
    }

    bool FSignal::fillBuffer(bool B) {
        consistent_ = false;
        bool success = freq_f_(&timeSpace_, B);
        computeTransform();
        return success;
    }

    freq_fill_t FSignal::fillFromSignal(Signal& sig, utime_t timestep)
    {
        std::shared_ptr<Signal> sigptr(&sig);

        return [sigptr, timestep](Interval* itvl, bool B) {
            // start after end of last interval
            utime_t start_time = itvl->end();
            utime_t end_time = start_time + timestep;

            while (sigptr->buffer_->size() < end_time * sigptr->sampleRate()) {
                bool success = sigptr->fillBuffer(B);
                B = !B;
                if (!success) {
                    return false;
                }
            }

            *itvl = Interval(*sigptr, start_time, end_time);
            return true;
        };
    }
}
