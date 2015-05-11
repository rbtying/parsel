#pragma once

#include "utils.h"
#include "Signal.h"
#include "FSignal.h"
#include "Chunk.h"
#include "fillers.h"

#include <cmath>

namespace psl
{

    auto ft = toChunk([]() { return
    std::function<FSignal(Chunk<Signal>)>(
        [](auto signal)
        {
            return FSignal(signal, 50 * 10e3);
        });
    });

    auto ift = toChunk([]() { return
    std::function<Signal(Chunk<FSignal>)>(
        [](auto fsignal)
        {
            return Signal(std::bind(psl::fillFromFrequency, fsignal),
                    fsignal().sampleRate(), fsignal().channels());
        });
    });

    auto loadSignal = toChunk([] () { return
    std::function<Signal(Chunk<std::vector<Chunk<char>>>)>(
        [](auto file)
        {
            return Signal(toString(file()));
        });
    });

    auto signal = toChunk([] () { return
    std::function<Signal(Chunk<dubop_t>)>(
        [](auto f)
        {
            // TODO: get these in a better way!
            int sampleRate = 44100;
            int channels = 2;
            return Signal(std::bind(fillFromFunction, f, sampleRate, channels),
                    sampleRate, channels);
        });
    });

    FSignal freqShift(Chunk<FSignal> fsig, Chunk<dubop_t> f) {
        freq_fill_t fs = [=](fbuffer_t* buf, double t, bool B) mutable -> bool {
            bool succ = fsig().fillBuffer(B);

            buf->clear();
            buf->resize(fsig().freqSpace_->size());

            Chunk<double> tc = psl::toChunk([=](){ return t; });
            double val = f()(tc).real();

            // f(t) in Hz, t in seconds
            // bin size = sampleRate / numBins
            int shift = val * fsig().freqSpace_->size() / fsig().sampleRate();

            if (shift > 0) {
                // shift right
                for (int i = 0; i < fsig().freqSpace_->size(); ++i) {
                    for (int channel = 0; channel < fsig().channels(); ++channel) {
                        if (i < shift) {
                            (*buf)[i].push_back(std::complex<double>(0,0));
                        } else {
                            (*buf)[i].push_back((*fsig().freqSpace_)[i - shift][channel]);
                        }
                    }
                }
            } else if (shift < 0) {
                // shift left
                int rshift = fsig().freqSpace_->size() + shift; // shift from the right side
                for (int i = fsig().freqSpace_->size() - 1; i >= 0; --i) {
                    for (int channel = 0; channel < fsig().channels(); ++channel) {
                        if (i > rshift) {
                            (*buf)[i].push_back(std::complex<double>(0,0));
                        } else {
                            (*buf)[i].push_back((*fsig().freqSpace_)[i - shift][channel]);
                        }
                    }
                }
            } else {
                buf->assign(fsig().freqSpace_->begin(), fsig().freqSpace_->end());
            }
            return succ;
        };
        return FSignal(fs, fsig().sampleRate(), fsig().channels());
    }

    auto sin = toChunk([] () { return
    dubop_t(
        [](auto t)
        {
            return std::sin(t());
        });
    });

    auto length = [](auto v) { return v().size(); };

    auto plus = [](auto x, auto y) { return x + y; };
    auto minus = [](auto x, auto y) { return x - y; };
    auto multiply = [](auto x, auto y) { return x * y; };
    auto divide = [](auto x, auto y) { return x / y; };
    auto lessThan = [](auto x, auto y) { return x > y; };
    auto greaterThan = [](auto x, auto y) { return x < y; };
    auto lessThanEq = [](auto x, auto y) { return x >= y; };
    auto greaterThanEq = [](auto x, auto y) { return x <= y; };
    auto eq = [](auto x, auto y) { return x == y; };
    auto and_ = [](auto x, auto y) { return x && y; };
    auto or_ = [](auto x, auto y) { return x || y; };
    auto negate = [](auto x) { return !x; };

    auto toComplex = [](auto x) { return std::complex<double>((double)x()); };
    auto fromComplex = [](auto x) { return std::real(x()); };

    // TODO: not lazy!
    auto map = [](auto &v, auto f)
    {
        int size = v().size();
        std::vector<Chunk<decltype(f()(v().at(std::declval<int>())))>> ret(size);

        for(int i = 0; i < size; i++)
            ret[i] = toChunk([v, f, i]() mutable { return f()(v().at(i)); });

        return ret;
    };

    auto fold = [](auto &v, auto f)
    {
        int size = v().size();

        // TODO: Handle the case when list is empty

        auto ret = v().at(0)();
        for (int i = 1; i < size; i++)
            ret = f()(toChunk([=] { return ret; }), v().at(i));

        return ret;
    };

    auto foldl = [](auto &initValue, auto &v, auto f)
    {
        int size = v().size();

        auto ret = initValue();
        for (int i = 0; i < size; i++)
            ret = f()(toChunk([=] { return ret; }), v().at(i));

        return ret;
    };

    auto at = [](auto &v, auto i)
    {
        return v[i];
    };
}

