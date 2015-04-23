#include <iostream>
#include "outputs.h"



psl::Chunk<interval, interval, fsignal> applyFilterF;
psl::Chunk<psl::Signal, psl::Signal, psl::Chunk<interval, double, interval>, double> intervalMap;
psl::Chunk<double, double, double, double> envelope;
psl::Chunk<fsignal, double> leftRightFilter;
psl::Chunk<interval, interval> filterInterval;
psl::Chunk<psl::Chunk<psl::Signal, psl::Signal>, psl::Signal> thing;
psl::Chunk<std::tuple<psl::Chunk<psl::Signal>>> out;

int main(int argc, char **argv) {
    applyFilterF = [&](psl::Chunk<interval> input, psl::Chunk<fsignal> filter) {
        return [&]() { return std::make_tuple(interval(psl::ft, (input()).stop, (input()).start)(psl::multiply(psl::ft(input), filter))); }();
    };
    intervalMap = [&](psl::Chunk<psl::Signal> input, psl::Chunk<interval, double, interval> f, psl::Chunk<double> width) {
        return merge(map(chop(input, width), f), width);
    };
    envelope = [&](psl::Chunk<double> t, psl::Chunk<double> decay, psl::Chunk<double> attack) {
        return [&]() {
            if(psl::lessThan(t, attack)) {
                return psl::divide(t, attack);
            }
            else {
                return [&]() {
                    if(psl::lessThan(t, decay)) {
                        return psl::minus([&]() { return 1.0; }, psl::divide([&]() { return std::make_tuple(psl::minus(t, attack)); }, decay));
                    }
                    else {
                        return [&]() { return 0.0; }();
                    };
                }();
            };
        }();
    };
    leftRightFilter = [&](psl::Chunk<double> cutoff) {
        return fsignal([&](psl::Chunk<double> f) {
                return [&]() {
                if(psl::lessThan(f, cutoff)) {
                return [&]() { return std::make_tuple([&]() { return 0.0; }, [&]() { return 1.0; }); }();
                }
                else {
                return [&]() { return std::make_tuple([&]() { return 1.0; }, [&]() { return 0.0; }); }();
                };
                }();
                });
    };
    filterInterval = [&](psl::Chunk<interval> input) {
        return [&]() {
            psl::Chunk<double, double> cutoff;
            psl::Chunk<double> thing;

            cutoff = [&](psl::Chunk<double> t) {
                return psl::multiply(envelope(t, [&]() { return 10.0; }, [&]() { return 0.0; }), thing);
            };
            thing = [&]() {
                return [&]() { return 1000.0; }();
            };

            return applyFilterF(input, leftRightFilter(cutoff((input()).start)));
        }();
    };
    thing = [&](psl::Chunk<psl::Signal> s) {
        return [&](psl::Chunk<psl::Signal> i) {
            return s();
        };
    };
    out = [&]() {
        return [&]() {
            psl::Chunk<psl::Signal> p;
            psl::Chunk<psl::Signal> m;
            psl::Chunk<psl::Signal> s;

            p = [&]() {
                return thing(s)(s);
            };
            m = [&]() {
                return signal(sampleRate(s), psl::sin);
            };
            s = [&]() {
                return psl::intervalMap(signalFromWav(input), filterInterval, [&]() { return 1.0000001e-2; });
            };

            return [&]() { return std::make_tuple(psl::plus(m, p)); }();
        }();
    };

    bool B = true;
    while(out().get(0)().fillBuffer(B))
        B = !B;
    return 0;
}
