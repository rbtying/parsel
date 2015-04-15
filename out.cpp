#include <iostream>
#include "outputs.h"



psl::Chunk<std::tuple<psl::Chunk<psl::Signal>, psl::Chunk<psl::Signal>>> out;

int main(int argc, char **argv) {
    out = [&]() {
        return [&]() {
            psl::Chunk<psl::Signal> s;
            psl::Chunk<psl::Signal> ss;

            s = [&]() {
                return signalFromWav(input());
            };
            ss = [&]() {
                return [&]() {
                    if(psl::eq(data(), "hi")()) {
                        return s;
                    }
                    else {
                        return psl::multiply(2.0, psl::negate(s()));
                    }
                };
            };

            return std::make_tuple(ss, ss);
        }();
    };

    bool B = true;
    while(out().get(0)().fillBuffer(B) && out().get(1)().fillBuffer(B))
        B = !B;
}
