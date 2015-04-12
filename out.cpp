#include <iostream>
#include "outputs.h"



psl::Chunk<std::tuple<psl::Chunk<psl::Signal>, psl::Chunk<psl::Signal>>> out;

int main(int argc, char **argv) {
    out = [&]() {
        return [&]() {
            psl::Chunk<psl::Signal> s;
            psl::Chunk<std::vector<psl::Chunk<psl::Signal>>> ss;
            psl::Chunk<psl::Signal> dumbsumb;

            s = [&]() {
                return signalFromWav(input());
            };
            ss = [&]() {
                return {s, s, s};
            };
            dumbsumb = [&]() {
                return sum(ss());
            };

            return std::make_tuple(dumbsum, dumbsum);
        }();
    };

    bool B = true;
    while(out().get(0)().fillBuffer(B) && out().get(1)().fillBuffer(B))
        B = !B;
}
