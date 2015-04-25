#include <iostream>
#include "outputs.h"



psl::Chunk<std::tuple<psl::Chunk<psl::Signal>>> out;

int main(int argc, char **argv) {
    out = [&]() {
        return [&]() {
            psl::Chunk<std::function<int(psl::Chunk<int>)>> g;
            psl::Chunk<std::function<int(psl::Chunk<int>)>> f;

            g = [&]() { return [&](psl::Chunk<int> x) {
                return [&]() { return [&]() { return psl::apply(psl::multiply, [&]() { return 2.0; }, x); }; }();
            }; };
            f = [&]() { return [&](psl::Chunk<int> x) {
                return [&]() { return [&]() { return psl::apply(psl::plus, x, [&]() { return 1.0; }); }; }();
            }; };

            return [&]() { return psl::apply(f, [&]() { return psl::apply(g, [&]() { return 5.0; }); }); }();
        }();
    };

    bool B = true;
    while(out().get(0)().fillBuffer(B))
        B = !B;
    return 0;
}
