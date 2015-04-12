#include <iostream>
#include "outputs.h"

struct thing;

struct thing {
    psl::Chunk<psl::Signal> b;
    psl::Chunk<int> a
};

psl::Chunk<psl::Signal, float, psl::Signal> s;
psl::Chunk<int> a;
psl::Chunk<std::tuple<psl::Chunk<psl::Signal>, psl::Chunk<psl::Signal>>> out;

int main(int argc, char **argv) {
    s = [&](float s, psl::Signal input) { return (binaryOp binOp expr1 expr2); };
    a = [&]() { return map(list es) also lambda tsyms t expr; };
    out = [&]() { return letexp ds expr; };

    bool B = true;
    while(out().get(0)().fillBuffer(B) && out().get(1)().fillBuffer(B)) B = !B;
}
