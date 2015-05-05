#include <iostream>
#include <functional>
#include "includes.h"

int main(int argc, char **argv) {
    psl::Chunk<std::function<int(int)>> i;
    psl::set(i, psl::toChunk([]() { return [](int j) { return j + 1; }; } ));
    std::cout << i()(2) << "\n";

    return 0;
}
